# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of PioneerTriptorelin
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# ------------------------------------------------------------------------------
# Private helpers
# ------------------------------------------------------------------------------

# Compute incidence rate + Poisson 95% CI for a single target-outcome pair.
# Returns a one-row data frame; suppresses IR/CI when n_events < minCellCount.
.calculateIR <- function(connection,
                         cohortDatabaseSchema,
                         cohortTable,
                         targetId,
                         outcomeId,
                         outcomeName,
                         isEpisodic,
                         minCellCount,
                         dbms) {

  sqlFile <- if (isEpisodic) "getIRDataEpisodic.sql" else "getIRDataChronic.sql"

  sql <- SqlRender::readSql(
    system.file("sql", "sql_server", sqlFile, package = "PioneerTriptorelin")
  )
  sql <- SqlRender::render(sql,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table           = cohortTable,
    target_id              = targetId,
    outcome_id             = outcomeId
  )
  sql <- SqlRender::translate(sql, targetDialect = dbms)

  row <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)

  nPersons    <- row$nPersons
  nEvents     <- row$nEvents
  personYears <- row$personYears

  # Suppress small cell counts
  if (is.na(nEvents) || nEvents < minCellCount) {
    return(data.frame(
      targetId               = targetId,
      outcomeId              = outcomeId,
      outcomeName            = outcomeName,
      type                   = if (isEpisodic) "episodic" else "chronic",
      nPersons               = nPersons,
      nEvents                = NA_integer_,
      personYears            = NA_real_,
      incidenceRatePer1000Py = NA_real_,
      ci95Lb                 = NA_real_,
      ci95Ub                 = NA_real_,
      stringsAsFactors       = FALSE
    ))
  }

  ci <- survival::cipoisson(nEvents) / personYears

  data.frame(
    targetId               = targetId,
    outcomeId              = outcomeId,
    outcomeName            = outcomeName,
    type                   = if (isEpisodic) "episodic" else "chronic",
    nPersons               = nPersons,
    nEvents                = nEvents,
    personYears            = round(personYears, 2),
    incidenceRatePer1000Py = round(nEvents / personYears * 1000, 2),
    ci95Lb                 = round(ci[1] * 1000, 2),
    ci95Ub                 = round(ci[2] * 1000, 2),
    stringsAsFactors       = FALSE
  )
}

#' Run safety analysis
#'
#' Estimates the 3-year cumulative incidence of safety outcomes (CV events,
#' VTE, diabetes, fractures) using Kaplan-Meier survival analysis, and the
#' incidence rate (per 1,000 person-years, with 95\% Poisson CI) for each
#' outcome. Episodic/chronic classification is read from
#' \code{inst/settings/IRsettings.csv}: chronic (new-onset) outcomes exclude
#' prevalent cases in both the KM and the incidence rate calculations.
#'
#' Individual-level time-to-event data is loaded into memory for computation
#' only and is never written to disk. All exported files contain aggregated
#' results only (KM survival table, 3-year point estimates, incidence rates).
#' Rows with fewer than \code{minCellCount} events are suppressed.
#'
#' @param connectionDetails       DatabaseConnector connection details object.
#' @param cdmDatabaseSchema       Schema containing the OMOP CDM tables.
#' @param cohortDatabaseSchema    Schema where cohort tables are written.
#' @param cohortTable             Name of the cohort table.
#' @param triptorelinCohortId     Cohort definition ID of the main triptorelin cohort.
#' @param outcomeCohortIds        Integer vector of outcome cohort IDs.
#'   \code{NA} entries are silently skipped. Outcome names are resolved from
#'   \code{inst/settings/CohortsToCreate.csv}.
#' @param databaseId              Short site identifier used to label outputs.
#' @param minCellCount            Minimum number of events required to report a
#'   KM time point. Rows below this threshold are suppressed.
#' @param outputFolder            Path where result files will be written.
#'
#' @export
runSafetyAnalysis <- function(connectionDetails,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema,
                               cohortTable,
                               triptorelinCohortId,
                               outcomeCohortIds,
                               databaseId,
                               minCellCount = 5,
                               outputFolder) {

  safetyFolder <- file.path(outputFolder, "safetyAnalysis")
  if (!dir.exists(safetyFolder)) {
    dir.create(safetyFolder, recursive = TRUE)
  }

  # Resolve cohort names from CohortsToCreate.csv (IDs are the single source of truth)
  cohortsToCreate <- readr::read_csv(
    system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
    col_types = readr::cols()
  )
  .cohortName <- function(id) {
    nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
    if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
  }

  # Episodic/chronic classification (chronic outcomes exclude prevalent cases)
  irSettings <- readr::read_csv(
    system.file("settings", "IRsettings.csv", package = "PioneerTriptorelin"),
    col_types = readr::cols()
  )
  .isChronic <- function(id) {
    row <- irSettings[irSettings$outcome_id == id, ]
    nrow(row) > 0 && row$chronic[1] == 1
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  dbms <- attr(connection, "dbms")

  # Containers for aggregated results across all outcomes
  allKmTables <- list()
  allKm3Year  <- list()
  allIr       <- list()

  for (i in seq_along(outcomeCohortIds)) {

    outcomeCohortId <- outcomeCohortIds[[i]]

    if (is.na(outcomeCohortId)) {
      ParallelLogger::logWarn("Skipping outcome at index ", i, ": cohort ID is NA.")
      next
    }

    outcomeName <- .cohortName(outcomeCohortId)

    ParallelLogger::logInfo("Safety analysis: ", outcomeName, " (cohort ", outcomeCohortId, ")")

    # --------------------------------------------------------------------------
    # 1. Extract time-to-event data  (individual level — memory only, not saved)
    #
    #    time_to_event : days from index to first outcome or censoring
    #    event         : 1 = outcome occurred, 0 = censored
    #
    #    Chronic (new-onset) outcomes use getTimeToEventChronic.sql, which
    #    excludes prevalent cases (outcome before cohort entry). Episodic
    #    outcomes use getTimeToEvent.sql, which keeps all subjects.
    #
    #    subject_id is used only for joining and is not retained in the result.
    # --------------------------------------------------------------------------
    isChronic  <- .isChronic(outcomeCohortId)
    sqlFile    <- if (isChronic) "getTimeToEventChronic.sql" else "getTimeToEvent.sql"
    ParallelLogger::logInfo(
      "  time-to-event (", if (isChronic) "chronic, prevalent excluded" else "episodic",
      "): ", sqlFile
    )

    sql <- SqlRender::loadRenderTranslateSql(
      sqlFilename            = sqlFile,
      packageName            = "PioneerTriptorelin",
      dbms                   = attr(connection, "dbms"),
      cohort_database_schema = cohortDatabaseSchema,
      cdm_database_schema    = cdmDatabaseSchema,
      cohort_table           = cohortTable,
      target_cohort_id       = triptorelinCohortId,
      outcome_cohort_id      = outcomeCohortId
    )

    tteData <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)

    if (nrow(tteData) == 0) {
      ParallelLogger::logWarn("No data returned for outcome: ", outcomeName, " — skipping.")
      next
    }

    # --------------------------------------------------------------------------
    # 2. Kaplan-Meier
    # --------------------------------------------------------------------------
    kmFit <- survival::survfit(
      survival::Surv(timeToEvent, event) ~ 1,
      data = tteData
    )

    kmTable <- data.frame(
      time     = kmFit$time,
      n_risk   = kmFit$n.risk,
      n_event  = kmFit$n.event,
      n_censor = kmFit$n.censor,
      surv     = kmFit$surv,
      std_err  = kmFit$std.err,
      ci_lower = kmFit$lower,
      ci_upper = kmFit$upper
    )

    kmTable$outcome    <- outcomeName
    kmTable$databaseId <- databaseId

    allKmTables[[outcomeName]] <- kmTable

    # 3-year cumulative incidence point estimate
    idx3yr <- which(kmTable$time <= 1095)
    if (length(idx3yr) > 0) {
      lastIdx <- max(idx3yr)
      km3yr <- data.frame(
        outcome                  = outcomeName,
        databaseId               = databaseId,
        surv_3yr                 = kmTable$surv[lastIdx],
        ci_lower_3yr             = kmTable$ci_lower[lastIdx],
        ci_upper_3yr             = kmTable$ci_upper[lastIdx],
        cumulative_incidence_3yr = 1 - kmTable$surv[lastIdx]
      )
    } else {
      km3yr <- data.frame(
        outcome                  = outcomeName,
        databaseId               = databaseId,
        surv_3yr                 = NA,
        ci_lower_3yr             = NA,
        ci_upper_3yr             = NA,
        cumulative_incidence_3yr = NA
      )
    }
    allKm3Year[[outcomeName]] <- km3yr

    # --------------------------------------------------------------------------
    # 3. Incidence rate (per 1,000 person-years, 95% Poisson CI)
    #    Episodic outcomes count all events; chronic outcomes count first-onset
    #    among the outcome-naive population (prevalent cases excluded).
    # --------------------------------------------------------------------------
    allIr[[outcomeName]] <- tryCatch(
      .calculateIR(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable          = cohortTable,
        targetId             = triptorelinCohortId,
        outcomeId            = outcomeCohortId,
        outcomeName          = outcomeName,
        isEpisodic           = !isChronic,
        minCellCount         = minCellCount,
        dbms                 = dbms
      ),
      error = function(e) {
        ParallelLogger::logError(
          "Failed to compute IR for outcome ", outcomeCohortId, ": ", conditionMessage(e)
        )
        NULL
      }
    )

    # Individual-level tteData goes out of scope here and is not saved
    rm(tteData)
  }

  # --------------------------------------------------------------------------
  # 4. Export aggregated results only
  # --------------------------------------------------------------------------
  if (length(allKmTables) > 0) {
    kmOut <- do.call(rbind, allKmTables)
    readr::write_excel_csv(kmOut, file.path(safetyFolder, "kmTable.csv"), na = "")
    ParallelLogger::logInfo("KM table written to kmTable.csv")
  }

  if (length(allKm3Year) > 0) {
    km3YearOut <- do.call(rbind, allKm3Year)
    readr::write_excel_csv(km3YearOut, file.path(safetyFolder, "km3YearEstimates.csv"), na = "")
    ParallelLogger::logInfo("3-year estimates written to km3YearEstimates.csv")
  }

  if (length(allIr) > 0) {
    irOut <- do.call(rbind, Filter(Negate(is.null), allIr))
    readr::write_excel_csv(irOut, file.path(safetyFolder, "incidenceRates.csv"), na = "")
    ParallelLogger::logInfo("Incidence rates written to incidenceRates.csv")
  }

  ParallelLogger::logInfo("Safety analysis complete. Results written to ", safetyFolder)
  invisible(NULL)
}
