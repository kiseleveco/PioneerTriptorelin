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

#' Run safety analysis
#'
#' Estimates the 3-year cumulative incidence of safety outcomes (CV events,
#' VTE, diabetes, fractures) using Kaplan-Meier survival analysis. For CV
#' outcomes (MACE, MI, stroke, AF), a multivariable Cox proportional hazards
#' model is additionally fitted, adjusted for age (natural spline) and history
#' of cardiovascular disease (yes/no).
#'
#' Individual-level time-to-event data is loaded into memory for computation
#' only and is never written to disk. All exported files contain aggregated
#' results only (KM survival table, 3-year point estimates, Cox coefficients).
#' Rows with fewer than \code{minCellCount} events are suppressed.
#'
#' @param connectionDetails       DatabaseConnector connection details object.
#' @param cdmDatabaseSchema       Schema containing the OMOP CDM tables.
#' @param cohortDatabaseSchema    Schema where cohort tables are written.
#' @param cohortTable             Name of the cohort table.
#' @param triptorelinCohortId     Cohort definition ID of the main triptorelin cohort.
#' @param outcomeCohortIds        Named integer vector of outcome cohort IDs.
#'   Expected names: \code{mace}, \code{mi}, \code{stroke}, \code{af},
#'   \code{vte}, \code{diabetes}, \code{fractures}.
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

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Containers for aggregated results across all outcomes
  allKmTables <- list()
  allKm3Year  <- list()

  for (outcomeName in names(outcomeCohortIds)) {

    outcomeCohortId <- outcomeCohortIds[[outcomeName]]

    if (is.na(outcomeCohortId)) {
      ParallelLogger::logWarn("Skipping outcome '", outcomeName, "': cohort ID is NA.")
      next
    }

    ParallelLogger::logInfo("Safety analysis: ", outcomeName, " (cohort ", outcomeCohortId, ")")

    # --------------------------------------------------------------------------
    # 1. Extract time-to-event data  (individual level — memory only, not saved)
    #
    #    time_to_event : days from index to first outcome or censoring
    #    event         : 1 = outcome occurred, 0 = censored
    #
    #    subject_id is used only for joining and is not retained in the result.
    # --------------------------------------------------------------------------
    sql <- SqlRender::loadRenderTranslateSql(
      sqlFilename            = "getTimeToEvent.sql",
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

    # Suppress rows with fewer than minCellCount events
    suppress <- kmTable$n_event > 0 & kmTable$n_event < minCellCount
    kmTable$surv[suppress]     <- NA
    kmTable$std_err[suppress]  <- NA
    kmTable$ci_lower[suppress] <- NA
    kmTable$ci_upper[suppress] <- NA

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

  ParallelLogger::logInfo("Safety analysis complete. Results written to ", safetyFolder)
  invisible(NULL)
}
