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

  ci <- survival::cipoisson(k = nEvents, pt = personYears)

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

# ------------------------------------------------------------------------------
# Main function
# ------------------------------------------------------------------------------

#' Describe real-world utilisation of triptorelin in mHSPC
#'
#' Runs four sub-analyses:
#' \enumerate{
#'   \item \strong{Baseline demographics} — temporal cohort characterisation of
#'     the target cohort via \code{CohortDiagnostics::executeDiagnostics()}.
#'   \item \strong{Formulation / dosing interval} — index event breakdown showing
#'     the distribution of 1-, 3-, and 6-month triptorelin depot formulations.
#'   \item \strong{Combination regimen distribution} — subject counts and
#'     percentages for each combination treatment cohort (e.g. ADT alone,
#'     ADT + ARPI, ADT + chemotherapy, ADT + ARPI + chemotherapy).
#'   \item \strong{Adverse event incidence rates} — incidence rates (per 1,000
#'     person-years) with 95\% Poisson confidence intervals for each AE outcome
#'     cohort, following the approach of the PioneerMetastaticAE package.
#'     Episodic/chronic classification is read from
#'     \code{inst/settings/IRsettings.csv}.
#' }
#'
#' Sub-analyses 1 and 2 are produced by a single \code{executeDiagnostics()}
#' call and exported as a zip archive viewable with
#' \code{CohortDiagnostics::launchDiagnosticsExplorer()}.
#'
#' @param connectionDetails           DatabaseConnector connection details object.
#' @param cdmDatabaseSchema           Schema containing the OMOP CDM tables
#'   (read-only).
#' @param vocabularyDatabaseSchema    Schema containing the vocabulary tables
#'   (usually the same as \code{cdmDatabaseSchema}).
#' @param cohortDatabaseSchema        Schema where cohort tables are written
#'   (needs read/write/delete).
#' @param cohortTable                 Name of the cohort table.
#' @param targetCohortId              Integer. Cohort definition ID of the main
#'   triptorelin cohort (used for sub-analyses 1, 2, and 4).
#' @param combinationTreatmentCohortIds Integer vector of cohort definition IDs
#'   for combination treatment cohorts (sub-analysis 3). \code{NA} entries are
#'   silently skipped. Cohort names are resolved from
#'   \code{inst/settings/CohortsToCreate.csv}.
#' @param outcomeCohortIds            Integer vector of cohort definition IDs for
#'   adverse event outcome cohorts (sub-analysis 4). \code{NA} entries are
#'   silently skipped. Cohort names are resolved from
#'   \code{inst/settings/CohortsToCreate.csv}; episodic/chronic classification
#'   is resolved from \code{inst/settings/IRsettings.csv}.
#' @param databaseId                  Short site identifier used to label outputs.
#' @param minCellCount                Minimum event count for a cell to be
#'   reported; rows with fewer events have IR and CI suppressed to \code{NA}.
#'   Default 5.
#' @param outputFolder                Path where result files will be written.
#'
#' @export
runUtilisationAnalysis <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   vocabularyDatabaseSchema    = cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   targetCohortId,
                                   combinationTreatmentCohortIds,
                                   outcomeCohortIds,
                                   databaseId,
                                   minCellCount = 5,
                                   outputFolder) {

  utilisationFolder <- file.path(outputFolder, "utilisationAnalysis")
  if (!dir.exists(utilisationFolder)) {
    dir.create(utilisationFolder, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # Load CohortsToCreate.csv once for name resolution
  # --------------------------------------------------------------------------
  cohortsToCreate <- readr::read_csv(
    system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
    col_types = readr::cols()
  )

  .cohortName <- function(id) {
    nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
    if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
  }

  # --------------------------------------------------------------------------
  # Sub-analysis 1 + 2: Baseline demographics and formulation breakdown
  #
  # A single CohortDiagnostics call covering:
  #   - Temporal cohort characterisation (demographics, comorbidities, drugs)
  #   - Index event breakdown (1-/3-/6-month triptorelin depot)
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Utilisation step 1+2: baseline demographics and formulation breakdown")

  baselineFolder <- file.path(utilisationFolder, "baselineAndFormulation")
  if (!dir.exists(baselineFolder)) dir.create(baselineFolder, recursive = TRUE)

  # Build single-row cohort definition set for the target cohort
  allCohortDefs <- CohortGenerator::getCohortDefinitionSet(
    settingsFileName     = system.file("settings", "CohortsToCreate.csv",
                                       package = "PioneerTriptorelin"),
    jsonFolder           = system.file("cohorts",
                                       package = "PioneerTriptorelin"),
    sqlFolder            = system.file("sql", "sql_server",
                                       package = "PioneerTriptorelin"),
    cohortFileNameFormat = "%s",
    cohortFileNameValue  = c("cohortId")
  )

  targetCohortDef <- allCohortDefs[allCohortDefs$cohortId == targetCohortId, ]
  if (nrow(targetCohortDef) == 0) {
    stop("targetCohortId ", targetCohortId, " not found in CohortsToCreate.csv")
  }

  temporalCovariateSettings <- FeatureExtraction::createTemporalCovariateSettings(
    useDemographicsGender               = TRUE,
    useDemographicsAge                  = TRUE,
    useDemographicsAgeGroup             = TRUE,
    useDemographicsRace                 = TRUE,
    useDemographicsEthnicity            = TRUE,
    useDemographicsIndexYear            = TRUE,
    useDemographicsIndexMonth           = TRUE,
    useDemographicsIndexYearMonth       = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime  = TRUE,
    useDemographicsTimeInCohort         = TRUE,
    useConditionOccurrence              = TRUE,
    useProcedureOccurrence              = TRUE,
    useDrugEraStart                     = TRUE,
    useMeasurement                      = TRUE,
    useConditionEraStart                = TRUE,
    useConditionEraOverlap              = TRUE,
    useConditionEraGroupStart           = FALSE, # https://github.com/OHDSI/FeatureExtraction/issues/144
    useConditionEraGroupOverlap         = TRUE,
    useDrugExposure                     = FALSE, # too many concept IDs
    useDrugEraOverlap                   = FALSE,
    useDrugEraGroupStart                = FALSE, # https://github.com/OHDSI/FeatureExtraction/issues/144
    useDrugEraGroupOverlap              = TRUE,
    useObservation                      = TRUE,
    useVisitConceptCount                = TRUE,
    useVisitCount                       = TRUE,
    useDeviceExposure                   = TRUE,
    useCharlsonIndex                    = TRUE,
    useDcsi                             = TRUE,
    useChads2                           = TRUE,
    useChads2Vasc                       = TRUE,
    useHfrs                             = FALSE,
    temporalStartDays = c(
      -9999, # anytime prior
       -365, # long term prior
       -180, # medium term prior
        -30, # short term prior
       -365, # one year prior to -31
        -30, # 30 days prior, not including day 0
          0, # index date only
          1, # 1 day after to day 30
         31, # 31 days after to day 365
      -9999, # any time prior to any time future
        -30,
        -30,
        -30,
        -30
    ),
    temporalEndDays = c(
         0, # anytime prior
         0, # long term prior
         0, # medium term prior
         0, # short term prior
       -31, # one year prior to -31
        -1, # 30 days prior, not including day 0
         0, # index date only
        30, # 1 day after to day 30
       365, # 31 days after to day 365
      9999, # any time prior to any time future
      9999, # all after
       365,
       180,
        30
    )
  )

  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet               = targetCohortDef,
    exportFolder                      = baselineFolder,
    databaseId                        = databaseId,
    connectionDetails                 = connectionDetails,
    cdmDatabaseSchema                 = cdmDatabaseSchema,
    cohortDatabaseSchema              = cohortDatabaseSchema,
    cohortTable                       = cohortTable,
    vocabularyDatabaseSchema          = vocabularyDatabaseSchema,
    temporalCovariateSettings         = temporalCovariateSettings,
    runTemporalCohortCharacterization = TRUE,   # sub-analysis 1: baseline demographics
    runBreakdownIndexEvents           = TRUE,   # sub-analysis 2: formulation breakdown
    runInclusionStatistics            = TRUE,
    runVisitContext                   = TRUE,
    runCohortRelationship             = FALSE,
    runIncidenceRate                  = FALSE,
    runOrphanConcepts                 = FALSE,
    runTimeSeries                     = FALSE,
    minCellCount                      = minCellCount
  )

  ParallelLogger::logInfo(
    "Baseline + formulation results written to ", baselineFolder,
    ". Load with CohortDiagnostics::launchDiagnosticsExplorer()."
  )

  # --------------------------------------------------------------------------
  # Sub-analysis 3: Combination regimen distribution
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Utilisation step 3: combination regimen distribution")

  validCombIds <- combinationTreatmentCohortIds[!is.na(combinationTreatmentCohortIds)]

  if (length(validCombIds) == 0) {
    ParallelLogger::logWarn(
      "combinationTreatmentCohortIds contains only NA values — ",
      "skipping combination regimen counts."
    )
  } else {
    allCounts <- CohortGenerator::getCohortCounts(
      connectionDetails    = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = cohortTable
    )

    targetSubjects <- allCounts$cohortSubjects[allCounts$cohortId == targetCohortId]
    if (length(targetSubjects) == 0) targetSubjects <- NA_integer_

    combCounts <- allCounts[allCounts$cohortId %in% validCombIds, ]
    combCounts$regimenName <- vapply(combCounts$cohortId, .cohortName, character(1))
    combCounts$pctOfTarget <- round(combCounts$cohortSubjects / targetSubjects * 100, 1)
    combCounts <- combCounts[, c("cohortId", "regimenName",
                                 "cohortSubjects", "cohortEntries", "pctOfTarget")]

    readr::write_excel_csv(
      combCounts,
      file.path(utilisationFolder, "combinationRegimens.csv"),
      na = ""
    )
    ParallelLogger::logInfo("Combination regimen counts written to combinationRegimens.csv")
  }

  # --------------------------------------------------------------------------
  # Sub-analysis 4: Adverse event incidence rates
  #
  # Approach: custom IR calculation following PioneerMetastaticAE.
  #   - Episodic/chronic classification from inst/settings/IRsettings.csv
  #   - IR = events / person-years × 1,000
  #   - 95% CI via survival::cipoisson()
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Utilisation step 4: adverse event incidence rates")

  validOutcomeIds <- outcomeCohortIds[!is.na(outcomeCohortIds)]

  if (length(validOutcomeIds) == 0) {
    ParallelLogger::logWarn(
      "outcomeCohortIds contains only NA values — skipping incidence rate analysis."
    )
  } else {
    irSettings <- readr::read_csv(
      system.file("settings", "IRsettings.csv", package = "PioneerTriptorelin"),
      col_types = readr::cols()
    )

    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
    dbms <- attr(connection, "dbms")

    irRows <- lapply(validOutcomeIds, function(outcomeId) {
      outcomeName <- .cohortName(outcomeId)

      # Look up episodic/chronic from IRsettings.csv; default to episodic
      settingsRow <- irSettings[irSettings$outcome_id == outcomeId, ]
      isEpisodic  <- if (nrow(settingsRow) > 0) settingsRow$episodic[1] == 1 else TRUE

      if (isEpisodic) {
        ParallelLogger::logInfo("  IR (episodic): ", outcomeName, " [", outcomeId, "]")
      } else {
        ParallelLogger::logInfo("  IR (chronic):  ", outcomeName, " [", outcomeId, "]")
      }

      tryCatch(
        .calculateIR(
          connection           = connection,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTable          = cohortTable,
          targetId             = targetCohortId,
          outcomeId            = outcomeId,
          outcomeName          = outcomeName,
          isEpisodic           = isEpisodic,
          minCellCount         = minCellCount,
          dbms                 = dbms
        ),
        error = function(e) {
          ParallelLogger::logError(
            "Failed to compute IR for outcome ", outcomeId, ": ", conditionMessage(e)
          )
          NULL
        }
      )
    })

    irResults <- do.call(rbind, Filter(Negate(is.null), irRows))

    readr::write_excel_csv(
      irResults,
      file.path(utilisationFolder, "incidenceRates.csv"),
      na = ""
    )
    ParallelLogger::logInfo("Incidence rates written to incidenceRates.csv")
  }

  ParallelLogger::logInfo(
    "Utilisation analysis complete. Results in ", utilisationFolder
  )
  invisible(NULL)
}
