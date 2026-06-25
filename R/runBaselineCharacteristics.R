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

#' Run cohort diagnostics for all study cohorts
#'
#' Executes \code{CohortDiagnostics::executeDiagnostics()} across every cohort
#' used in the study: the main triptorelin cohort, all combination treatment
#' cohorts, and all outcome cohorts. Running all cohorts together allows
#' side-by-side comparison in the Diagnostics Explorer.
#'
#' Modules run:
#' \itemize{
#'   \item Temporal cohort characterization (demographics, comorbidities, drugs)
#'   \item Index event breakdown (formulation / dosing interval)
#'   \item Inclusion statistics
#'   \item Visit context
#'   \item Cohort relationship
#' }
#'
#' Results are exported as a zip file to
#' \code{outputFolder/baselineCharacteristics} and can be viewed with
#' \code{CohortDiagnostics::launchDiagnosticsExplorer()}.
#'
#' @param connectionDetails               DatabaseConnector connection details object.
#' @param cdmDatabaseSchema               Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema        Schema containing the vocabulary tables.
#' @param cohortDatabaseSchema            Schema where cohort tables are written.
#' @param cohortTable                     Name of the cohort table.
#' @param targetCohortId                  Cohort definition ID of the main triptorelin cohort.
#' @param combinationTreatmentCohortIds   Integer vector of combination treatment cohort IDs.
#'   \code{NA} entries are silently skipped.
#' @param outcomeCohortIds                Integer vector of outcome cohort IDs.
#'   \code{NA} entries are silently skipped.
#' @param databaseId                      Short site identifier used to label outputs.
#' @param minCellCount                    Minimum cell count for suppression. Default 5.
#' @param outputFolder                    Path where result files will be written.
#'
#' @export
runBaselineCharacteristics <- function(connectionDetails,
                                       cdmDatabaseSchema,
                                       vocabularyDatabaseSchema        = cdmDatabaseSchema,
                                       cohortDatabaseSchema,
                                       cohortTable,
                                       targetCohortId,
                                       combinationTreatmentCohortIds   = integer(0),
                                       outcomeCohortIds                = integer(0),
                                       databaseId,
                                       minCellCount                    = 5,
                                       outputFolder) {

  exportFolder <- file.path(outputFolder, "baselineCharacteristics")
  if (!dir.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # Collect all unique cohort IDs used in the study
  # --------------------------------------------------------------------------
  allIds <- unique(c(
    targetCohortId,
    combinationTreatmentCohortIds[!is.na(combinationTreatmentCohortIds)],
    outcomeCohortIds[!is.na(outcomeCohortIds)]
  ))

  # --------------------------------------------------------------------------
  # Build cohort definition set for all study cohorts
  # --------------------------------------------------------------------------
  pathToCsv       <- system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  cohortsToCreate <- dplyr::rename(cohortsToCreate, cohortName = cohort_name)

  jsonFolder <- system.file("cohorts",          package = "PioneerTriptorelin")
  sqlFolder  <- system.file("sql", "sql_server", package = "PioneerTriptorelin")

  rows <- cohortsToCreate[cohortsToCreate$cohortId %in% allIds, ]
  if (nrow(rows) == 0) {
    stop("None of the supplied cohort IDs were found in CohortsToCreate.csv")
  }

  missing <- setdiff(allIds, rows$cohortId)
  if (length(missing) > 0) {
    ParallelLogger::logWarn(
      "The following cohort IDs were not found in CohortsToCreate.csv and will be skipped: ",
      paste(missing, collapse = ", ")
    )
  }

  cohortDefinitionSet <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    data.frame(
      cohortId   = rows$cohortId[i],
      cohortName = rows$cohortName[i],
      json       = paste(readLines(file.path(jsonFolder, paste0(rows$cohortId[i], ".json")), warn = FALSE), collapse = "\n"),
      sql        = paste(readLines(file.path(sqlFolder,  paste0(rows$cohortId[i], ".sql")),  warn = FALSE), collapse = "\n"),
      stringsAsFactors = FALSE
    )
  }))

  # --------------------------------------------------------------------------
  # Temporal covariate settings (14 windows)
  # --------------------------------------------------------------------------
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
        -30, # 30 days prior to 9999 days after
        -30, # 30 days prior to 365 days after
        -30, # 30 days prior to 180 days after
        -30  # 30 days prior to 30 days after
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

  # --------------------------------------------------------------------------
  # Run diagnostics across all study cohorts
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Running cohort diagnostics for ", nrow(cohortDefinitionSet), " cohorts: ",
    paste(cohortDefinitionSet$cohortId, collapse = ", ")
  )

  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet               = cohortDefinitionSet,
    exportFolder                      = exportFolder,
    databaseId                        = databaseId,
    connectionDetails                 = connectionDetails,
    cdmDatabaseSchema                 = cdmDatabaseSchema,
    cohortDatabaseSchema              = cohortDatabaseSchema,
    cohortTable                       = cohortTable,
    vocabularyDatabaseSchema          = vocabularyDatabaseSchema,
    temporalCovariateSettings         = temporalCovariateSettings,
    runTemporalCohortCharacterization = TRUE,
    runBreakdownIndexEvents           = TRUE,
    runInclusionStatistics            = TRUE,
    runVisitContext                   = TRUE,
    runCohortRelationship             = TRUE,
    runIncidenceRate                  = FALSE,
    runOrphanConcepts                 = FALSE,
    runTimeSeries                     = FALSE,
    minCellCount                      = minCellCount
  )

  ParallelLogger::logInfo(
    "Cohort diagnostics complete. Results written to ", exportFolder,
    ". Load with CohortDiagnostics::launchDiagnosticsExplorer()."
  )
  invisible(NULL)
}
