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

#' Run baseline cohort characterization using CohortDiagnostics
#'
#' Runs temporal cohort characterization via
#' \code{CohortDiagnostics::executeDiagnostics()} for the main triptorelin
#' cohort. Only the temporal characterization module is executed here; full
#' cohort diagnostics (incidence rates, index event breakdown, etc.) are
#' handled separately in the diagnostics step.
#'
#' Results are exported as a zip file to \code{outputFolder/baselineCharacteristics}
#' and can be viewed with \code{CohortDiagnostics::launchDiagnosticsExplorer()}.
#'
#' @param connectionDetails        DatabaseConnector connection details object.
#' @param cdmDatabaseSchema        Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables.
#' @param cohortDatabaseSchema     Schema where cohort tables are written.
#' @param cohortTable              Name of the cohort table.
#' @param cohortId                 Cohort definition ID of the main triptorelin cohort.
#' @param databaseId               Short site identifier used to label outputs.
#' @param outputFolder             Path where result files will be written.
#' @param maxCores                 Number of CPU cores for parallel steps.
#'
#' @export
runBaselineCharacteristics <- function(connectionDetails,
                                       cdmDatabaseSchema,
                                       vocabularyDatabaseSchema = cdmDatabaseSchema,
                                       cohortDatabaseSchema,
                                       cohortTable,
                                       cohortId,
                                       databaseId,
                                       outputFolder,
                                       maxCores = 1) {

  exportFolder <- file.path(outputFolder, "baselineCharacteristics")
  if (!dir.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # Build cohort definition set (triptorelin cohort only)
  # --------------------------------------------------------------------------
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  cohortsToCreate <- dplyr::rename(cohortsToCreate, cohortName = cohort_name)

  row <- cohortsToCreate[cohortsToCreate$cohortId == cohortId, ]
  if (nrow(row) == 0) {
    stop("cohortId ", cohortId, " not found in CohortsToCreate.csv")
  }

  jsonFolder <- system.file("cohorts",           package = "PioneerTriptorelin")
  sqlFolder  <- system.file("sql", "sql_server",  package = "PioneerTriptorelin")

  cohortDefinitionSet <- data.frame(
    cohortId   = row$cohortId,
    cohortName = row$cohortName,
    json       = paste(readLines(file.path(jsonFolder, paste0(row$cohortId, ".json")), warn = FALSE), collapse = "\n"),
    sql        = paste(readLines(file.path(sqlFolder,  paste0(row$cohortId, ".sql")),  warn = FALSE), collapse = "\n"),
    stringsAsFactors = FALSE
  )

  # --------------------------------------------------------------------------
  # Temporal covariate settings
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
      -9999  # any time prior to any time future
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
      9999  # any time prior to any time future
    )
  )

  # --------------------------------------------------------------------------
  # Run characterization via CohortDiagnostics
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Running temporal cohort characterization for cohort ", cohortId)

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
    runInclusionStatistics            = TRUE,
    runIncidenceRate                  = FALSE,
    runCohortRelationship             = TRUE,
    runOrphanConcepts                 = FALSE,
    runBreakdownIndexEvents           = TRUE,
    runTimeSeries                     = FALSE,
    runVisitContext                   = TRUE,
    minCellCount                      = 5
  )

  ParallelLogger::logInfo("Baseline characterization complete. Results written to ", exportFolder)
}
