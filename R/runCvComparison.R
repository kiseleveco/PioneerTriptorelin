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

#' Compare CV outcomes between triptorelin patients with and without prior CV history
#'
#' Uses \pkg{CohortMethod} (\code{runCmAnalyses}) with 1:1 propensity score
#' matching to compare the risk of CV outcomes (MACE, MI, stroke, AF) between
#' triptorelin patients with prior CV history (target) and without (comparator).
#' A stratified Cox proportional hazards model is fitted in the matched population.
#'
#' Intermediate objects (CmData, PS models, matched populations) are written to
#' \code{outputFolder/cvComparison/cm/} by \code{runCmAnalyses} and contain
#' individual-level data — this subfolder must remain at the site and must not
#' be shared. Only the aggregated export produced by \code{exportToCsv} in
#' \code{outputFolder/cvComparison/export/} is intended for sharing and is
#' compatible with \code{OhdsiShinyModules::cohortMethodModule()}.
#'
#' @param connectionDetails     DatabaseConnector connection details object.
#' @param cdmDatabaseSchema     Schema containing the OMOP CDM tables (read-only).
#' @param cohortDatabaseSchema  Schema where cohort tables are written.
#' @param cohortTable           Name of the cohort table.
#' @param withCvCohortId        Cohort ID for triptorelin patients WITH prior CV history (target).
#' @param withoutCvCohortId     Cohort ID for triptorelin patients WITHOUT prior CV history (comparator).
#' @param cvOutcomeCohortIds    Integer vector of CV outcome cohort IDs.
#'   Cohort names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param databaseId            Short site identifier used to label outputs.
#' @param databaseName          Human-readable site name.
#' @param databaseDescription   One-sentence description of the data source.
#' @param minCellCount          Minimum count for a cell to be reported in the
#'   export; smaller values are suppressed. Default 5.
#' @param maxCores              Number of CPU cores for parallel steps.
#' @param outputFolder          Path where result files will be written.
#'
#' @export
runCvComparison <- function(connectionDetails,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             cohortTable,
                             withCvCohortId,
                             withoutCvCohortId,
                             cvOutcomeCohortIds,
                             databaseId,
                             databaseName        = databaseId,
                             databaseDescription = "",
                             minCellCount        = 5,
                             maxCores            = 1,
                             outputFolder) {

  # Intermediate CohortMethod objects (individual-level — site only, do not share)
  cmFolder     <- file.path(outputFolder, "cvComparison", "cm")
  # Aggregated export (share with study coordinator / load into Shiny app)
  exportFolder <- file.path(outputFolder, "cvComparison", "export")

  if (!dir.exists(cmFolder))     dir.create(cmFolder,     recursive = TRUE)
  if (!dir.exists(exportFolder)) dir.create(exportFolder, recursive = TRUE)

  # --------------------------------------------------------------------------
  # 1. Analysis design
  # --------------------------------------------------------------------------

  # Covariate settings for propensity score model
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
    addDescendantsToExclude = TRUE
  )

  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    covariateSettings = covariateSettings
  )

  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback           = 99999,
    riskWindowStart                = 0,
    startAnchor                    = "cohort start",
    riskWindowEnd                  = 1095,   # 3-year follow-up
    endAnchor                      = "cohort start",
    minDaysAtRisk                  = 1
  )

  createPsArgs <- CohortMethod::createCreatePsArgs(
    control = Cyclops::createControl(
      cvType           = "auto",
      startingVariance = 0.01,
      noiseLevel       = "quiet",
      fold             = 10,
      seed             = 42
    )
  )

  matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
    caliper      = 0.2,
    caliperScale = "standardized logit",
    maxRatio     = 1
  )

  fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
    modelType  = "cox",
    stratified = TRUE
  )

  cmAnalysis <- CohortMethod::createCmAnalysis(
    analysisId                = 1,
    description               = "PS-matched Cox PH: triptorelin with vs without prior CV history",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs        = createStudyPopArgs,
    createPsArgs              = createPsArgs,
    matchOnPsArgs             = matchOnPsArgs,
    fitOutcomeModelArgs       = fitOutcomeModelArgs
  )

  # --------------------------------------------------------------------------
  # 2. Target-comparator-outcomes specification
  # --------------------------------------------------------------------------
  cohortsToCreate <- readr::read_csv(
    system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
    col_types = readr::cols()
  )
  .cohortName <- function(id) {
    nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
    if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
  }

  outcomes <- lapply(seq_along(cvOutcomeCohortIds), function(i) {
    CohortMethod::createOutcome(
      outcomeId         = cvOutcomeCohortIds[[i]],
      outcomeOfInterest = TRUE
    )
  })

  tcos <- CohortMethod::createTargetComparatorOutcomes(
    targetId     = withCvCohortId,
    comparatorId = withoutCvCohortId,
    outcomes     = outcomes
  )

  # --------------------------------------------------------------------------
  # 3. Run analyses
  #    Intermediate results (individual-level) are written to cmFolder.
  #    runCmAnalyses is incremental: reruns only incomplete steps.
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Running CohortMethod analyses (intermediate data in ", cmFolder, ")"
  )

  CohortMethod::runCmAnalyses(
    connectionDetails            = connectionDetails,
    cdmDatabaseSchema            = cdmDatabaseSchema,
    exposureDatabaseSchema       = cohortDatabaseSchema,
    exposureTable                = cohortTable,
    outcomeDatabaseSchema        = cohortDatabaseSchema,
    outcomeTable                 = cohortTable,
    outputFolder                 = cmFolder,
    cmAnalysisList               = list(cmAnalysis),
    targetComparatorOutcomesList = list(tcos),
    refitPsForEveryOutcome       = FALSE,
    refitPsForEveryStudyPopulation = FALSE,
    maxCores                     = maxCores
  )

  # --------------------------------------------------------------------------
  # 4. Export aggregated results
  #    Produces standard OHDSI CohortMethod CSV files (cm_result.csv,
  #    cm_attrition.csv, cm_covariate_balance.csv, cm_ps_dist.csv, etc.)
  #    compatible with OhdsiShinyModules::cohortMethodModule().
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Exporting aggregated results to ", exportFolder
  )

  CohortMethod::exportToCsv(
    outputFolder        = cmFolder,
    exportFolder        = exportFolder,
    databaseId          = databaseId,
    databaseName        = databaseName,
    databaseDescription = databaseDescription,
    minCellCount        = minCellCount,
    maxCores            = maxCores
  )

  ParallelLogger::logInfo(
    "CV comparison complete.",
    "\n  Intermediate (site-only): ", cmFolder,
    "\n  Export (for sharing):     ", exportFolder
  )
  invisible(NULL)
}
