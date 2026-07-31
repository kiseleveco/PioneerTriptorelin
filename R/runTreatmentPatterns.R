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

#' Run treatment patterns analysis
#'
#' Describes the real-world sequence of therapies around triptorelin initiation
#' using the \pkg{TreatmentPatterns} package (mi-erasmusmc, v1.0.0 settings-object
#' API). The target cohort is the triptorelin cohort; the event cohorts are the
#' individual treatments whose sequences and combinations are traced.
#'
#' Cohorts are NOT re-generated here: this function reuses the cohorts already
#' instantiated in \code{cohortTable} by \code{generateStudyCohorts()}. The
#' target cohort and every cohort in \code{eventCohortIds} must already exist in
#' that table.
#'
#' This function NEVER launches the Shiny app. It only writes the pathway output
#' to \code{outputFolder/treatmentPatterns/output}. Visualise it afterwards with
#' \code{TreatmentPatterns::launchResultsExplorer(outputFolder = ...)}.
#'
#' @param connectionDetails    DatabaseConnector connection details object.
#' @param cdmDatabaseSchema    Schema containing the OMOP CDM tables (read-only).
#' @param cohortDatabaseSchema Schema where cohort tables are written; also used
#'   by TreatmentPatterns for its intermediate storage.
#' @param cohortTable          Name of the (already-generated) cohort table.
#' @param targetCohortId       Integer. Cohort definition ID of the triptorelin
#'   target cohort. Its name is resolved from
#'   \code{inst/settings/CohortsToCreate.csv}.
#' @param eventCohortIds       Integer vector of treatment event cohort IDs whose
#'   sequences/combinations are analysed. \code{NA} entries are silently skipped.
#'   These should be the INDIVIDUAL treatment cohorts (each agent), not the
#'   pre-combined regimen cohorts, since TreatmentPatterns constructs combinations
#'   itself. Names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param databaseId           Short site identifier used to label outputs.
#' @param databaseName         Human-readable database name shown in the results.
#' @param minCellCount         Minimum count for a pathway to be reported; smaller
#'   counts are handled per \code{minCellMethod} ("Remove"). Default 5.
#' @param outputFolder         Path where result files will be written.
#' @param studyName            Label for this pathway analysis. Default
#'   "TriptorelinTreatmentPatterns".
#' @param minEraDuration       Minimum days a treatment era must last to be kept.
#'   Shorter eras are dropped as noise. Default 0.
#' @param eraCollapseSize      Two eras of the SAME treatment separated by a gap of
#'   at most this many days are merged into one era (absorbs refill gaps).
#'   Default 30.
#' @param combinationWindow    Two DIFFERENT treatments must overlap by at least
#'   this many days to be treated as a combination (otherwise sequential).
#'   Default 30.
#' @param minPostCombinationDuration Minimum days a mono-therapy remnant before or
#'   after a split-out combination must last to be kept. Default 30.
#' @param filterTreatments     How eras collapse into pathway steps: "First" (first
#'   appearance only), "Changes" (new step when the treatment set changes), or
#'   "All" (every era). Default "First".
#' @param maxPathLength        Maximum number of steps shown in a pathway
#'   (package maximum 5). Default 5.
#' @param addNoPaths           Include untreated persons as an empty pathway.
#'   Default FALSE.
#'
#' @export
runTreatmentPatterns <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 targetCohortId,
                                 eventCohortIds,
                                 databaseId,
                                 databaseName               = databaseId,
                                 minCellCount               = 5,
                                 outputFolder,
                                 studyName                  = "TriptorelinTreatmentPatterns",
                                 minEraDuration             = 0,
                                 eraCollapseSize            = 30,
                                 combinationWindow          = 30,
                                 minPostCombinationDuration = 30,
                                 filterTreatments           = "First",
                                 maxPathLength              = 5,
                                 addNoPaths                 = FALSE) {

  tpFolder <- file.path(outputFolder, "treatmentPatterns")
  if (!dir.exists(tpFolder)) {
    dir.create(tpFolder, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # Resolve cohort names from CohortsToCreate.csv (IDs are the single source
  # of truth; names are looked up for labelling only)
  # --------------------------------------------------------------------------
  cohortsToCreate <- readr::read_csv(
    system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
    col_types = readr::cols()
  )
  .cohortName <- function(id) {
    nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
    if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
  }

  validEventIds <- eventCohortIds[!is.na(eventCohortIds)]
  if (length(validEventIds) == 0) {
    ParallelLogger::logWarn(
      "eventCohortIds contains only NA values — skipping treatment patterns analysis."
    )
    return(invisible(NULL))
  }

  # --------------------------------------------------------------------------
  # Target / event cohort tables (cohortId + cohortName) for TreatmentPatterns
  # --------------------------------------------------------------------------
  targetCohorts <- data.frame(
    cohortId   = targetCohortId,
    cohortName = .cohortName(targetCohortId),
    stringsAsFactors = FALSE
  )
  eventCohorts <- data.frame(
    cohortId   = validEventIds,
    cohortName = vapply(validEventIds, .cohortName, character(1)),
    stringsAsFactors = FALSE
  )

  ParallelLogger::logInfo(
    "Treatment patterns: target ", .cohortName(targetCohortId),
    " (", targetCohortId, "); ", nrow(eventCohorts), " event cohorts."
  )

  # --------------------------------------------------------------------------
  # Assemble TreatmentPatterns settings objects
  #   - Data: reuse the existing, already-generated cohort table
  #   - Cohort: no generation / no loading (cohorts already instantiated)
  #   - Pathway: era / combination / filtering parameters
  #   - Save: output + temp folders under treatmentPatterns/
  # --------------------------------------------------------------------------
  dataSettings <- TreatmentPatterns::createDataSettings(
    OMOP_CDM             = TRUE,
    connectionDetails    = connectionDetails,
    cdmDatabaseSchema    = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable          = cohortTable
  )

  cohortSettings <- TreatmentPatterns::createCohortSettings(
    targetCohorts   = targetCohorts,
    eventCohorts    = eventCohorts,
    loadCohorts     = FALSE,
    generateCohorts = FALSE
  )

  # createPathwaySettings() takes a list of one-or-more addPathwaySettings() rows
  pathwaySettings <- TreatmentPatterns::createPathwaySettings(
    pathwaySettings_list = list(
      TreatmentPatterns::addPathwaySettings(
        studyName                  = studyName,
        targetCohortId             = targetCohortId,
        eventCohortIds             = validEventIds,
        minEraDuration             = minEraDuration,
        eraCollapseSize            = eraCollapseSize,
        combinationWindow          = combinationWindow,
        minPostCombinationDuration = minPostCombinationDuration,
        filterTreatments           = filterTreatments,
        maxPathLength              = maxPathLength,
        minCellCount               = minCellCount,
        minCellMethod              = "Remove",
        addNoPaths                 = addNoPaths
      )
    )
  )

  saveSettings <- TreatmentPatterns::createSaveSettings(
    databaseName = databaseName,
    rootFolder   = tpFolder,
    outputFolder = file.path(tpFolder, "output"),
    tempFolder   = file.path(tpFolder, "temp")
  )

  # createSaveSettings() nests results under <databaseName>, and pathway_settings.csv
  # is written to <outputFolder>/<databaseName>/settings/. That subfolder is normally
  # created by the cohort create/characterize steps, which we skip here (cohorts
  # already exist), so create the expected tree up front to avoid
  # "cannot open the connection" on the first write.
  dir.create(file.path(tpFolder, "output", databaseName, "settings"),
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tpFolder, "temp"),
             recursive = TRUE, showWarnings = FALSE)

  # --------------------------------------------------------------------------
  # Run — construct pathways and generate output only. NEVER launch Shiny.
  # --------------------------------------------------------------------------
  TreatmentPatterns::executeTreatmentPatterns(
    dataSettings              = dataSettings,
    cohortSettings            = cohortSettings,
    pathwaySettings           = pathwaySettings,
    saveSettings              = saveSettings,
    runCreateCohorts          = FALSE,
    runCohortCharacterization = FALSE,
    runConstructPathways      = TRUE,
    runGenerateOutput         = TRUE,
    launchShiny               = FALSE
  )

  ParallelLogger::logInfo(
    "Treatment patterns output written to ", file.path(tpFolder, "output"),
    ". View with TreatmentPatterns::launchResultsExplorer(outputFolder = \"",
    file.path(tpFolder, "output"), "\")."
  )
  invisible(NULL)
}
