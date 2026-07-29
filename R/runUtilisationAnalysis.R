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
# Main function
# ------------------------------------------------------------------------------

#' Describe real-world utilisation of triptorelin in mHSPC
#'
#' Computes the \strong{combination regimen distribution} — subject counts and
#' percentages for each combination treatment cohort (e.g. ADT alone,
#' ADT + ARPI, ADT + chemotherapy, ADT + ARPI + chemotherapy).
#'
#' Cohort diagnostics (baseline demographics, index event breakdown, formulation
#' distribution) are produced separately by \code{runBaselineCharacteristics()};
#' adverse event incidence rates are produced by \code{runSafetyAnalysis()}.
#'
#' @param connectionDetails           DatabaseConnector connection details object.
#' @param cdmDatabaseSchema           Schema containing the OMOP CDM tables
#'   (read-only).
#' @param cohortDatabaseSchema        Schema where cohort tables are written
#'   (needs read/write/delete).
#' @param cohortTable                 Name of the cohort table.
#' @param targetCohortId              Integer. Cohort definition ID of the main
#'   triptorelin cohort (used as denominator for combination regimen
#'   percentages).
#' @param combinationTreatmentCohortIds Integer vector of cohort definition IDs
#'   for combination treatment cohorts. \code{NA} entries are silently skipped.
#'   Cohort names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param databaseId                  Short site identifier used to label outputs.
#' @param minCellCount                Minimum count for a cell to be reported.
#'   Default 5.
#' @param outputFolder                Path where result files will be written.
#'
#' @export
runUtilisationAnalysis <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   targetCohortId,
                                   combinationTreatmentCohortIds,
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
  # Combination regimen distribution
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Utilisation: combination regimen distribution")

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

  ParallelLogger::logInfo(
    "Utilisation analysis complete. Results in ", utilisationFolder
  )
  invisible(NULL)
}
