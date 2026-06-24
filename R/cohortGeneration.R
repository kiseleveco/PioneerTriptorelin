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

#' Instantiate study cohorts on the CDM
#'
#' Generates all study cohorts using the \pkg{CohortGenerator} package.
#' Cohort definitions are read from \code{inst/settings/CohortsToCreate.csv},
#' \code{inst/cohorts/} (JSON), and \code{inst/sql/sql_server/} (SQL).
#'
#' This function is intentionally separate from \code{createCohorts.R}, which
#' is managed by \code{ROhdsiWebApi::insertCohortDefinitionSetInPackage} and
#' will be overwritten whenever cohort definitions are refreshed from Atlas.
#'
#' When \code{includeCohortStats = TRUE} the cohort SQL is rebuilt from the
#' Circe JSON expressions using
#' \code{CirceR::buildCohortQuery(..., generateStats = TRUE)} so that
#' inclusion rule statistics are populated during generation. The statistics
#' are then exported to \code{outputFolder/cohortStatistics/} as CSV files
#' compatible with \code{CohortDiagnostics}.
#'
#' @param connectionDetails        DatabaseConnector connection details object.
#' @param cdmDatabaseSchema        Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables
#'   (typically the same as \code{cdmDatabaseSchema}).
#' @param cohortDatabaseSchema     Schema where cohort tables will be written
#'   (needs read / write / delete).
#' @param cohortTable              Base name for the cohort table. Additional
#'   statistics tables are derived from this name by
#'   \code{CohortGenerator::getCohortTableNames()}.
#' @param outputFolder             Path where result files will be written.
#' @param incremental              Logical. If \code{TRUE},
#'   \code{CohortGenerator::generateCohortSet} skips cohorts whose SQL
#'   checksum has not changed since the last run.
#' @param includeCohortStats       Logical. If \code{TRUE}, regenerates cohort
#'   SQL with \code{generateStats = TRUE} so that inclusion rule statistics
#'   are captured and then exports them to
#'   \code{outputFolder/cohortStatistics/}.
#'
#' @export
generateStudyCohorts <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  vocabularyDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  outputFolder,
                                  incremental        = FALSE,
                                  includeCohortStats = FALSE) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # 1. Load cohort definition set from inst/
  #    getCohortDefinitionSet reads CohortsToCreate.csv and pairs each row
  #    with its JSON (inst/cohorts/{cohortId}.json) and SQL
  #    (inst/sql/sql_server/{cohortId}.sql).
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Loading cohort definition set from package")

  cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
    settingsFileName     = system.file("settings", "CohortsToCreate.csv",
                                       package = "PioneerTriptorelin"),
    jsonFolder           = system.file("cohorts",
                                       package = "PioneerTriptorelin"),
    sqlFolder            = system.file("sql", "sql_server",
                                       package = "PioneerTriptorelin"),
    cohortFileNameFormat = "%s",
    cohortFileNameValue  = c("cohortId")
  )

  if (nrow(cohortDefinitionSet) == 0) {
    stop(
      "No cohorts found. Verify inst/settings/CohortsToCreate.csv, ",
      "inst/cohorts/, and inst/sql/sql_server/ are populated."
    )
  }

  ParallelLogger::logInfo(nrow(cohortDefinitionSet), " cohort(s) found in definition set.")

  # --------------------------------------------------------------------------
  # 2. Optionally rebuild SQL with inclusion rule statistics
  #    CirceR::buildCohortQuery with generateStats = TRUE injects the
  #    additional INSERT statements that populate the four stats tables
  #    (cohort_inclusion, cohort_inclusion_result,
  #     cohort_summary_stats, cohort_censor_stats).
  # --------------------------------------------------------------------------
  if (includeCohortStats) {
    ParallelLogger::logInfo(
      "Rebuilding cohort SQL with generateStats = TRUE (inclusion rule statistics)"
    )
    for (i in seq_len(nrow(cohortDefinitionSet))) {
      cohortExpression <- CirceR::cohortExpressionFromJson(
        cohortDefinitionSet$json[i]
      )
      cohortDefinitionSet$sql[i] <- CirceR::buildCohortQuery(
        cohortExpression,
        options = CirceR::createGenerateOptions(generateStats = TRUE)
      )
    }
  }

  # --------------------------------------------------------------------------
  # 3. Resolve cohort table names
  #    getCohortTableNames returns a named list:
  #      $cohortTable, $cohortInclusionTable, $cohortInclusionResultTable,
  #      $cohortSummaryStatsTable, $cohortCensorStatsTable
  # --------------------------------------------------------------------------
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

  # --------------------------------------------------------------------------
  # 4. Create cohort tables on the database
  #    With incremental = TRUE the function checks whether tables already
  #    exist before creating them, preserving any previous results.
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Creating cohort tables")
  CohortGenerator::createCohortTables(
    connectionDetails    = connectionDetails,
    cohortTableNames     = cohortTableNames,
    cohortDatabaseSchema = cohortDatabaseSchema,
    incremental          = incremental
  )

  # --------------------------------------------------------------------------
  # 5. Generate cohorts
  #    In incremental mode a checksum of each cohort's SQL is stored in the
  #    database; cohorts whose SQL has not changed are skipped on re-runs.
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo("Generating cohorts")
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails    = connectionDetails,
    cdmDatabaseSchema    = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames     = cohortTableNames,
    cohortDefinitionSet  = cohortDefinitionSet,
    incremental          = incremental
  )

  # --------------------------------------------------------------------------
  # 6. Cohort counts
  # --------------------------------------------------------------------------
  counts <- CohortGenerator::getCohortCounts(
    connectionDetails    = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable          = cohortTableNames$cohortTable
  )

  readr::write_excel_csv(counts, file.path(outputFolder, "CohortCounts.csv"), na = "")
  ParallelLogger::logInfo("Cohort counts written to CohortCounts.csv")

  # --------------------------------------------------------------------------
  # 7. Export cohort statistics (inclusion rule statistics)
  #    Step 7a: insert human-readable rule names from the Circe expressions
  #             (these are not stored automatically during generation).
  #    Step 7b: export the four stats tables to CSV files in cohortStatistics/.
  #             These files are compatible with CohortDiagnostics and can be
  #             shared with the study coordinator.
  # --------------------------------------------------------------------------
  if (includeCohortStats) {
    statsFolder <- file.path(outputFolder, "cohortStatistics")
    if (!dir.exists(statsFolder)) {
      dir.create(statsFolder, recursive = TRUE)
    }

    ParallelLogger::logInfo("Inserting inclusion rule names")
    CohortGenerator::insertInclusionRuleNames(
      connectionDetails    = connectionDetails,
      cohortDefinitionSet  = cohortDefinitionSet,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortInclusionTable = cohortTableNames$cohortInclusionTable
    )

    ParallelLogger::logInfo("Exporting cohort statistics to cohortStatistics/")
    CohortGenerator::exportCohortStatsTables(
      connectionDetails      = connectionDetails,
      cohortDatabaseSchema   = cohortDatabaseSchema,
      cohortTableNames       = cohortTableNames,
      cohortStatisticsFolder = statsFolder
    )

    ParallelLogger::logInfo(
      "Cohort statistics (inclusion rule stats) written to ", statsFolder
    )
  }

  invisible(counts)
}
