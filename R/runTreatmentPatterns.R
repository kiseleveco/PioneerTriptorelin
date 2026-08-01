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
#' using the \pkg{TreatmentPatterns} package (CRAN v3.1.2 API:
#' \code{computePathways()} + \code{export()}). The target cohort is the
#' triptorelin cohort; the event cohorts are the individual treatments whose
#' sequences and combinations are traced.
#'
#' Cohorts are NOT re-generated here: this function reuses the cohorts already
#' instantiated in \code{cohortTable} by \code{generateStudyCohorts()}. The
#' target cohort and every cohort in \code{eventCohortIds} must already exist in
#' that table.
#'
#' Output is written to \code{outputFolder/treatmentPatterns}:
#' \itemize{
#'   \item \code{output/} — shareable aggregate CSV files from \code{export()}.
#'   \item \code{sunburst.html}, \code{sankey.html} — standalone interactive
#'     plots (best effort; requires \pkg{htmlwidgets}).
#' }
#' The \code{export()} results object is returned invisibly so it can be plotted
#' interactively with \code{results$plotSunburst()} / \code{results$plotSankey()}.
#'
#' @param connectionDetails    DatabaseConnector connection details object.
#' @param cdmDatabaseSchema    Schema containing the OMOP CDM tables (read-only),
#'   passed to \code{computePathways(cdmSchema = )}.
#' @param cohortDatabaseSchema Schema holding the cohort table, passed to
#'   \code{computePathways(resultSchema = )}.
#' @param cohortTable          Name of the (already-generated) cohort table,
#'   passed to \code{computePathways(cohortTableName = )}.
#' @param targetCohortId       Integer. Cohort definition ID of the triptorelin
#'   target cohort. Its name is resolved from
#'   \code{inst/settings/CohortsToCreate.csv}.
#' @param eventCohortIds       Integer vector of treatment event cohort IDs whose
#'   sequences/combinations are analysed. \code{NA} entries are silently skipped.
#'   These should be the INDIVIDUAL treatment cohorts (each agent), not the
#'   pre-combined regimen cohorts, since TreatmentPatterns constructs combinations
#'   itself. Names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param databaseId           Short site identifier used to label outputs.
#' @param databaseName         Human-readable database name used as the analysis
#'   \code{description}.
#' @param minCellCount         Minimum count for a pathway to be reported;
#'   smaller counts are censored per \code{censorType = "minCellCount"}. Default 5.
#' @param outputFolder         Path where result files will be written.
#' @param studyName            Label for this pathway analysis (used in logs).
#'   Default "TriptorelinTreatmentPatterns".
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
#' @param addNoPaths           Include untreated persons as a "None" pathway,
#'   passed to \code{export(nonePaths = )}. Default FALSE.
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

  tpFolder  <- file.path(outputFolder, "treatmentPatterns")
  csvFolder <- file.path(tpFolder, "output")
  dir.create(csvFolder, recursive = TRUE, showWarnings = FALSE)

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
  # cohorts data frame for computePathways(): columns cohortId, cohortName, type
  #   type = "target" for the triptorelin cohort, "event" for each treatment
  # --------------------------------------------------------------------------
  cohorts <- rbind(
    data.frame(
      cohortId   = targetCohortId,
      cohortName = .cohortName(targetCohortId),
      type       = "target",
      stringsAsFactors = FALSE
    ),
    data.frame(
      cohortId   = validEventIds,
      cohortName = vapply(validEventIds, .cohortName, character(1)),
      type       = "event",
      stringsAsFactors = FALSE
    )
  )

  ParallelLogger::logInfo(
    "Treatment patterns [", studyName, "]: target ", .cohortName(targetCohortId),
    " (", targetCohortId, "); ", length(validEventIds), " event cohorts."
  )

  # --------------------------------------------------------------------------
  # 1. Construct pathways from the existing cohort table
  #
  # computePathways(connectionDetails = ) references the cohort table by BARE
  # name — dplyr::tbl(con, cohortTableName) — with no schema, so on schema-based
  # platforms it fails (e.g. Redshift: relation "cohort2" does not exist).
  # Build the CDMConnector cdm ourselves and register the cohort table WITH its
  # schema, then pass cdm = (which makes computePathways skip that bare
  # reference and use ours).
  # --------------------------------------------------------------------------
  con <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  cdm <- CDMConnector::cdmFromCon(
    con         = con,
    cdmSchema   = cdmDatabaseSchema,
    writeSchema = cohortDatabaseSchema
  )
  cdm[[cohortTable]] <- dplyr::tbl(
    con,
    CDMConnector::inSchema(schema = cohortDatabaseSchema, table = cohortTable)
  )

  andromeda <- TreatmentPatterns::computePathways(
    cohorts                    = cohorts,
    cohortTableName            = cohortTable,
    cdm                        = cdm,
    analysisId                 = 1,
    description                = databaseName,
    minEraDuration             = minEraDuration,
    eraCollapseSize            = eraCollapseSize,
    combinationWindow          = combinationWindow,
    minPostCombinationDuration = minPostCombinationDuration,
    filterTreatments           = filterTreatments,
    maxPathLength              = maxPathLength
  )
  on.exit(
    tryCatch(Andromeda::close(andromeda), error = function(e) NULL),
    add = TRUE
  )

  # --------------------------------------------------------------------------
  # 2. Export aggregate results (writes shareable CSV files to csvFolder)
  # --------------------------------------------------------------------------
  results <- TreatmentPatterns::export(
    andromeda    = andromeda,
    outputPath   = csvFolder,
    minCellCount = minCellCount,
    censorType   = "minCellCount",
    nonePaths    = addNoPaths
  )
  ParallelLogger::logInfo("Treatment patterns CSVs written to ", csvFolder)

  # --------------------------------------------------------------------------
  # 3. Save interactive sunburst + Sankey as standalone HTML (best effort)
  # --------------------------------------------------------------------------
  if (requireNamespace("htmlwidgets", quietly = TRUE)) {
    tryCatch({
      htmlwidgets::saveWidget(
        results$plotSunburst(),
        file.path(tpFolder, "sunburst.html"), selfcontained = TRUE
      )
      htmlwidgets::saveWidget(
        results$plotSankey(),
        file.path(tpFolder, "sankey.html"), selfcontained = TRUE
      )
      ParallelLogger::logInfo("Saved sunburst.html and sankey.html to ", tpFolder)
    }, error = function(e) {
      ParallelLogger::logWarn("Could not save pathway plots: ", conditionMessage(e))
    })
  } else {
    ParallelLogger::logInfo(
      "Install 'htmlwidgets' to save sunburst.html / sankey.html. ",
      "The returned results object still supports interactive plotting."
    )
  }

  ParallelLogger::logInfo(
    "Treatment patterns complete. Visualise with results$plotSankey() / ",
    "results$plotSunburst(), or open the saved .html files."
  )
  invisible(results)
}
