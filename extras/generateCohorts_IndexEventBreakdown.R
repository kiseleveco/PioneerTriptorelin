# ==============================================================================
# PioneerTriptorelin — Cohort Generation + Index Event Breakdown
#
# Standalone script: generates ALL study cohorts from the package and then
# runs the CohortDiagnostics index event breakdown for every cohort.
#
# Purpose:
#   Use this script to inspect what events drive cohort entry BEFORE running
#   the full analysis pipeline (codeToRun.R). It is completely independent of
#   codeToRun.R — no shared state, no shared output folder.
#
# Output (all in <outputFolder>/):
#   CohortCounts.csv                — cohort entry counts (subjects / events)
#   cohortStatistics/               — inclusion rule statistics (if enabled)
#   indexEventBreakdown/            — CohortDiagnostics export folder
#   indexEventBreakdown/*.zip       — zip archive loadable in DiagnosticsExplorer
#   log.txt                         — execution log
#
# Adjust all parameters in Sections 1-2 before running.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Environment
# ------------------------------------------------------------------------------
library(PioneerTriptorelin)
library(DatabaseConnector)
library(CohortGenerator)
library(CohortDiagnostics)
library(ParallelLogger)

source("../credentials.r")

# ------------------------------------------------------------------------------
# 1. Site-specific parameters  (edit these for every site)
# ------------------------------------------------------------------------------

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms     = DBMS,
  user     = USER,
  password = PASSWORD,
  server   = SERVER,
  port     = DB_PORT
)

## -- Schema names -------------------------------------------------------------
cdmDatabaseSchema        <- "lungcancer_synth10k" # read-only CDM
vocabularyDatabaseSchema <- cdmDatabaseSchema      # usually the same as CDM
cohortDatabaseSchema     <- "study_results"        # write-enabled (read/write/delete)
cohortTable              <- "pioneer_triptorelin_diag" # separate table to avoid collisions

## -- Temp table emulation (Oracle / some Spark) -------------------------------
options(sqlRenderTempEmulationSchema = NULL)

## -- Dataset metadata ---------------------------------------------------------
databaseId <- "MarketScan"   # short identifier, no spaces

## -- Output -------------------------------------------------------------------
outputFolder <- file.path("/home/a_kiselev/output", paste0(databaseId, "_indexEventBreakdown"))

# ------------------------------------------------------------------------------
# 2. Script options
# ------------------------------------------------------------------------------
incrementalCohorts <- FALSE  # TRUE = skip cohorts whose SQL has not changed since last run
includeCohortStats <- TRUE   # TRUE = also export inclusion rule statistics

# ------------------------------------------------------------------------------
# 3. Initialise output folder and logging
# ------------------------------------------------------------------------------
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}

# R 4.3.3 workaround ──────────────────────────────────────────────────────────
# ParallelLogger file appenders use file(..., blocking = FALSE) which can throw
# on Linux R 4.3.3, and newer ParallelLogger versions call internal functions
# (getThreadNumber / getLoggerSettings) that are unreachable from closures.
# Fix: self-contained layout + cat()-based file I/O (always available).
# ─────────────────────────────────────────────────────────────────────────────
.safeLayout <- function(level, message) {
  threadLabel <- if (!is.null(getOption("threadNumber")))
    paste("Thread", getOption("threadNumber"))
  else
    "Main thread"
  paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\t[", threadLabel, "]\t",
         level, "\t", message)
}

.makeFileAppender <- function(fileName) {
  fn <- fileName
  app <- list(
    appendFunction = function(this, level, message, echoToConsole) {
      tryCatch(
        cat(message, "\n", file = fn, append = TRUE, sep = ""),
        error = function(e) NULL
      )
    },
    layout = .safeLayout
  )
  class(app) <- "Appender"
  app
}

ParallelLogger::registerLogger(ParallelLogger::createLogger(
  name      = "BREAKDOWN_FILE_LOGGER",
  threshold = "TRACE",
  appenders = list(.makeFileAppender(file.path(outputFolder, "log.txt")))
))
# Clean up on exit (error or normal)
on.exit(ParallelLogger::unregisterLogger("BREAKDOWN_FILE_LOGGER", silent = TRUE))

ParallelLogger::logInfo("=== generateCohorts_IndexEventBreakdown.R starting ===")
ParallelLogger::logInfo("Database  : ", databaseId)
ParallelLogger::logInfo("CDM schema: ", cdmDatabaseSchema)
ParallelLogger::logInfo("Output    : ", outputFolder)

# ------------------------------------------------------------------------------
# 4. Load cohort definition set (all cohorts in the package)
# ------------------------------------------------------------------------------
ParallelLogger::logInfo("Loading cohort definition set from PioneerTriptorelin package")

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
    "No cohort definitions found. ",
    "Check inst/settings/CohortsToCreate.csv, inst/cohorts/, ",
    "and inst/sql/sql_server/ in the installed package."
  )
}

ParallelLogger::logInfo(nrow(cohortDefinitionSet), " cohort definition(s) loaded:")
for (i in seq_len(nrow(cohortDefinitionSet))) {
  ParallelLogger::logInfo("  [", cohortDefinitionSet$cohortId[i], "] ",
                           cohortDefinitionSet$cohortName[i])
}

# ------------------------------------------------------------------------------
# 5. Optionally rebuild SQL with inclusion rule statistics enabled
# ------------------------------------------------------------------------------
if (includeCohortStats) {
  ParallelLogger::logInfo(
    "Rebuilding cohort SQL with generateStats = TRUE (inclusion rule statistics)"
  )
  for (i in seq_len(nrow(cohortDefinitionSet))) {
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortDefinitionSet$json[i])
    cohortDefinitionSet$sql[i] <- CirceR::buildCohortQuery(
      cohortExpression,
      options = CirceR::createGenerateOptions(generateStats = TRUE)
    )
  }
}

# ------------------------------------------------------------------------------
# 6. Create cohort tables and generate cohorts
# ------------------------------------------------------------------------------
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

ParallelLogger::logInfo("Creating cohort tables in ", cohortDatabaseSchema)
CohortGenerator::createCohortTables(
  connectionDetails    = connectionDetails,
  cohortTableNames     = cohortTableNames,
  cohortDatabaseSchema = cohortDatabaseSchema,
  incremental          = incrementalCohorts
)

ParallelLogger::logInfo("Generating cohorts")
CohortGenerator::generateCohortSet(
  connectionDetails    = connectionDetails,
  cdmDatabaseSchema    = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames     = cohortTableNames,
  cohortDefinitionSet  = cohortDefinitionSet,
  incremental          = incrementalCohorts
)

# Cohort counts
counts <- CohortGenerator::getCohortCounts(
  connectionDetails    = connectionDetails,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable          = cohortTableNames$cohortTable
)
readr::write_excel_csv(counts, file.path(outputFolder, "CohortCounts.csv"), na = "")
ParallelLogger::logInfo("Cohort counts:")
for (i in seq_len(nrow(counts))) {
  ParallelLogger::logInfo(
    "  cohortId=", counts$cohortId[i],
    "  subjects=", counts$cohortSubjects[i],
    "  entries=",  counts$cohortEntries[i]
  )
}

# ------------------------------------------------------------------------------
# 7. Export inclusion rule statistics (if enabled)
# ------------------------------------------------------------------------------
if (includeCohortStats) {
  statsFolder <- file.path(outputFolder, "cohortStatistics")
  if (!dir.exists(statsFolder)) dir.create(statsFolder, recursive = TRUE)

  ParallelLogger::logInfo("Inserting inclusion rule names")
  CohortGenerator::insertInclusionRuleNames(
    connectionDetails    = connectionDetails,
    cohortDefinitionSet  = cohortDefinitionSet,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortInclusionTable = cohortTableNames$cohortInclusionTable
  )

  ParallelLogger::logInfo("Exporting inclusion rule statistics to cohortStatistics/")
  CohortGenerator::exportCohortStatsTables(
    connectionDetails      = connectionDetails,
    cohortDatabaseSchema   = cohortDatabaseSchema,
    cohortTableNames       = cohortTableNames,
    cohortStatisticsFolder = statsFolder
  )
  ParallelLogger::logInfo("Inclusion rule statistics written to ", statsFolder)
}

# ------------------------------------------------------------------------------
# 8. Index event breakdown (CohortDiagnostics)
#    Analyses what domain-level events (conditions, drugs, procedures, …)
#    are most commonly contributing to the index date for each cohort.
#    All other CohortDiagnostics modules are disabled to keep runtime short.
#
#    Results are exported as a zip in indexEventBreakdown/ and can be viewed
#    with CohortDiagnostics::launchDiagnosticsExplorer().
# ------------------------------------------------------------------------------
breakdownFolder <- file.path(outputFolder, "indexEventBreakdown")
if (!dir.exists(breakdownFolder)) dir.create(breakdownFolder, recursive = TRUE)

ParallelLogger::logInfo(
  "Running index event breakdown for ", nrow(cohortDefinitionSet), " cohort(s)"
)

CohortDiagnostics::executeDiagnostics(
  cohortDefinitionSet               = cohortDefinitionSet,
  exportFolder                      = breakdownFolder,
  databaseId                        = databaseId,
  connectionDetails                 = connectionDetails,
  cdmDatabaseSchema                 = cdmDatabaseSchema,
  cohortDatabaseSchema              = cohortDatabaseSchema,
  cohortTable                       = cohortTableNames$cohortTable,
  vocabularyDatabaseSchema          = vocabularyDatabaseSchema,
  # ── Only the index event breakdown module is enabled ──────────────────────
  runBreakdownIndexEvents           = TRUE,
  # ── All other modules disabled ────────────────────────────────────────────
  runInclusionStatistics            = FALSE,
  runIncidenceRate                  = FALSE,
  runCohortRelationship             = FALSE,
  runOrphanConcepts                 = FALSE,
  runTemporalCohortCharacterization = FALSE,
  runTimeSeries                     = FALSE,
  runVisitContext                   = FALSE,
  minCellCount                      = 5
)

ParallelLogger::logInfo(
  "Index event breakdown complete. ",
  "Results written to ", breakdownFolder
)

# ------------------------------------------------------------------------------
# 9. View results interactively (optional — uncomment and run in RStudio)
# ------------------------------------------------------------------------------
# CohortDiagnostics::launchDiagnosticsExplorer(
#   exportFolder = breakdownFolder
# )

ParallelLogger::logInfo("=== generateCohorts_IndexEventBreakdown.R done ===")
message("Done. Output: ", outputFolder)
