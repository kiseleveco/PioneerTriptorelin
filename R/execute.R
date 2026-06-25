#' Execute the PioneerTriptorelin study pipeline
#'
#' @param connectionDetails       DatabaseConnector connection details object.
#' @param cdmDatabaseSchema       Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables (usually same as CDM).
#' @param cohortDatabaseSchema    Schema where cohort tables will be written (needs read/write/delete).
#' @param cohortTable             Name of the cohort table to create.
#' @param databaseId              Short site identifier used to label all outputs.
#' @param databaseName            Human-readable site name.
#' @param databaseDescription     One-sentence description of the data source.
#' @param outputFolder            Path where all result files will be written.
#' @param maxCores                Number of CPU cores available for parallel steps.
#' @param incrementalCohorts         Logical. Passed to \code{generateStudyCohorts}: if \code{TRUE},
#'   skip cohorts whose SQL checksum has not changed since the last run.
#' @param includeCohortStats         Logical. Passed to \code{generateStudyCohorts}: if \code{TRUE},
#'   cohort SQL is rebuilt with inclusion rule statistics enabled and the stats are
#'   exported to \code{outputFolder/cohortStatistics/}.
#' @param runCreateCohorts           Logical. Instantiate cohorts via CohortGenerator.
#' @param triptorelinCohortId         Cohort definition ID of the main triptorelin cohort,
#'   passed as \code{targetCohortId} to \code{runBaselineCharacteristics} and
#'   \code{runUtilisationAnalysis}.
#' @param combinationTreatmentCohortIds Integer vector of cohort definition IDs for
#'   combination treatment cohorts (ADT alone, ADT + ARPI, ADT + chemotherapy, etc.),
#'   passed to \code{runUtilisationAnalysis}. \code{NA} entries are silently skipped.
#'   Cohort names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param runBaselineCharacteristics Logical. Extract baseline covariates via FeatureExtraction.
#' @param runUtilisationAnalysis     Logical. Describe depot formulation, combination regimen
#'   use, and adverse event incidence rates.
#' @param outcomeCohortIds           Named integer vector of outcome cohort IDs passed to
#'   \code{runSafetyAnalysis}. Expected names: \code{mace}, \code{mi}, \code{stroke},
#'   \code{af}, \code{vte}, \code{diabetes}, \code{fractures}.
#' @param minCellCount               Minimum event count for KM table rows to be reported.
#'   Rows below this threshold are suppressed. Default 5.
#' @param runSafetyAnalysis          Logical. Run KM + Cox for CV, VTE, diabetes, and fracture endpoints.
#' @param withCvCohortId             Cohort ID for triptorelin patients WITH prior CV history,
#'   passed to \code{runCvComparison}.
#' @param withoutCvCohortId          Cohort ID for triptorelin patients WITHOUT prior CV history,
#'   passed to \code{runCvComparison}.
#' @param cvOutcomeCohortIds         Named integer vector of CV outcome cohort IDs passed to
#'   \code{runCvComparison}. Expected names: \code{mace}, \code{mi}, \code{stroke}, \code{af}.
#' @param runCvComparison            Logical. Compare CV outcomes between with/without CV history
#'   groups using PS-matched Cox PH via CohortMethod.
#' @param runSwitchingAnalysis       Logical. Run KM for switching to alternative ADT.
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable              = "pioneer_triptorelin",
                    databaseId,
                    databaseName             = databaseId,
                    databaseDescription      = "",
                    outputFolder,
                    maxCores                 = parallel::detectCores(),
                    incrementalCohorts              = FALSE,
                    includeCohortStats              = FALSE,
                    triptorelinCohortId,
                    combinationTreatmentCohortIds,
                    outcomeCohortIds,
                    minCellCount                 = 5,
                    withCvCohortId,
                    withoutCvCohortId,
                    cvOutcomeCohortIds,
                    runCreateCohorts             = TRUE,
                    runBaselineCharacteristics   = TRUE,
                    runUtilisationAnalysis       = TRUE,
                    runSafetyAnalysis            = TRUE,
                    runCvComparison              = TRUE,
                    runSwitchingAnalysis         = TRUE) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  # R 4.3.3 workaround ──────────────────────────────────────────────────────────
  # ParallelLogger 3.3.x: file appender uses file(..., blocking = FALSE) which
  #   can throw an error on Linux, cascading into a namespace-scoping failure.
  # ParallelLogger 3.4.0+: layoutParallel / layoutErrorReport call
  #   getThreadNumber(), an internal function that is unreachable from the layout
  #   closure in R 4.3.3.
  #
  # Fix: register equivalent loggers using (a) a self-contained layout that reads
  # only base-R options, and (b) cat()-based file I/O (standard blocking writes).
  # This is version-agnostic and works with any installed ParallelLogger 3.x.
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
    name      = "DEFAULT_FILE_LOGGER",
    threshold = "TRACE",
    appenders = list(.makeFileAppender(file.path(outputFolder, "log.txt")))
  ))
  ParallelLogger::registerLogger(ParallelLogger::createLogger(
    name      = "DEFAULT_ERRORREPORT_LOGGER",
    threshold = "FATAL",
    appenders = list(.makeFileAppender(file.path(outputFolder, "errorReport.txt")))
  ))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER",        silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE),
          add = TRUE)

  ParallelLogger::logInfo("Starting PioneerTriptorelin analysis for database: ", databaseId)

  # ----------------------------------------------------------------------------
  # Step 1: Cohort instantiation
  # ----------------------------------------------------------------------------
  if (runCreateCohorts) {
    ParallelLogger::logInfo("Step 1: Creating cohorts")
    generateStudyCohorts(
      connectionDetails        = connectionDetails,
      cdmDatabaseSchema        = cdmDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      cohortDatabaseSchema     = cohortDatabaseSchema,
      cohortTable              = cohortTable,
      outputFolder             = outputFolder,
      incremental              = incrementalCohorts,
      includeCohortStats       = includeCohortStats
    )
  }

  # ----------------------------------------------------------------------------
  # Step 2: Baseline characteristics
  # ----------------------------------------------------------------------------
  if (runBaselineCharacteristics) {
    ParallelLogger::logInfo("Step 3: Extracting baseline characteristics")
    runBaselineCharacteristics(
      connectionDetails              = connectionDetails,
      cdmDatabaseSchema              = cdmDatabaseSchema,
      vocabularyDatabaseSchema       = vocabularyDatabaseSchema,
      cohortDatabaseSchema           = cohortDatabaseSchema,
      cohortTable                    = cohortTable,
      targetCohortId                 = triptorelinCohortId,
      combinationTreatmentCohortIds  = combinationTreatmentCohortIds,
      outcomeCohortIds               = outcomeCohortIds,
      databaseId                     = databaseId,
      minCellCount                   = minCellCount,
      outputFolder                   = outputFolder
    )
  }

  # ----------------------------------------------------------------------------
  # Step 3: Primary endpoint — utilisation analysis
  #   - Depot formulation distribution (1-month/3.75mg, 3-month/11.25mg, 6-month/22.5mg)
  #   - Combination regimen distribution (ADT alone, ADT+ARPI, ADT+chemo, ADT+chemo+ARPI)
  # ----------------------------------------------------------------------------
  if (runUtilisationAnalysis) {
    ParallelLogger::logInfo("Step 3: Running utilisation analysis")
    runUtilisationAnalysis(
      connectionDetails              = connectionDetails,
      cdmDatabaseSchema              = cdmDatabaseSchema,
      cohortDatabaseSchema           = cohortDatabaseSchema,
      cohortTable                    = cohortTable,
      targetCohortId                 = triptorelinCohortId,
      combinationTreatmentCohortIds  = combinationTreatmentCohortIds,
      outcomeCohortIds               = outcomeCohortIds,
      databaseId                     = databaseId,
      minCellCount                   = minCellCount,
      outputFolder                   = outputFolder
    )
  }

  # ----------------------------------------------------------------------------
  # Step 4: Safety analysis
  #   KM (3-year cumulative incidence) for: MACE, MI, stroke, AF, VTE, diabetes, fractures
  # ----------------------------------------------------------------------------
  if (runSafetyAnalysis) {
    ParallelLogger::logInfo("Step 4: Running safety analysis")
    runSafetyAnalysis(
      connectionDetails    = connectionDetails,
      cdmDatabaseSchema    = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = cohortTable,
      triptorelinCohortId  = triptorelinCohortId,
      outcomeCohortIds     = outcomeCohortIds,
      databaseId           = databaseId,
      minCellCount         = minCellCount,
      outputFolder         = outputFolder
    )
  }

  # ----------------------------------------------------------------------------
  # Step 5: CV comparison — PS-matched Cox PH (CohortMethod)
  #   Target:     triptorelin patients WITH prior CV history
  #   Comparator: triptorelin patients WITHOUT prior CV history
  #   Outcomes:   MACE, MI, stroke, AF
  # ----------------------------------------------------------------------------
  if (runCvComparison) {
    ParallelLogger::logInfo("Step 5: Running CV comparison (CohortMethod)")
    runCvComparison(
      connectionDetails    = connectionDetails,
      cdmDatabaseSchema    = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = cohortTable,
      withCvCohortId       = withCvCohortId,
      withoutCvCohortId    = withoutCvCohortId,
      cvOutcomeCohortIds   = cvOutcomeCohortIds,
      databaseId           = databaseId,
      databaseName         = databaseName,
      databaseDescription  = databaseDescription,
      minCellCount         = minCellCount,
      maxCores             = maxCores,
      outputFolder         = outputFolder
    )
  }

  # ----------------------------------------------------------------------------
  # Step 6: Treatment switching analysis
  #   KM for switching to: Relugolix, Degarelix, Leuprolide, Goserelin, Buserelin,
  #   or surgical castration (3-year probability)
  # ----------------------------------------------------------------------------
  if (runSwitchingAnalysis) {
    ParallelLogger::logInfo("Step 6: Running treatment switching analysis")
    runSwitchingAnalysis(
      connectionDetails    = connectionDetails,
      cdmDatabaseSchema    = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = cohortTable,
      databaseId           = databaseId,
      outputFolder         = outputFolder
    )
  }

  ParallelLogger::logInfo("Analysis complete. Results written to: ", outputFolder)
  invisible(NULL)
}
