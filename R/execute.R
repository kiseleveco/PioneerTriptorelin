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
#' @param runUtilisationAnalysis     Logical. Describe combination regimen use.
#' @param outcomeCohortIds           Integer vector of outcome cohort IDs passed to
#'   \code{runSafetyAnalysis} (KM + incidence rates) and
#'   \code{runBaselineCharacteristics}. Cohort names are resolved from
#'   \code{inst/settings/CohortsToCreate.csv}.
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
#' @param eventCohortIds             Integer vector of treatment event cohort IDs passed to
#'   \code{runTreatmentPatterns}. Should be the INDIVIDUAL treatment cohorts (each agent),
#'   not the pre-combined regimen cohorts. \code{NA} entries are silently skipped.
#'   Cohort names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param runTreatmentPatterns       Logical. Describe real-world treatment sequences and
#'   combinations via the TreatmentPatterns package. Never launches Shiny; output is
#'   written for \code{TreatmentPatterns::launchResultsExplorer}.
#' @param progressionTargetCohortIds Integer vector of target cohort IDs for the landmark
#'   progression analysis (each analysed against every progression event cohort).
#'   \code{NA} entries are silently skipped.
#' @param progressionEventCohortIds  Integer vector of progression event cohort IDs passed to
#'   \code{runProgressionAnalysis}, analysed as separate endpoints. Names are resolved from
#'   \code{inst/settings/CohortsToCreate.csv}.
#' @param landmarkDays               Landmark time in days for the progression analysis
#'   (subjects must be event-free and in follow-up at this time). Default 183.
#' @param progressionHorizonDays     Horizon (days after the landmark) at which cumulative
#'   incidence of progression is reported. Default 1095 (3 years).
#' @param runProgressionAnalysis     Logical. Run the landmark progression (KM) analysis.
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
                    eventCohortIds,
                    progressionTargetCohortIds,
                    progressionEventCohortIds,
                    landmarkDays                 = 183,
                    progressionHorizonDays       = 1095,
                    minCellCount                 = 5,
                    withCvCohortId,
                    withoutCvCohortId,
                    cvOutcomeCohortIds,
                    runCreateCohorts             = TRUE,
                    runBaselineCharacteristics   = TRUE,
                    runUtilisationAnalysis       = TRUE,
                    runSafetyAnalysis            = TRUE,
                    runCvComparison              = TRUE,
                    runSwitchingAnalysis         = TRUE,
                    runTreatmentPatterns         = TRUE,
                    runProgressionAnalysis       = TRUE) {

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
    ParallelLogger::logInfo("Step 2: Extracting baseline characteristics")
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

  # ----------------------------------------------------------------------------
  # Step 7: Treatment patterns analysis (TreatmentPatterns)
  #   Real-world treatment sequences and combinations around triptorelin.
  #   Output only — never launches Shiny; view with launchResultsExplorer().
  # ----------------------------------------------------------------------------
  if (runTreatmentPatterns) {
    ParallelLogger::logInfo("Step 7: Running treatment patterns analysis")
    runTreatmentPatterns(
      connectionDetails    = connectionDetails,
      cdmDatabaseSchema    = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = cohortTable,
      targetCohortId       = triptorelinCohortId,
      eventCohortIds       = eventCohortIds,
      databaseId           = databaseId,
      databaseName         = databaseName,
      minCellCount         = minCellCount,
      outputFolder         = outputFolder
    )
  }

  # ----------------------------------------------------------------------------
  # Step 8: Progression analysis (landmark KM)
  #   Time to first progression event after the landmark (default 183 days),
  #   among subjects event-free & in follow-up at the landmark. Each target x
  #   event pair is a separate endpoint.
  # ----------------------------------------------------------------------------
  if (runProgressionAnalysis) {
    ParallelLogger::logInfo("Step 8: Running progression analysis (landmark)")
    runProgressionAnalysis(
      connectionDetails    = connectionDetails,
      cdmDatabaseSchema    = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable          = cohortTable,
      targetCohortIds      = progressionTargetCohortIds,
      eventCohortIds       = progressionEventCohortIds,
      landmarkDays         = landmarkDays,
      horizonDays          = progressionHorizonDays,
      databaseId           = databaseId,
      minCellCount         = minCellCount,
      outputFolder         = outputFolder
    )
  }

  ParallelLogger::logInfo("Analysis complete. Results written to: ", outputFolder)
  invisible(NULL)
}
