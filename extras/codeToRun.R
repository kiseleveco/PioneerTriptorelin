# ==============================================================================
# PioneerTriptorelin — Study Execution Script
# Run this file to execute the full analysis pipeline at a single site.
# Adjust all parameters in Sections 1–2 before running.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Environment
# ------------------------------------------------------------------------------
library(PioneerTriptorelin)
library(DatabaseConnector)
source("../credentials.r")

# ------------------------------------------------------------------------------
# 1. Site-specific parameters  (edit these for every site)
# ------------------------------------------------------------------------------

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms        = DBMS,
  user        = USER,
  password    = PASSWORD,
  server      = SERVER,
  port        = DB_PORT
)

## -- Schema names -------------------------------------------------------------
cdmDatabaseSchema        <- "lungcancer_synth10k"# read-only CDM
vocabularyDatabaseSchema <- cdmDatabaseSchema     # usually same as CDM
cohortDatabaseSchema     <- "study_results"        # write-enabled; needs read/write/delete
cohortTable              <- "cohort"

## -- Temp table emulation (Oracle / some Spark configs) -----------------------
options(sqlRenderTempEmulationSchema = NULL)

## -- Cohort IDs ---------------------------------------------------------------
triptorelinCohortId <- 1686   # cohort definition ID of the main triptorelin cohort

# Combination treatment cohorts for utilisation analysis (fill in once finalised in Atlas)
# IDs only — cohort names are resolved automatically from CohortsToCreate.csv
combinationTreatmentCohortIds <- c(
  1699,   # ADT only
  NA,     # ADT + ARPI (abiraterone / enzalutamide / apalutamide / darolutamide)
  NA,     # ADT + docetaxel
  NA      # ADT + docetaxel + ARPI
)

# Adverse event cohorts for incidence rate analysis (fill in once finalised in Atlas)
# IDs only — names come from CohortsToCreate.csv; episodic/chronic from IRsettings.csv
aeCohortIds <- c(
  1702,   # ACE (acute cardiovascular events)
  1703    # Fractures
)

# Outcome cohort IDs for safety analysis (fill in once cohorts are finalised in Atlas)
outcomeCohortIds <- c(
  mace      = 1702,   # MACE (composite)
  mi        = NA,   # myocardial infarction
  stroke    = NA,   # stroke
  af        = NA,   # atrial fibrillation
  vte       = NA,   # venous thromboembolism
  diabetes  = NA,   # diabetes mellitus
  fractures = 1703    # fractures
)

minCellCount      <- 5    # suppress KM rows / cells with fewer events than this

# CV comparison cohorts (CohortMethod step — fill in once cohorts are finalised in Atlas)
withCvCohortId    <- NA   # triptorelin patients WITH prior CV history
withoutCvCohortId <- NA   # triptorelin patients WITHOUT prior CV history

cvOutcomeCohortIds <- c(
  mace   = NA,   # MACE (composite)
  mi     = NA,   # myocardial infarction
  stroke = NA,   # stroke
  af     = NA    # atrial fibrillation
)

## -- Dataset metadata ---------------------------------------------------------
# All results are stratified by dataset; never pooled across sites.
databaseId          <- "MarketScan"           # short identifier, no spaces
databaseName        <- "MarketScan aug2025"
databaseDescription <- "MarketScan aug2025"

## -- Output -------------------------------------------------------------------
outputFolder <- file.path("/home/a_kiselev/output", databaseId)
options(andromedaTempFolder = file.path(outputFolder, "andromedaTemp"))
maxCores <- parallel::detectCores()

# ------------------------------------------------------------------------------
# 2. Pipeline control  (set FALSE to skip a step)
# ------------------------------------------------------------------------------
incrementalCohorts         <- TRUE   # TRUE = skip cohorts whose SQL has not changed since last run
includeCohortStats         <- FALSE  # TRUE = export inclusion rule statistics to cohortStatistics/
runCreateCohorts           <- FALSE
runBaselineCharacteristics <- FALSE
runUtilisationAnalysis     <- FALSE   # primary: depot formulation + regimen mix
runSafetyAnalysis          <- TRUE   # KM for MACE/MI/stroke/AF/VTE/DM/fractures (3-year probability)
runCvComparison            <- FALSE   # PS-matched Cox PH: with vs without prior CV history
runSwitchingAnalysis       <- FALSE   # KM for switching to alternative ADT

# ------------------------------------------------------------------------------
# 3. Execute
# ------------------------------------------------------------------------------
execute(
  connectionDetails            = connectionDetails,
  cdmDatabaseSchema            = cdmDatabaseSchema,
  vocabularyDatabaseSchema     = vocabularyDatabaseSchema,
  cohortDatabaseSchema         = cohortDatabaseSchema,
  cohortTable                  = cohortTable,
  databaseId                   = databaseId,
  databaseName                 = databaseName,
  databaseDescription          = databaseDescription,
  outputFolder                 = outputFolder,
  maxCores                     = maxCores,
  incrementalCohorts           = incrementalCohorts,
  includeCohortStats           = includeCohortStats,
  triptorelinCohortId              = triptorelinCohortId,
  combinationTreatmentCohortIds    = combinationTreatmentCohortIds,
  aeCohortIds                      = aeCohortIds,
  outcomeCohortIds                 = outcomeCohortIds,
  minCellCount                 = minCellCount,
  runCreateCohorts             = runCreateCohorts,
  runBaselineCharacteristics   = runBaselineCharacteristics,
  runUtilisationAnalysis       = runUtilisationAnalysis,
  runSafetyAnalysis            = runSafetyAnalysis,
  withCvCohortId               = withCvCohortId,
  withoutCvCohortId            = withoutCvCohortId,
  cvOutcomeCohortIds           = cvOutcomeCohortIds,
  runCvComparison              = runCvComparison,
  runSwitchingAnalysis         = runSwitchingAnalysis
)
