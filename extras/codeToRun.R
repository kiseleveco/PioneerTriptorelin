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

JDBC <- "/home/a_kiselev/Jdbc"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms        = DBMS,
  user        = USER,
  password    = PASSWORD,
  server      = SERVER,
  port        = DB_PORT,
  pathToDriver = JDBC
)

## -- Schema names -------------------------------------------------------------
cdmDatabaseSchema        <- "marketscan_ccaemdcr_aug2025"# read-only CDM
vocabularyDatabaseSchema <- cdmDatabaseSchema     # usually same as CDM
cohortDatabaseSchema     <- "marketscan_ccaemdcr_aug2025_results"        # write-enabled; needs read/write/delete
cohortTable              <- "cohort2"

## -- Temp table emulation (Oracle / some Spark configs) -----------------------
options(sqlRenderTempEmulationSchema = NULL)

## -- Cohort IDs ---------------------------------------------------------------
triptorelinCohortId <- 1751   # cohort definition ID of the main triptorelin cohort

# Combination treatment cohorts for utilisation analysis (fill in once finalised in Atlas)
# IDs only — cohort names are resolved automatically from CohortsToCreate.csv
combinationTreatmentCohortIds <- c(
  1755,   # Triptorelin only
  1752,     # Triptorelin + ARPI
  1753,     # Triptorelin + Chemo
  1754      # Triple
)

# Adverse event cohorts for incidence rate analysis (fill in once finalised in Atlas)
# IDs only — names come from CohortsToCreate.csv; episodic/chronic from IRsettings.csv
outcomeCohortIds <- c(1688, 1689, 1690, 1691)

minCellCount      <- 5    # suppress KM rows / cells with fewer events than this

## -- Dataset metadata ---------------------------------------------------------
# All results are stratified by dataset; never pooled across sites.
databaseId          <- "MarketScan"           # short identifier, no spaces
databaseName        <- "MarketScan aug2025"
databaseDescription <- "MarketScan aug2025"

## -- Output -------------------------------------------------------------------
outputFolder <- file.path("/home/a_kiselev/output", paste0(databaseId, "_triptorelin"))
options(andromedaTempFolder = file.path(outputFolder, "andromedaTemp"))
maxCores <- parallel::detectCores()

# ------------------------------------------------------------------------------
# 2. Pipeline control  (set FALSE to skip a step)
# ------------------------------------------------------------------------------
incrementalCohorts         <- TRUE   # TRUE = skip cohorts whose SQL has not changed since last run
includeCohortStats         <- TRUE  # TRUE = export inclusion rule statistics to cohortStatistics/
runCreateCohorts           <- TRUE
runBaselineCharacteristics <- TRUE
runUtilisationAnalysis     <- TRUE   # primary: depot formulation + regimen mix
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
