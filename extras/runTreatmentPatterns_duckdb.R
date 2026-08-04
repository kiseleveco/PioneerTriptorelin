# ==============================================================================
# PioneerTriptorelin — Treatment Patterns via a local DuckDB (Redshift workaround)
#
# TreatmentPatterns 3.1.2 runs through CDMConnector, whose temp-table /
# materialization layer is unreliable against this Redshift (native #temp throws
# java.util.NoSuchElementException; emulation lets it start but rows do not
# survive the temp -> Andromeda round-trip, so every era is dropped at
# minEraDuration = 0). The data and the SQL date math are correct — only the
# Redshift plumbing is broken.
#
# This script sidesteps Redshift entirely: it pulls the minimal data
# TreatmentPatterns needs (the target + event cohorts, plus person and
# observation_period for those subjects) into a LOCAL DuckDB file, builds the
# cdm from DuckDB (whose temp/date handling is correct), and runs
# computePathways() + export() there.
#
# Individual-level rows are written to a local DuckDB file — confirm this is
# permitted under your MarketScan data-governance rules before running.
#
# Requires (install locally if missing):
#   install.packages(c("duckdb", "DBI", "htmlwidgets"))
# ==============================================================================

library(DatabaseConnector)
library(CDMConnector)
library(TreatmentPatterns)
library(dplyr)

# ------------------------------------------------------------------------------
# 0. Parameters  (edit to match your site / codeToRun.R)
# ------------------------------------------------------------------------------
source("../credentials.r")   # DBMS, USER, PASSWORD, SERVER, DB_PORT

JDBC <- "/home/a_kiselev/Jdbc"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms         = DBMS,
  user         = USER,
  password     = PASSWORD,
  server       = SERVER,
  port         = DB_PORT,
  pathToDriver = JDBC
)

cdmDatabaseSchema    <- "marketscan_ccaemdcr_aug2025"
cohortDatabaseSchema <- "marketscan_ccaemdcr_aug2025_results"
cohortTable          <- "cohort2"

targetCohortId <- 1751
eventCohortIds <- c(1758, 1759, 1760, 1761, 1762, 1763)

databaseId   <- "MarketScan"
databaseName <- "MarketScan aug2025"
minCellCount <- 5

outputFolder <- file.path("/home/a_kiselev/output", paste0(databaseId, "_triptorelin"))

# Pathway-construction settings (see runTreatmentPatterns.R for meanings)
minEraDuration             <- 0
eraCollapseSize            <- 30
combinationWindow          <- 30
minPostCombinationDuration <- 30
filterTreatments           <- "First"
maxPathLength              <- 5
addNoPaths                 <- FALSE

# ------------------------------------------------------------------------------
# Derived
# ------------------------------------------------------------------------------
allIds  <- c(targetCohortId, eventCohortIds[!is.na(eventCohortIds)])
idList  <- paste(allIds, collapse = ", ")
tpFolder  <- file.path(outputFolder, "treatmentPatterns")
csvFolder <- file.path(tpFolder, "output")
duckDir   <- file.path(tpFolder, "duckdb")
duckFile  <- file.path(duckDir, "cdm.duckdb")
dir.create(csvFolder, recursive = TRUE, showWarnings = FALSE)
dir.create(duckDir,   recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Extract the minimal data from Redshift
#    - cohort rows for the 7 cohorts
#    - person + observation_period for those subjects (via in-DB subquery, so no
#      giant IN-list is sent from R)
#    - cdm_source (for the CDM name/version)
# ------------------------------------------------------------------------------
con <- DatabaseConnector::connect(connectionDetails)

pull <- function(sql, ...) {
  s  <- SqlRender::render(sql, ...)
  s  <- SqlRender::translate(s, targetDialect = attr(con, "dbms"))
  df <- DatabaseConnector::querySql(con, s)
  colnames(df) <- tolower(colnames(df))    # OMOP tables use snake_case
  df
}

message("Extracting cohort rows ...")
cohort <- pull(
  "SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
     FROM @cds.@ct
    WHERE cohort_definition_id IN (@ids)",
  cds = cohortDatabaseSchema, ct = cohortTable, ids = idList)
message("  ", nrow(cohort), " cohort records for ",
        length(unique(cohort$subject_id)), " subjects.")

subjectSubquery <- sprintf(
  "SELECT DISTINCT subject_id FROM %s.%s WHERE cohort_definition_id IN (%s)",
  cohortDatabaseSchema, cohortTable, idList)

message("Extracting person ...")
person <- pull(
  "SELECT person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth,
          birth_datetime, race_concept_id, ethnicity_concept_id
     FROM @cdm.person
    WHERE person_id IN (@subq)",
  cdm = cdmDatabaseSchema, subq = subjectSubquery)

message("Extracting observation_period ...")
observationPeriod <- pull(
  "SELECT observation_period_id, person_id, observation_period_start_date,
          observation_period_end_date, period_type_concept_id
     FROM @cdm.observation_period
    WHERE person_id IN (@subq)",
  cdm = cdmDatabaseSchema, subq = subjectSubquery)

cdmSource <- tryCatch(
  pull("SELECT * FROM @cdm.cdm_source", cdm = cdmDatabaseSchema),
  error = function(e) NULL)
if (is.null(cdmSource) || nrow(cdmSource) == 0) {
  cdmSource <- data.frame(
    cdm_source_name                = databaseName,
    cdm_source_abbreviation        = databaseId,
    cdm_holder                     = "NA",
    source_description             = databaseName,
    source_documentation_reference = "NA",
    cdm_etl_reference              = "NA",
    source_release_date            = as.Date("2025-01-01"),
    cdm_release_date               = as.Date("2025-01-01"),
    cdm_version                    = "5.4",
    cdm_version_concept_id         = 0L,
    vocabulary_version             = "NA",
    stringsAsFactors               = FALSE
  )
}

DatabaseConnector::disconnect(con)

# ------------------------------------------------------------------------------
# 2. Build a local DuckDB CDM
#    Clinical tables that TreatmentPatterns actually uses (cohort, person,
#    observation_period, cdm_source) are populated; vocabulary tables are created
#    EMPTY only to satisfy CDM validation (pathway construction does not read
#    them).
# ------------------------------------------------------------------------------
if (file.exists(duckFile)) file.remove(duckFile)
duck <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckFile)
on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(duck, "person",             person,            overwrite = TRUE)
DBI::dbWriteTable(duck, "observation_period", observationPeriod, overwrite = TRUE)
DBI::dbWriteTable(duck, "cdm_source",         cdmSource,         overwrite = TRUE)
DBI::dbWriteTable(duck, cohortTable,          cohort,            overwrite = TRUE)

# Empty vocabulary tables (0 rows, correct columns) for CDM validation
emptyTables <- list(
  concept = data.frame(concept_id = integer(), concept_name = character(),
    domain_id = character(), vocabulary_id = character(), concept_class_id = character(),
    standard_concept = character(), concept_code = character(),
    valid_start_date = as.Date(character()), valid_end_date = as.Date(character()),
    invalid_reason = character()),
  vocabulary = data.frame(vocabulary_id = character(), vocabulary_name = character(),
    vocabulary_reference = character(), vocabulary_version = character(),
    vocabulary_concept_id = integer()),
  domain = data.frame(domain_id = character(), domain_name = character(),
    domain_concept_id = integer()),
  concept_class = data.frame(concept_class_id = character(), concept_class_name = character(),
    concept_class_concept_id = integer()),
  concept_relationship = data.frame(concept_id_1 = integer(), concept_id_2 = integer(),
    relationship_id = character(), valid_start_date = as.Date(character()),
    valid_end_date = as.Date(character()), invalid_reason = character()),
  relationship = data.frame(relationship_id = character(), relationship_name = character(),
    is_hierarchical = character(), defines_ancestry = character(),
    reverse_relationship_id = character(), relationship_concept_id = integer()),
  concept_synonym = data.frame(concept_id = integer(), concept_synonym_name = character(),
    language_concept_id = integer()),
  concept_ancestor = data.frame(ancestor_concept_id = integer(),
    descendant_concept_id = integer(), min_levels_of_separation = integer(),
    max_levels_of_separation = integer()),
  drug_strength = data.frame(drug_concept_id = integer(), ingredient_concept_id = integer(),
    amount_value = numeric(), amount_unit_concept_id = integer(),
    numerator_value = numeric(), numerator_unit_concept_id = integer(),
    denominator_value = numeric(), denominator_unit_concept_id = integer(),
    box_size = integer(), valid_start_date = as.Date(character()),
    valid_end_date = as.Date(character()), invalid_reason = character())
)
for (nm in names(emptyTables)) {
  DBI::dbWriteTable(duck, nm, emptyTables[[nm]], overwrite = TRUE)
}

# ------------------------------------------------------------------------------
# 3. cdm reference from DuckDB + register the cohort table
# ------------------------------------------------------------------------------
cdm <- CDMConnector::cdmFromCon(
  con            = duck,
  cdmSchema      = "main",
  writeSchema    = "main",
  cdmName        = databaseName,
  .softValidation = TRUE
)

# ------------------------------------------------------------------------------
# 4. cohorts table for computePathways (type = target / event)
# ------------------------------------------------------------------------------
cohortsToCreate <- readr::read_csv(
  system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
  col_types = readr::cols()
)
.cohortName <- function(id) {
  nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
  if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
}
cohorts <- rbind(
  data.frame(cohortId = targetCohortId,
             cohortName = .cohortName(targetCohortId),
             type = "target", stringsAsFactors = FALSE),
  data.frame(cohortId = eventCohortIds[!is.na(eventCohortIds)],
             cohortName = vapply(eventCohortIds[!is.na(eventCohortIds)], .cohortName, character(1)),
             type = "event", stringsAsFactors = FALSE)
)

# Register cohort2 as a PROPER omopgenerics cohort table (a plain dplyr::tbl
# lacks the cohort_set metadata that computePathways needs to build the
# treatment history — the working example uses generateCohortSet() for this).
DBI::dbWriteTable(
  duck, paste0(cohortTable, "_set"),
  data.frame(cohort_definition_id = cohorts$cohortId,
             cohort_name          = cohorts$cohortName,
             stringsAsFactors     = FALSE),
  overwrite = TRUE
)
cdm[[cohortTable]] <- omopgenerics::newCohortTable(
  table          = dplyr::tbl(duck, cohortTable),
  cohortSetRef   = dplyr::tbl(duck, paste0(cohortTable, "_set")),
  .softValidation = TRUE
)

# ------------------------------------------------------------------------------
# 5. Construct pathways + export
# ------------------------------------------------------------------------------
message("Computing pathways on DuckDB ...")
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

results <- TreatmentPatterns::export(
  andromeda    = andromeda,
  outputPath   = csvFolder,
  minCellCount = minCellCount,
  censorType   = "minCellCount",
  nonePaths    = addNoPaths
)
message("CSVs written to ", csvFolder)

# ------------------------------------------------------------------------------
# 6. Save interactive sunburst + Sankey as standalone HTML (best effort)
# ------------------------------------------------------------------------------
if (requireNamespace("htmlwidgets", quietly = TRUE)) {
  tryCatch({
    htmlwidgets::saveWidget(results$plotSunburst(),
                            file.path(tpFolder, "sunburst.html"), selfcontained = TRUE)
    htmlwidgets::saveWidget(results$plotSankey(),
                            file.path(tpFolder, "sankey.html"),   selfcontained = TRUE)
    message("Saved sunburst.html and sankey.html to ", tpFolder)
  }, error = function(e) message("Could not save plots: ", conditionMessage(e)))
}

try(Andromeda::close(andromeda), silent = TRUE)
message("Done. Explore interactively: results$plotSankey() / results$plotSunburst()")
