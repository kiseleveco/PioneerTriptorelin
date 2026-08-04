# ==============================================================================
# PioneerTriptorelin — Treatment Patterns via a local DuckDB
# Structured after the darwin-eu/TreatmentPatterns vignette
#   (vignettes/a010_Computing_Pathways.Rmd)
#
# The vignette always builds the cohort table with CDMConnector::generateCohortSet
# (a proper omopgenerics cohort table carrying cohort_set + cohort_attrition
# companions). We already have the cohorts generated upstream in Redshift, so we
# extract them into DuckDB and RE-CREATE that same structure with
# omopgenerics::newCohortTable (cohort_set + cohort_attrition), rather than a bare
# dplyr::tbl() — which TreatmentPatterns cannot build a treatment history from.
#
# A DATA sanity check is printed before computePathways: how many target (1751)
# subjects actually have event-cohort records inside their cohort window. If that
# is ~0, the empty output is a cohort-design issue, not a code issue.
#
# Requires (local):  install.packages(c("duckdb", "DBI", "htmlwidgets"))
# Individual-level rows are written to a local DuckDB file — confirm this is
# permitted under your MarketScan data-governance rules.
# ==============================================================================

library(dplyr)
library(DBI)
library(duckdb)
library(DatabaseConnector)
library(CDMConnector)
library(TreatmentPatterns)

# ------------------------------------------------------------------------------
# 0. Parameters  (edit to match your site / codeToRun.R)
# ------------------------------------------------------------------------------
source("../credentials.r")   # DBMS, USER, PASSWORD, SERVER, DB_PORT

JDBC <- "/home/a_kiselev/Jdbc"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = DBMS, user = USER, password = PASSWORD,
  server = SERVER, port = DB_PORT, pathToDriver = JDBC
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

# Pathway-construction settings (vignette "Full Analysis Example" defaults)
minEraDuration             <- 7
eraCollapseSize            <- 30
combinationWindow          <- 30
minPostCombinationDuration <- 30
filterTreatments           <- "First"
maxPathLength              <- 5
addNoPaths                 <- FALSE

# ------------------------------------------------------------------------------
# Derived
# ------------------------------------------------------------------------------
allIds    <- c(targetCohortId, eventCohortIds[!is.na(eventCohortIds)])
idList    <- paste(allIds, collapse = ", ")
evList    <- paste(eventCohortIds[!is.na(eventCohortIds)], collapse = ", ")
tpFolder  <- file.path(outputFolder, "treatmentPatterns")
csvFolder <- file.path(tpFolder, "output")
duckDir   <- file.path(tpFolder, "duckdb")
duckFile  <- file.path(duckDir, "cdm.duckdb")
dir.create(csvFolder, recursive = TRUE, showWarnings = FALSE)
dir.create(duckDir,   recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Extract the minimal data from Redshift
# ------------------------------------------------------------------------------
con <- DatabaseConnector::connect(connectionDetails)

pull <- function(sql, ...) {
  s  <- SqlRender::render(sql, ...)
  s  <- SqlRender::translate(s, targetDialect = attr(con, "dbms"))
  df <- DatabaseConnector::querySql(con, s)
  colnames(df) <- tolower(colnames(df))
  df
}

message("Extracting cohort rows ...")
cohort <- pull(
  "SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
     FROM @cds.@ct WHERE cohort_definition_id IN (@ids)",
  cds = cohortDatabaseSchema, ct = cohortTable, ids = idList)
message("  ", nrow(cohort), " records / ",
        length(unique(cohort$subject_id)), " subjects.")

subjectSubquery <- sprintf(
  "SELECT DISTINCT subject_id FROM %s.%s WHERE cohort_definition_id IN (%s)",
  cohortDatabaseSchema, cohortTable, idList)

message("Extracting person ...")
person <- pull(
  "SELECT person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth,
          race_concept_id, ethnicity_concept_id
     FROM @cdm.person WHERE person_id IN (@subq)",
  cdm = cdmDatabaseSchema, subq = subjectSubquery)

message("Extracting observation_period ...")
observationPeriod <- pull(
  "SELECT observation_period_id, person_id, observation_period_start_date,
          observation_period_end_date, period_type_concept_id
     FROM @cdm.observation_period WHERE person_id IN (@subq)",
  cdm = cdmDatabaseSchema, subq = subjectSubquery)

# TreatmentPatterns resolves `sex` via an INNER JOIN of cdm$person to cdm$concept
# on gender_concept_id == concept_id. The concept table therefore MUST contain the
# gender concepts, or the join drops every cohort record. Extract them.
message("Extracting gender concepts ...")
genderConcepts <- pull(
  "SELECT concept_id, concept_name, domain_id, vocabulary_id, concept_class_id,
          standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason
     FROM @cdm.concept
    WHERE concept_id IN (SELECT DISTINCT gender_concept_id FROM @cdm.person
                          WHERE person_id IN (@subq))",
  cdm = cdmDatabaseSchema, subq = subjectSubquery)
message("  ", nrow(genderConcepts), " gender concept(s).")

cdmSource <- tryCatch(pull("SELECT * FROM @cdm.cdm_source", cdm = cdmDatabaseSchema),
                      error = function(e) NULL)
if (is.null(cdmSource) || nrow(cdmSource) == 0) {
  cdmSource <- data.frame(
    cdm_source_name = databaseName, cdm_source_abbreviation = databaseId,
    cdm_holder = "NA", source_description = databaseName,
    source_documentation_reference = "NA", cdm_etl_reference = "NA",
    source_release_date = as.Date("2025-01-01"), cdm_release_date = as.Date("2025-01-01"),
    cdm_version = "5.4", cdm_version_concept_id = 0L, vocabulary_version = "NA",
    stringsAsFactors = FALSE)
}
DatabaseConnector::disconnect(con)

# ------------------------------------------------------------------------------
# 1b. Remap subject/person ids to a dense 1..N integer range
#     MarketScan enrolids exceed the 32-bit integer range (max seen ~3.5e10).
#     TreatmentPatterns / Andromeda coerce person ids to 32-bit integers, so any
#     id > 2,147,483,647 overflows to NA and the WHOLE population is silently
#     dropped (every record removed at the first filter). Pathways are aggregate
#     over treatments, so subject identity is irrelevant — remap to 1..N.
# ------------------------------------------------------------------------------
allSubjects <- sort(unique(c(cohort$subject_id, person$person_id,
                             observationPeriod$person_id)))
idMap <- stats::setNames(seq_along(allSubjects), as.character(allSubjects))
remap <- function(x) unname(idMap[as.character(x)])

cohort$subject_id            <- remap(cohort$subject_id)
person$person_id             <- remap(person$person_id)
observationPeriod$person_id  <- remap(observationPeriod$person_id)
cohort$cohort_definition_id  <- as.integer(cohort$cohort_definition_id)
message("Remapped ", length(allSubjects), " subjects to a 1..N integer range ",
        "(max id now ", max(idMap), ").")

# ------------------------------------------------------------------------------
# 2. Build the DuckDB CDM (clinical tables populated; vocab empty for validation)
# ------------------------------------------------------------------------------
if (file.exists(duckFile)) file.remove(duckFile)
duck <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckFile)
on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(duck, "person",             person,            overwrite = TRUE)
DBI::dbWriteTable(duck, "observation_period", observationPeriod, overwrite = TRUE)
DBI::dbWriteTable(duck, "cdm_source",         cdmSource,         overwrite = TRUE)
DBI::dbWriteTable(duck, cohortTable,          cohort,            overwrite = TRUE)

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
for (nm in names(emptyTables)) DBI::dbWriteTable(duck, nm, emptyTables[[nm]], overwrite = TRUE)

# concept MUST hold the gender concepts (TP inner-joins it to resolve sex);
# an empty concept table drops every cohort record.
DBI::dbWriteTable(duck, "concept", genderConcepts, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 3. cohort name lookup + cohorts data.frame (cohortId / cohortName / type)
# ------------------------------------------------------------------------------
cohortsToCreate <- readr::read_csv(
  system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
  col_types = readr::cols())
.cohortName <- function(id) {
  nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
  if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
}
cohorts <- rbind(
  data.frame(cohortId = targetCohortId, cohortName = .cohortName(targetCohortId),
             type = "target", stringsAsFactors = FALSE),
  data.frame(cohortId = eventCohortIds[!is.na(eventCohortIds)],
             cohortName = vapply(eventCohortIds[!is.na(eventCohortIds)], .cohortName, character(1)),
             type = "event", stringsAsFactors = FALSE))
print(cohorts)

# ------------------------------------------------------------------------------
# 3b. DATA sanity check — do target (1751) subjects have event records inside
#     their cohort window? If ~0, empty pathways are a cohort-design issue.
# ------------------------------------------------------------------------------
overlap <- DBI::dbGetQuery(duck, sprintf("
  SELECT e.cohort_definition_id,
         COUNT(DISTINCT e.subject_id) AS n_event_subjects,
         COUNT(DISTINCT CASE WHEN t.subject_id IS NOT NULL THEN e.subject_id END) AS n_shared_with_target,
         COUNT(DISTINCT CASE WHEN t.subject_id IS NOT NULL
                              AND e.cohort_start_date >= t.cohort_start_date
                              AND e.cohort_start_date <= t.cohort_end_date
                             THEN e.subject_id END) AS n_in_target_window
  FROM %s e
  LEFT JOIN (SELECT subject_id, cohort_start_date, cohort_end_date
               FROM %s WHERE cohort_definition_id = %d) t
    ON e.subject_id = t.subject_id
  WHERE e.cohort_definition_id IN (%s)
  GROUP BY e.cohort_definition_id
  ORDER BY e.cohort_definition_id",
  cohortTable, cohortTable, targetCohortId, evList))
message("Event/target overlap (n_in_target_window drives the pathways):")
print(overlap)

# ------------------------------------------------------------------------------
# 4. cdm reference + register cohort2 as a full cohort table (set + attrition),
#    mirroring generateCohortSet() output.
# ------------------------------------------------------------------------------
cdm <- CDMConnector::cdmFromCon(
  con = duck, cdmSchema = "main", writeSchema = "main",
  cdmName = databaseName, .softValidation = TRUE)

cohortSet <- data.frame(
  cohort_definition_id = cohorts$cohortId,
  cohort_name          = cohorts$cohortName,
  stringsAsFactors     = FALSE)

cohortAttrition <- cohort %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::summarise(number_records = dplyr::n(),
                   number_subjects = dplyr::n_distinct(subject_id), .groups = "drop") %>%
  dplyr::mutate(reason_id = 1L, reason = "Initial qualifying events",
                excluded_records = 0L, excluded_subjects = 0L) %>%
  as.data.frame()

DBI::dbWriteTable(duck, paste0(cohortTable, "_set"),       cohortSet,       overwrite = TRUE)
DBI::dbWriteTable(duck, paste0(cohortTable, "_attrition"), cohortAttrition, overwrite = TRUE)

# Assign into the cdm first (so each becomes an omopgenerics cdm_table), then wrap
cdm[[cohortTable]]                       <- dplyr::tbl(duck, cohortTable)
cdm[[paste0(cohortTable, "_set")]]       <- dplyr::tbl(duck, paste0(cohortTable, "_set"))
cdm[[paste0(cohortTable, "_attrition")]] <- dplyr::tbl(duck, paste0(cohortTable, "_attrition"))
cdm[[cohortTable]] <- omopgenerics::newCohortTable(
  table              = cdm[[cohortTable]],
  cohortSetRef       = cdm[[paste0(cohortTable, "_set")]],
  cohortAttritionRef = cdm[[paste0(cohortTable, "_attrition")]],
  .softValidation    = TRUE)

# ------------------------------------------------------------------------------
# 5. Compute pathways + export  (vignette structure)
# ------------------------------------------------------------------------------
message("Computing pathways on DuckDB ...")
outputEnv <- TreatmentPatterns::computePathways(
  cohorts                    = cohorts,
  cohortTableName            = cohortTable,
  cdm                        = cdm,
  analysisId                 = 1,
  description                = databaseName,
  startAnchor                = "startDate",
  windowStart                = 0,
  endAnchor                  = "endDate",
  windowEnd                  = 0,
  minEraDuration             = minEraDuration,
  eraCollapseSize            = eraCollapseSize,
  combinationWindow          = combinationWindow,
  minPostCombinationDuration = minPostCombinationDuration,
  filterTreatments           = filterTreatments,
  maxPathLength              = maxPathLength
)

# ------------------------------------------------------------------------------
# DEBUG: dump the Andromeda intermediate tables to see where records vanish
# ------------------------------------------------------------------------------
message("=== Andromeda tables after computePathways ===")
for (nm in names(outputEnv)) {
  n <- tryCatch(outputEnv[[nm]] %>% dplyr::tally() %>% dplyr::pull(1),
                error = function(e) NA)
  message(sprintf("  %-28s %s rows", nm, n))
}
for (nm in names(outputEnv)) {
  cat("\n--- ", nm, " (head) ---\n")
  print(tryCatch(outputEnv[[nm]] %>% head(5) %>% dplyr::collect(),
                 error = function(e) conditionMessage(e)))
}

results <- TreatmentPatterns::export(
  andromeda    = outputEnv,
  outputPath   = csvFolder,
  minCellCount = minCellCount,
  censorType   = "minCellCount",
  nonePaths    = addNoPaths)
message("Pathways rows: ", nrow(results$treatment_pathways),
        "  |  CSVs written to ", csvFolder)

# ------------------------------------------------------------------------------
# 6. Save interactive sunburst + Sankey as standalone HTML (best effort)
# ------------------------------------------------------------------------------
if (requireNamespace("htmlwidgets", quietly = TRUE) &&
    nrow(results$treatment_pathways) > 0) {
  tryCatch({
    htmlwidgets::saveWidget(results$plotSunburst(),
                            file.path(tpFolder, "sunburst.html"), selfcontained = TRUE)
    htmlwidgets::saveWidget(results$plotSankey(),
                            file.path(tpFolder, "sankey.html"),   selfcontained = TRUE)
    message("Saved sunburst.html and sankey.html to ", tpFolder)
  }, error = function(e) message("Could not save plots: ", conditionMessage(e)))
}

try(Andromeda::close(outputEnv), silent = TRUE)
message("Done.")
