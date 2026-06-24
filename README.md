# PioneerTriptorelin

Real-world data study of triptorelin utilisation and safety in metastatic hormone-sensitive prostate cancer (mHSPC). Built on the OHDSI HADES stack and designed for federated execution across OMOP CDM databases.

---

## Prerequisites

### Software

| Requirement | Version |
|---|---|
| R | ≥ 4.5.2 |
| Java (for DatabaseConnector JDBC) | ≥ 11 |
| JDBC driver for your database | matching your DBMS |

R package dependencies are managed via `renv`. Run `renv::restore()` once after cloning the repository to install all required packages at the locked versions.

### Database

- An **OMOP CDM v5.x** database with read access
- A **write-enabled results schema** for cohort tables (`cohortDatabaseSchema`)
- The CDM must contain: `person`, `observation_period`, `drug_exposure`, `condition_occurrence`, `procedure_occurrence`, `measurement`, `visit_occurrence`

### Atlas / WebAPI

Cohort definitions are developed in Atlas and exported to the package via `inst/settings/insertCohortDefinition.R`. Before running the study, ensure all cohort JSON and SQL files are present in:

- `inst/cohorts/` — one `.json` file per cohort
- `inst/sql/sql_server/` — one `.sql` file per cohort

---

## Setup

1. Clone the repository and open `PioneerTriptorelin.Rproj` in RStudio
2. Run `renv::restore()` to install dependencies
3. Open `extras/codeToRun.R`
4. Fill in all site-specific parameters in **Section 1** (connection details, schema names, cohort IDs)
5. Set pipeline step flags in **Section 2**
6. Source the file or run it section by section

Credentials should be stored in a separate file (e.g. `../credentials.r`) and sourced at the top of `codeToRun.R`. Never commit credentials to the repository.

---

## Cohort ID Reference

All cohort IDs must be filled in `extras/codeToRun.R` before running the relevant step.

| Parameter | Description |
|---|---|
| `triptorelinCohortId` | Main triptorelin cohort: men with mHSPC initiating triptorelin as first ADT |
| `outcomeCohortIds["mace"]` | MACE composite (safety analysis) |
| `outcomeCohortIds["mi"]` | Myocardial infarction |
| `outcomeCohortIds["stroke"]` | Stroke |
| `outcomeCohortIds["af"]` | Atrial fibrillation |
| `outcomeCohortIds["vte"]` | Venous thromboembolism |
| `outcomeCohortIds["diabetes"]` | Diabetes mellitus |
| `outcomeCohortIds["fractures"]` | Fractures |
| `cvHistoryCohortId` | Prior CV disease history (safety analysis covariate) |
| `withCvCohortId` | Triptorelin patients WITH prior CV history (CV comparison target) |
| `withoutCvCohortId` | Triptorelin patients WITHOUT prior CV history (CV comparison comparator) |
| `cvOutcomeCohortIds["mace/mi/stroke/af"]` | CV outcomes for the CohortMethod comparison step |

---

## Analysis Steps

### Step 1 — Cohort Creation (`runCreateCohorts`)

**What it does**
Instantiates all study cohorts in `cohortDatabaseSchema.cohortTable` using the SQL files in `inst/sql/sql_server/`. Optionally skips already-generated cohorts when `incrementalCohorts = TRUE`.

**Requires**
- All cohort SQL files present in `inst/sql/sql_server/`
- Write access to `cohortDatabaseSchema`

**Outputs** (in `outputFolder/`)
- `cohortCounts.csv` — row counts per cohort

**Share with coordinator:** `cohortCounts.csv`

---

### Step 2 — Cohort Diagnostics (`runCohortDiagnostics`)

**What it does**
Runs `CohortDiagnostics::executeDiagnostics()` to evaluate cohort validity: inclusion rule statistics, index event breakdown, visit context, cohort overlap, and temporal characterisation.

**Requires**
- Step 1 completed
- Cohort JSON files in `inst/cohorts/`

**Outputs** (in `outputFolder/`)
- A zip file per site compatible with `CohortDiagnostics::launchDiagnosticsExplorer()`

**Share with coordinator:** the zip file

**Viewing results**
```r
CohortDiagnostics::launchDiagnosticsExplorer(
  dataFolder = "output/SiteABC/"
)
```

---

### Step 3 — Baseline Characterisation (`runBaselineCharacteristics`)

**What it does**
Runs temporal cohort characterisation via `CohortDiagnostics::executeDiagnostics()` for the main triptorelin cohort. Extracts a broad set of temporal covariates across multiple time windows (anytime prior, long-term, medium-term, short-term, index date, post-index) covering:

- Demographics: age, age group, sex, race, ethnicity, index year/month
- Conditions: occurrence, era start, era overlap, era group overlap
- Drug era start and drug era group overlap (concurrent medications at index — captures ARPI, chemotherapy, ADT combinations)
- Procedures, measurements, device exposures, observations, visit counts
- Comorbidity indices: Charlson Comorbidity Index (CCI), DCSI, CHADS2, CHADS2-VASc

**Requires**
- Step 1 completed
- `triptorelinCohortId` set in `codeToRun.R`
- Cohort JSON and SQL files for the triptorelin cohort

**Outputs** (in `outputFolder/baselineCharacteristics/`)
- Zip file compatible with `CohortDiagnostics::launchDiagnosticsExplorer()`

**Share with coordinator:** the zip file

---

### Step 4 — Utilisation Analysis (`runUtilisationAnalysis`)

**What it does**
Describes real-world triptorelin use at the index date:
- Depot formulation and dosing interval (1-month/3.75 mg, 3-month/11.25 mg, 6-month/22.5 mg)
- Combination regimen (ADT alone, ADT + ARPI, ADT + chemotherapy, ADT + chemotherapy + ARPI)

*Not yet implemented.*

---

### Step 5 — Safety Analysis (`runSafetyAnalysis`)

**What it does**
Estimates the 3-year cumulative incidence of seven safety outcomes using Kaplan-Meier survival analysis. Patients are censored at last clinical visit or end of data availability. Outcomes with fewer than `minCellCount` events at a given time point are suppressed in the output.

| Outcome | Method |
|---|---|
| MACE, MI, stroke, AF | Kaplan-Meier |
| VTE | Kaplan-Meier |
| Diabetes | Kaplan-Meier |
| Fractures | Kaplan-Meier |

**Requires**
- Step 1 completed
- `triptorelinCohortId` and all `outcomeCohortIds` set in `codeToRun.R`
- `cvHistoryCohortId` set (used as a join to extract CV history flag for the data pull)

**Outputs** (in `outputFolder/safetyAnalysis/`)
- `kmTable.csv` — full KM survival table per outcome (time, n.risk, n.event, n.censor, survival, 95% CI); rows below `minCellCount` suppressed
- `km3YearEstimates.csv` — 3-year cumulative incidence point estimate and 95% CI per outcome
- `coxResults.csv` — *(placeholder; Cox modelling is handled in Step 6)*

**Share with coordinator:** all files in `safetyAnalysis/`

---

### Step 6 — CV Comparison: With vs Without Prior CV History (`runCvComparison`)

**What it does**
Compares the risk of CV outcomes between triptorelin patients with prior cardiovascular disease history (target) and those without (comparator), using `CohortMethod::runCmAnalyses()`:

1. **Propensity score model** — large-scale regularised logistic regression (`Cyclops`) on default covariates to balance the two groups
2. **1:1 PS matching** — caliper 0.2 on the standardised logit scale
3. **Stratified Cox PH** — fitted in the matched population, conditioned on matched pairs

Outcomes: MACE, MI, stroke, AF.

> **Important:** Intermediate objects produced by `runCmAnalyses()` (CmData, PS models, matched populations) are written to `outputFolder/cvComparison/cm/` and contain individual-level data. This folder must remain at the site and must **not** be shared.

**Requires**
- Step 1 completed
- `withCvCohortId`, `withoutCvCohortId`, and `cvOutcomeCohortIds` set in `codeToRun.R`

**Outputs**

*Site-only (individual-level — do not share):*
- `outputFolder/cvComparison/cm/` — intermediate CohortMethod objects

*For sharing (aggregated):*
- `outputFolder/cvComparison/export/cm_result.csv` — HR, 95% CI, p-value per outcome
- `outputFolder/cvComparison/export/cm_kaplan_meier_dist.csv` — KM curves in matched population (for plotting)
- `outputFolder/cvComparison/export/cm_attrition.csv` — patient counts at each step
- `outputFolder/cvComparison/export/cm_ps_dist.csv` — binned PS distributions
- `outputFolder/cvComparison/export/cm_covariate_balance.csv` — standardised mean differences before/after matching (for love plots)
- `outputFolder/cvComparison/export/cm_diagnostics_summary.csv` — balance and equipoise diagnostics
- Additional standard CohortMethod export files

**Share with coordinator:** all files in `outputFolder/cvComparison/export/`

**Viewing results**
Results are compatible with `OhdsiShinyModules::cohortMethodModule()`. To view locally:
```r
# Upload export CSVs to a SQLite results database, then launch Shiny
# (see OhdsiShinyModules and ShinyAppBuilder documentation)
```

---

### Step 7 — Treatment Switching Analysis (`runSwitchingAnalysis`)

**What it does**
Estimates the 3-year probability of switching from triptorelin to an alternative castration therapy using Kaplan-Meier survival analysis.

Alternative agents: Relugolix, Degarelix, Leuprolide, Goserelin, Buserelin, surgical castration.

*Not yet implemented.*

---

## Output Sharing Summary

| Folder / File | Share with coordinator |
|---|---|
| `cohortCounts.csv` | Yes |
| `*.zip` (cohort diagnostics) | Yes |
| `baselineCharacteristics/*.zip` | Yes |
| `safetyAnalysis/` | Yes |
| `cvComparison/export/` | Yes |
| `cvComparison/cm/` | **No — individual-level data, site-only** |

---

## Study Contact

Andrei Kiselev — andrei.kiselev@bdmconsulting.fr
