-- Incidence rate data for CHRONIC (new-onset) outcomes.
-- Time-at-risk (TAR) ends at the first occurrence of the outcome or at the
-- earlier of the observation-period end and the cohort end date, whichever
-- comes first.  Each person contributes at most one event.
--
-- Prevalent cases are EXCLUDED: any subject whose outcome occurred strictly
-- before target cohort entry is dropped, so the rate reflects NEW-ONSET
-- disease among the outcome-naive population at baseline.  An outcome on the
-- index date itself is counted as an incident event (TAR = 0 days).
--
-- Parameters (SqlRender):
--   @cohort_database_schema  schema containing the cohort table
--   @cdm_database_schema     schema containing the OMOP CDM (observation_period)
--   @cohort_table            cohort table name (base name only)
--   @target_id               cohort_definition_id of the target (exposure) cohort
--   @outcome_id              cohort_definition_id of the outcome cohort

SELECT
    COUNT(x.subject_id)                              AS n_persons,
    COALESCE(SUM(x.had_event), 0)                    AS n_events,
    SUM(CAST(x.time_at_risk_days AS FLOAT)) / 365.25 AS person_years
FROM (
    SELECT
        t.subject_id,
        CASE WHEN o.outcome_date IS NOT NULL THEN 1 ELSE 0 END AS had_event,
        DATEDIFF(day, t.cohort_start_date,
            COALESCE(o.outcome_date,
                CASE WHEN op.observation_period_end_date < t.cohort_end_date
                     THEN op.observation_period_end_date
                     ELSE t.cohort_end_date END))                      AS time_at_risk_days
    FROM @cohort_database_schema.@cohort_table t
    INNER JOIN @cdm_database_schema.observation_period op
        ON  t.subject_id                     = op.person_id
        AND op.observation_period_start_date <= t.cohort_start_date
        AND op.observation_period_end_date   >= t.cohort_start_date
    LEFT JOIN (
        -- First occurrence of the outcome per person (prevalent cases are
        -- excluded below, so this is the first ON/AFTER cohort entry)
        SELECT subject_id, MIN(cohort_start_date) AS outcome_date
        FROM   @cohort_database_schema.@cohort_table
        WHERE  cohort_definition_id = @outcome_id
        GROUP BY subject_id
    ) o ON  t.subject_id        = o.subject_id
        AND o.outcome_date     >= t.cohort_start_date
        AND o.outcome_date     <=
              CASE WHEN op.observation_period_end_date < t.cohort_end_date
                   THEN op.observation_period_end_date
                   ELSE t.cohort_end_date END
    WHERE t.cohort_definition_id = @target_id
      -- Exclude prevalent cases: outcome occurred BEFORE cohort entry
      AND NOT EXISTS (
          SELECT 1
          FROM   @cohort_database_schema.@cohort_table pre
          WHERE  pre.subject_id           = t.subject_id
            AND  pre.cohort_definition_id = @outcome_id
            AND  pre.cohort_start_date    < t.cohort_start_date
      )
) x
;
