-- Time-to-event data for CHRONIC (new-onset) outcomes, for Kaplan-Meier.
-- One row per target-cohort subject:
--   time_to_event : days from cohort entry to the FIRST outcome on/after entry,
--                   or to end of follow-up if no event.
--   event         : 1 = outcome occurred in the window, 0 = censored.
--
-- Prevalent cases are EXCLUDED: any subject whose outcome occurred strictly
-- before target cohort entry is dropped, so survival is estimated among the
-- outcome-naive population at baseline (incident disease only).  An outcome on
-- the index date itself is counted as an incident event (time_to_event = 0).
--
-- Censoring: at the earlier of the observation-period end and the cohort end
-- date, for subjects who do not experience the outcome.  Outcomes occurring
-- after that date are censored, not counted as events.
--
-- Parameters (SqlRender):
--   @cohort_database_schema  schema containing the cohort table
--   @cdm_database_schema     schema containing the OMOP CDM (observation_period)
--   @cohort_table            cohort table name (base name only)
--   @target_cohort_id        cohort_definition_id of the target (exposure) cohort
--   @outcome_cohort_id       cohort_definition_id of the outcome cohort

SELECT
  DATEDIFF(day, t.cohort_start_date,
    CASE
      WHEN o.outcome_date IS NOT NULL
        THEN o.outcome_date
      ELSE CASE WHEN op.observation_period_end_date < t.cohort_end_date
                THEN op.observation_period_end_date
                ELSE t.cohort_end_date END
    END) AS time_to_event,
  CASE WHEN o.outcome_date IS NOT NULL THEN 1 ELSE 0 END AS event
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
    WHERE  cohort_definition_id = @outcome_cohort_id
    GROUP BY subject_id
) o
  ON t.subject_id    = o.subject_id
  AND o.outcome_date >= t.cohort_start_date
  AND o.outcome_date <=
        CASE WHEN op.observation_period_end_date < t.cohort_end_date
             THEN op.observation_period_end_date
             ELSE t.cohort_end_date END
WHERE t.cohort_definition_id = @target_cohort_id
  -- Exclude prevalent cases: outcome occurred BEFORE cohort entry
  AND NOT EXISTS (
      SELECT 1
      FROM   @cohort_database_schema.@cohort_table pre
      WHERE  pre.subject_id           = t.subject_id
        AND  pre.cohort_definition_id = @outcome_cohort_id
        AND  pre.cohort_start_date    < t.cohort_start_date
  )
;
