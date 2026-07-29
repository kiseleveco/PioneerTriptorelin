-- Time-to-event data for EPISODIC outcomes, for Kaplan-Meier.
-- One row per target-cohort subject (time to the FIRST post-index event):
--   time_to_event : days from cohort entry to the first outcome on/after entry,
--                   or to end of follow-up if no event.
--   event         : 1 = outcome occurred in the window, 0 = censored.
--
-- Prevalent cases are NOT excluded (recurrent outcomes): a person with a prior
-- event still contributes their first ON/AFTER-index event.  The first-event
-- subquery is restricted to events on/after cohort entry, so a pre-index event
-- cannot mask a genuine post-index one.
--
-- Censoring: at the earlier of the observation-period end and the cohort end
-- date, for subjects who do not experience the outcome.
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
    -- First outcome ON/AFTER each subject's cohort entry
    SELECT o.subject_id, MIN(o.cohort_start_date) AS outcome_date
    FROM @cohort_database_schema.@cohort_table o
    INNER JOIN @cohort_database_schema.@cohort_table tt
      ON  o.subject_id            = tt.subject_id
      AND tt.cohort_definition_id = @target_cohort_id
      AND o.cohort_start_date    >= tt.cohort_start_date
    WHERE o.cohort_definition_id = @outcome_cohort_id
    GROUP BY o.subject_id
) o
  ON t.subject_id   = o.subject_id
  AND o.outcome_date <=
        CASE WHEN op.observation_period_end_date < t.cohort_end_date
             THEN op.observation_period_end_date
             ELSE t.cohort_end_date END
WHERE t.cohort_definition_id = @target_cohort_id
;
