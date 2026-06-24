-- Incidence rate data for EPISODIC (recurrent) outcomes.
-- Time-at-risk spans the full cohort period (cohort_start_date to
-- cohort_end_date).  ALL occurrences of the outcome within that window are
-- counted, not just the first.
--
-- Parameters (SqlRender):
--   @cohort_database_schema  schema containing the cohort table
--   @cohort_table            cohort table name (base name only)
--   @target_id               cohort_definition_id of the target (exposure) cohort
--   @outcome_id              cohort_definition_id of the outcome cohort

SELECT
    COUNT(DISTINCT t.subject_id)                                                AS n_persons,
    COUNT(o.subject_id)                                                         AS n_events,
    SUM(CAST(DATEDIFF(day, t.cohort_start_date, t.cohort_end_date) AS FLOAT))
        / 365.25                                                                AS person_years
FROM @cohort_database_schema.@cohort_table t
LEFT JOIN @cohort_database_schema.@cohort_table o
    ON  t.subject_id              = o.subject_id
    AND o.cohort_definition_id    = @outcome_id
    AND o.cohort_start_date      >= t.cohort_start_date
    AND o.cohort_start_date      <= t.cohort_end_date
WHERE t.cohort_definition_id = @target_id
;
