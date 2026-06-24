-- Incidence rate data for CHRONIC outcomes.
-- Time-at-risk (TAR) ends at the first occurrence of the outcome or at
-- cohort end date, whichever comes first.  Each person contributes at most
-- one event.
--
-- Parameters (SqlRender):
--   @cohort_database_schema  schema containing the cohort table
--   @cohort_table            cohort table name (base name only)
--   @target_id               cohort_definition_id of the target (exposure) cohort
--   @outcome_id              cohort_definition_id of the outcome cohort

SELECT
    COUNT(x.subject_id)               AS n_persons,
    COALESCE(SUM(x.had_event), 0)     AS n_events,
    SUM(CAST(x.time_at_risk_days AS FLOAT)) / 365.25 AS person_years
FROM (
    SELECT
        t.subject_id,
        CASE WHEN o.outcome_date IS NOT NULL THEN 1 ELSE 0 END AS had_event,
        DATEDIFF(day, t.cohort_start_date,
            COALESCE(o.outcome_date, t.cohort_end_date)) AS time_at_risk_days
    FROM @cohort_database_schema.@cohort_table t
    LEFT JOIN (
        -- First occurrence of the outcome per person
        SELECT subject_id, MIN(cohort_start_date) AS outcome_date
        FROM   @cohort_database_schema.@cohort_table
        WHERE  cohort_definition_id = @outcome_id
        GROUP BY subject_id
    ) o ON  t.subject_id        = o.subject_id
        AND o.outcome_date      >= t.cohort_start_date
        AND o.outcome_date      <= t.cohort_end_date
    WHERE t.cohort_definition_id = @target_id
) x
;
