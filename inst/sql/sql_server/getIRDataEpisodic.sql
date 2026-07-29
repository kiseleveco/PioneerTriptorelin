-- Incidence rate data for EPISODIC (recurrent) outcomes.
-- Time-at-risk runs from cohort entry to the earlier of the observation-period
-- end and the cohort end date.  ALL occurrences of the outcome within that
-- window are counted, not just the first.  Prevalent cases are NOT excluded:
-- for recurrent outcomes a prior event does not remove a person from risk.
--
-- Person-years are computed ONCE per person (in the derived table), so a
-- subject with multiple events does not inflate the denominator.
--
-- Parameters (SqlRender):
--   @cohort_database_schema  schema containing the cohort table
--   @cdm_database_schema     schema containing the OMOP CDM (observation_period)
--   @cohort_table            cohort table name (base name only)
--   @target_id               cohort_definition_id of the target (exposure) cohort
--   @outcome_id              cohort_definition_id of the outcome cohort

SELECT
    COUNT(*)                                        AS n_persons,
    COALESCE(SUM(x.n_events), 0)                    AS n_events,
    SUM(CAST(x.tar_days AS FLOAT)) / 365.25         AS person_years
FROM (
    SELECT
        t.subject_id,
        -- days from entry to min(observation end, cohort end)
        DATEDIFF(day, t.cohort_start_date,
            CASE WHEN op.observation_period_end_date < t.cohort_end_date
                 THEN op.observation_period_end_date
                 ELSE t.cohort_end_date END)                            AS tar_days,
        -- all outcome occurrences within the same window
        (
            SELECT COUNT(*)
            FROM @cohort_database_schema.@cohort_table o
            WHERE o.subject_id           = t.subject_id
              AND o.cohort_definition_id = @outcome_id
              AND o.cohort_start_date   >= t.cohort_start_date
              AND o.cohort_start_date   <=
                    CASE WHEN op.observation_period_end_date < t.cohort_end_date
                         THEN op.observation_period_end_date
                         ELSE t.cohort_end_date END
        )                                                               AS n_events
    FROM @cohort_database_schema.@cohort_table t
    INNER JOIN @cdm_database_schema.observation_period op
        ON  t.subject_id                     = op.person_id
        AND op.observation_period_start_date <= t.cohort_start_date
        AND op.observation_period_end_date   >= t.cohort_start_date
    WHERE t.cohort_definition_id = @target_id
) x
;
