SELECT
  DATEDIFF(day, t.cohort_start_date,
    CASE
      WHEN o.cohort_start_date IS NOT NULL THEN o.cohort_start_date
      ELSE op.observation_period_end_date
    END) AS time_to_event,
  CASE WHEN o.cohort_start_date IS NOT NULL THEN 1 ELSE 0 END AS event
FROM @cohort_database_schema.@cohort_table t
INNER JOIN @cdm_database_schema.observation_period op
  ON t.subject_id = op.person_id
  AND op.observation_period_start_date <= t.cohort_start_date
  AND op.observation_period_end_date   >= t.cohort_start_date
LEFT JOIN @cohort_database_schema.@cohort_table o
  ON t.subject_id = o.subject_id
  AND o.cohort_definition_id = @outcome_cohort_id
  AND o.cohort_start_date > t.cohort_start_date
WHERE t.cohort_definition_id = @target_cohort_id
;
