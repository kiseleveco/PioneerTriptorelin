CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 7 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (722906,821571,912113,953181,973992,1373045,1602824,1602825,1612993,2718622,3172736,3177079,3181234,3193726,3196294,3196651,3205873,3207026,3207948,3211294,3479775,3481137,4137521,4146283,4164112,4169049,4211281,4213832,4272891,4333866,4336364,19056346,19093370,21234506,21235644,21244430,21262888,21280455,21281915,21603827,35625995,35625996,35625999,35803367,35824560,36130381,36173585,36199292,36287348,36289226,36289302,36298445,36467650,36496554,36498100,36618778,36620907,36621731,36623961,36628977,36630822,36636188,36636691,36641214,36641913,36644364,36645697,36658738,36676201,36680447,36683151,36691559,36691561,36691562,36818046,36818047,36818048,37488818,37492791,37493043,37504076,37505572,37508830,37508831,37508832,37514096,37514097,37514098,38002840,40205567,40483809,42378313,42378314,42378315,42383225,42383226,42542446,42848799,42850922,42850923,42861304,42861478,42861479,42861480,42868285,43006131,43006132,43048893,43307550,43307551,43307552,43309259,43316511,43325544,43327315,43336330,43343563,43352641,43361602,43547803,43547804,43547805,44141668,44154627,44341786,44349834,44371471,44403374,44415698,44422549,44425251,44442993,44444376,44448478,44467193,44467194,44467195,44472224,44928158,45183367,45251957,45269105,45286140,45354275,45415191,45418243,45700820,45710419,45712599,45716056,45716057,45723593,45729683,45777911,45833070,45833075,45833076,45833077,45833083,45833084,45923941,45963180,46083776,46094428,46096565,46099187)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (722906,821571,912113,953181,973992,1373045,1602824,1602825,1612993,2718622,3172736,3177079,3181234,3193726,3196294,3196651,3205873,3207026,3207948,3211294,3479775,3481137,4137521,4146283,4164112,4169049,4211281,4213832,4272891,4333866,4336364,19056346,19093370,21234506,21235644,21244430,21262888,21280455,21281915,21603827,35625995,35625996,35625999,35803367,35824560,36130381,36173585,36199292,36287348,36289226,36289302,36298445,36467650,36496554,36498100,36618778,36620907,36621731,36623961,36628977,36630822,36636188,36636691,36641214,36641913,36644364,36645697,36658738,36676201,36680447,36683151,36691559,36691561,36691562,36818046,36818047,36818048,37488818,37492791,37493043,37504076,37505572,37508830,37508831,37508832,37514096,37514097,37514098,38002840,40205567,40483809,42378313,42378314,42378315,42383225,42383226,42542446,42848799,42850922,42850923,42861304,42861478,42861479,42861480,42868285,43006131,43006132,43048893,43307550,43307551,43307552,43309259,43316511,43325544,43327315,43336330,43343563,43352641,43361602,43547803,43547804,43547805,44141668,44154627,44341786,44349834,44371471,44403374,44415698,44422549,44425251,44442993,44444376,44448478,44467193,44467194,44467195,44472224,44928158,45183367,45251957,45269105,45286140,45354275,45415191,45418243,45700820,45710419,45712599,45716056,45716057,45723593,45729683,45777911,45833070,45833075,45833076,45833077,45833083,45833084,45923941,45963180,46083776,46094428,46096565,46099187)
  and c.invalid_reason is null

) I
) C
;

SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id
INTO #qualified_events
FROM 
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal, cast(pe.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM (-- Begin Primary Events
select P.ordinal as event_id, P.person_id, P.start_date, P.end_date, op_start_date, op_end_date, cast(P.visit_occurrence_id as bigint) as visit_occurrence_id
FROM
(
  select E.person_id, E.start_date, E.end_date,
         row_number() OVER (PARTITION BY E.person_id ORDER BY E.sort_date ASC, E.event_id) ordinal,
         OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date, cast(E.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM 
  (
  -- Begin Drug Exposure Criteria
select C.person_id, C.drug_exposure_id as event_id, C.drug_exposure_start_date as start_date,
       COALESCE(C.DRUG_EXPOSURE_END_DATE, DATEADD(day,C.DAYS_SUPPLY,DRUG_EXPOSURE_START_DATE), DATEADD(day,1,C.DRUG_EXPOSURE_START_DATE)) as end_date,
       C.visit_occurrence_id,C.drug_exposure_start_date as sort_date
from 
(
  select de.* 
  FROM @cdm_database_schema.DRUG_EXPOSURE de
JOIN #Codesets cs on (de.drug_concept_id = cs.concept_id and cs.codeset_id = 7)
) C


-- End Drug Exposure Criteria

  ) E
	JOIN @cdm_database_schema.observation_period OP on E.person_id = OP.person_id and E.start_date >=  OP.observation_period_start_date and E.start_date <= op.observation_period_end_date
  WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE
) P
WHERE P.ordinal = 1
-- End Primary Events
) pe
  
) QE

;

--- Inclusion Rule Inserts

create table #inclusion_events (inclusion_rule_id bigint,
	person_id bigint,
	event_id bigint
);

select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM (
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

) Results
WHERE Results.ordinal = 1
;

-- custom era strategy

with ctePersons(person_id) as (
	select distinct person_id from #included_events
)

select person_id, drug_exposure_start_date, drug_exposure_end_date
INTO #drugTarget
FROM (
	select de.PERSON_ID, DRUG_EXPOSURE_START_DATE, COALESCE(DRUG_EXPOSURE_END_DATE, DATEADD(day,DAYS_SUPPLY,DRUG_EXPOSURE_START_DATE), DATEADD(day,1,DRUG_EXPOSURE_START_DATE)) as DRUG_EXPOSURE_END_DATE 
	FROM @cdm_database_schema.DRUG_EXPOSURE de
	JOIN ctePersons p on de.person_id = p.person_id
	JOIN #Codesets cs on cs.codeset_id = 7 AND de.drug_concept_id = cs.concept_id

	UNION ALL

	select de.PERSON_ID, DRUG_EXPOSURE_START_DATE, COALESCE(DRUG_EXPOSURE_END_DATE, DATEADD(day,DAYS_SUPPLY,DRUG_EXPOSURE_START_DATE), DATEADD(day,1,DRUG_EXPOSURE_START_DATE)) as DRUG_EXPOSURE_END_DATE 
	FROM @cdm_database_schema.DRUG_EXPOSURE de
	JOIN ctePersons p on de.person_id = p.person_id
	JOIN #Codesets cs on cs.codeset_id = 7 AND de.drug_source_concept_id = cs.concept_id
) E
;

select et.event_id, et.person_id, ERAS.era_end_date as end_date
INTO #strategy_ends
from #included_events et
JOIN 
(
  select ENDS.person_id, min(drug_exposure_start_date) as era_start_date, DATEADD(day,90, ENDS.era_end_date) as era_end_date
  from
  (
    select de.person_id, de.drug_exposure_start_date, MIN(e.END_DATE) as era_end_date
    FROM #drugTarget DE
    JOIN 
    (
      --cteEndDates
      select PERSON_ID, DATEADD(day,-1 * 100,EVENT_DATE) as END_DATE -- unpad the end date by 100
      FROM
      (
				select PERSON_ID, EVENT_DATE, EVENT_TYPE, 
				MAX(START_ORDINAL) OVER (PARTITION BY PERSON_ID ORDER BY event_date, event_type, START_ORDINAL ROWS UNBOUNDED PRECEDING) AS start_ordinal,
				ROW_NUMBER() OVER (PARTITION BY PERSON_ID ORDER BY EVENT_DATE, EVENT_TYPE, START_ORDINAL) AS OVERALL_ORD -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
				from
				(
					-- select the start dates, assigning a row number to each
					Select PERSON_ID, DRUG_EXPOSURE_START_DATE AS EVENT_DATE, 0 as EVENT_TYPE, ROW_NUMBER() OVER (PARTITION BY PERSON_ID ORDER BY DRUG_EXPOSURE_START_DATE) as START_ORDINAL
					from #drugTarget D

					UNION ALL

					-- add the end dates with NULL as the row number, padding the end dates by 100 to allow a grace period for overlapping ranges.
					select PERSON_ID, DATEADD(day,100,DRUG_EXPOSURE_END_DATE), 1 as EVENT_TYPE, NULL
					FROM #drugTarget D
				) RAWDATA
      ) E
      WHERE 2 * E.START_ORDINAL - E.OVERALL_ORD = 0
    ) E on DE.PERSON_ID = E.PERSON_ID and E.END_DATE >= DE.DRUG_EXPOSURE_START_DATE
    GROUP BY de.person_id, de.drug_exposure_start_date
  ) ENDS
  GROUP BY ENDS.person_id, ENDS.era_end_date
) ERAS on ERAS.person_id = et.person_id 
WHERE et.start_date between ERAS.era_start_date and ERAS.era_end_date;

TRUNCATE TABLE #drugTarget;
DROP TABLE #drugTarget;


-- generate cohort periods into #final_cohort
select person_id, start_date, end_date
INTO #cohort_rows
from ( -- first_ends
	select F.person_id, F.start_date, F.end_date
	FROM (
	  select I.event_id, I.person_id, I.start_date, CE.end_date, row_number() over (partition by I.person_id, I.event_id order by CE.end_date) as ordinal
	  from #included_events I
	  join ( -- cohort_ends
-- cohort exit dates
-- By default, cohort exit at the event's op end date
select event_id, person_id, op_end_date as end_date from #included_events
UNION ALL
-- End Date Strategy
SELECT event_id, person_id, end_date from #strategy_ends

    ) CE on I.event_id = CE.event_id and I.person_id = CE.person_id and CE.end_date >= I.start_date
	) F
	WHERE F.ordinal = 1
) FE;

select person_id, min(start_date) as start_date, end_date
into #final_cohort
from ( --cteEnds
	SELECT
		 c.person_id
		, c.start_date
		, MIN(ed.end_date) AS end_date
	FROM #cohort_rows c
	JOIN ( -- cteEndDates
    SELECT
      person_id
      , DATEADD(day,-1 * 0, event_date)  as end_date
    FROM
    (
      SELECT
        person_id
        , event_date
        , event_type
        , SUM(event_type) OVER (PARTITION BY person_id ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS interval_status
      FROM
      (
        SELECT
          person_id
          , start_date AS event_date
          , -1 AS event_type
        FROM #cohort_rows

        UNION ALL


        SELECT
          person_id
          , DATEADD(day,0,end_date) as end_date
          , 1 AS event_type
        FROM #cohort_rows
      ) RAWDATA
    ) e
    WHERE interval_status = 0
  ) ed ON c.person_id = ed.person_id AND ed.end_date >= c.start_date
	GROUP BY c.person_id, c.start_date
) e
group by person_id, end_date
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM #final_cohort CO
;




TRUNCATE TABLE #strategy_ends;
DROP TABLE #strategy_ends;


TRUNCATE TABLE #cohort_rows;
DROP TABLE #cohort_rows;

TRUNCATE TABLE #final_cohort;
DROP TABLE #final_cohort;

TRUNCATE TABLE #inclusion_events;
DROP TABLE #inclusion_events;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;
