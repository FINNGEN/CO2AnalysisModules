-- DESCRIPTION:
-- Creates a comprehensive events table combining multiple OMOP CDM clinical event domains.
-- Includes conditions, procedures, observations, devices, and death records in a unified
-- longitudinal view with visit context for temporal analysis.
--
-- PARAMETERS:
-- - n_subjects: Number of subjects to sample from the cohort
-- - cohort_id_cases: ID of the cohort to sample from
-- - cohort_database_schema: Schema containing the cohort table
-- - cohort_table: Name of the cohort table
-- - cdm_database_schema: Schema containing the OMOP CDM tables
-- - seed: Random seed for reproducible sampling
-- 
DROP TABLE IF EXISTS #events;

CREATE TABLE #events (
    person_source_value VARCHAR(255),
    event_domain VARCHAR(255),
    visit_concept_id BIGINT,
    visit_source_concept_id BIGINT,
    start_date DATE,
    end_date DATE,
    concept_id BIGINT,
    source_concept_id BIGINT,
    source_value VARCHAR(255)
);

-- Sample cohort
-- Select @n_subjects subjects from the @cohort_id_cases
WITH sample_cohort AS (
    SELECT TOP @n_subjects 
        ct.subject_id as person_id, 
        p.person_source_value 
    FROM @cohort_database_schema.@cohort_table AS ct
    LEFT JOIN @cdm_database_schema.person AS p
    ON ct.subject_id = p.person_id
    WHERE cohort_definition_id = @cohort_id_cases
    ORDER BY HASHBYTES('MD5', CONCAT(person_source_value, CAST(@seed AS VARCHAR)))
),

-- Combined events
-- Union together condition occurrences, procedures, observations, and devices into a single longitudinal view
-- for the @n_subjects subjects
combined_events AS (
    SELECT 
        sc.person_source_value,
        'condition' as event_domain,
        co.visit_occurrence_id,
        co.condition_start_date as start_date,
        co.condition_end_date as end_date,
        co.condition_concept_id as concept_id,
        co.condition_source_concept_id as source_concept_id,
        co.condition_source_value as source_value
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.condition_occurrence AS co
    ON sc.person_id = co.person_id
    

    UNION ALL

    SELECT 
        sc.person_source_value,
        'procedure' as event_domain,
        po.visit_occurrence_id,
        po.procedure_date as start_date,
        po.procedure_date as end_date,
        po.procedure_concept_id as concept_id,
        po.procedure_source_concept_id as source_concept_id,
        po.procedure_source_value as source_value
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.procedure_occurrence AS po
    ON sc.person_id = po.person_id

    UNION ALL

    SELECT 
        sc.person_source_value,
        'observation' as event_domain,
        o.visit_occurrence_id,
        o.observation_date as start_date,
        o.observation_date as end_date,
        o.observation_concept_id as concept_id,
        o.observation_source_concept_id as source_concept_id,
        o.observation_source_value as source_value
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.observation AS o
    ON sc.person_id = o.person_id

    UNION ALL

    SELECT 
        sc.person_source_value,
        'device' as event_domain,
        d.visit_occurrence_id,
        d.device_exposure_start_date as start_date,
        d.device_exposure_end_date as end_date,
        d.device_concept_id as concept_id,
        d.device_source_concept_id as source_concept_id,
        d.device_source_value as source_value
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.device_exposure AS d
    ON sc.person_id = d.person_id

    UNION ALL

    SELECT 
        sc.person_source_value,
        'death' as event_domain,
        NULL as visit_occurrence_id,
        d.death_date as start_date,
        d.death_date as end_date,
        d.cause_concept_id as concept_id,
        d.cause_source_concept_id as source_concept_id,
        d.cause_source_value as source_value
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.death AS d
    ON sc.person_id = d.person_id
)

INSERT INTO #events (
    person_source_value,
    event_domain,
    visit_concept_id,
    visit_source_concept_id,
    start_date,
    end_date,
    concept_id,
    source_concept_id,
    source_value
)
SELECT 
    ce.person_source_value,
    ce.event_domain,
    vo.visit_concept_id,
    vo.visit_source_concept_id,
    ce.start_date,
    ce.end_date,
    ce.concept_id,
    ce.source_concept_id,
    ce.source_value
FROM combined_events ce
 JOIN @cdm_database_schema.visit_occurrence vo
    ON ce.visit_occurrence_id = vo.visit_occurrence_id;
