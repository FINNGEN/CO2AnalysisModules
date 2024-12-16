-- DESCRIPTION:
-- Creates an eras table combining condition and drug eras for a sample cohort.
-- Aggregates longitudinal data from condition_era and drug_era tables to create
-- a unified view of patient condition and medication periods.
--
-- PARAMETERS:
-- - n_subjects: Number of subjects to sample from the cohort
-- - cohort_id_cases: ID of the cohort to sample from
-- - cohort_database_schema: Schema containing the cohort table
-- - cohort_table: Name of the cohort table
-- - cdm_database_schema: Schema containing the OMOP CDM tables
-- - seed: Random seed for reproducible sampling
--
DROP TABLE IF EXISTS #eras;

CREATE TABLE #eras (
    person_source_value VARCHAR(255),
    event_domain VARCHAR(255),
    start_date DATE,
    end_date DATE,
    concept_id BIGINT
);

INSERT INTO #eras (
    person_source_value,
    event_domain,
    start_date,
    end_date,
    concept_id
)
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
        co.condition_era_start_date as start_date,
        co.condition_era_end_date as end_date,
        co.condition_concept_id as concept_id
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.condition_era AS co
    ON sc.person_id = co.person_id

    UNION ALL

    SELECT 
        sc.person_source_value,
        'drug' as event_domain,
        de.drug_era_start_date as start_date,
        de.drug_era_end_date as end_date,
        de.drug_concept_id as concept_id
    FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.drug_era AS de
    ON sc.person_id = de.person_id

)
SELECT 
    ce.person_source_value,
    ce.event_domain,
    ce.start_date,
    ce.end_date,
    ce.concept_id
FROM combined_events ce
