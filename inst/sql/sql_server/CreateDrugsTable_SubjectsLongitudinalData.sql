-- DESCRIPTION:
-- Creates a drugs table containing medication exposure data for a sample cohort.
-- Joins cohort data with OMOP CDM drug_exposure table to capture drug exposure details
-- including start/end dates, concepts, quantities and visit information.
--
-- PARAMETERS:
-- - n_subjects: Number of subjects to sample from the cohort
-- - cohort_id_cases: ID of the cohort to sample from
-- - cohort_database_schema: Schema containing the cohort table
-- - cohort_table: Name of the cohort table
-- - cdm_database_schema: Schema containing the OMOP CDM tables
-- - seed: Random seed for reproducible sampling
--
DROP TABLE IF EXISTS #drugs;

CREATE TABLE #drugs (
    person_source_value VARCHAR(255),
    event_domain VARCHAR(255),
    visit_concept_id BIGINT,
    visit_source_concept_id BIGINT,
    start_date DATE,
    end_date DATE,
    concept_id BIGINT,
    source_concept_id BIGINT,
    source_value VARCHAR(255),
    quantity FLOAT
);

-- Sample cohort
-- Select @n_subjects subjects from the @cohort_id_cases
INSERT INTO #drugs (
    person_source_value,
    event_domain,
    visit_concept_id,
    visit_source_concept_id,
    start_date,
    end_date,
    concept_id,
    source_concept_id,
    source_value,
    quantity
)

WITH sample_cohort AS (
    SELECT TOP @n_subjects 
        ct.subject_id as person_id, 
        p.person_source_value 
    FROM @cohort_database_schema.@cohort_table AS ct
    LEFT JOIN @cdm_database_schema.person AS p
    ON ct.subject_id = p.person_id
    WHERE cohort_definition_id = @cohort_id_cases
    ORDER BY HASHBYTES('MD5', CONCAT(person_source_value, CAST(@seed AS VARCHAR)))
)

SELECT 
    sc.person_source_value,
    'drug' as event_domain,
    vo.visit_concept_id,
    vo.visit_source_concept_id,
    de.drug_exposure_start_date as start_date,
    de.drug_exposure_end_date as end_date,
    de.drug_concept_id as concept_id,
    de.drug_source_concept_id as source_concept_id,
    de.drug_source_value as source_value,
    de.quantity
FROM sample_cohort sc
INNER JOIN @cdm_database_schema.drug_exposure de
    ON sc.person_id = de.person_id
LEFT JOIN @cdm_database_schema.visit_occurrence vo
    ON de.visit_occurrence_id = vo.visit_occurrence_id; 