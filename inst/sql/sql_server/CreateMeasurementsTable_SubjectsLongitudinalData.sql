-- DESCRIPTION:
-- Creates a measurements table containing clinical measurement data for a sample cohort.
-- Captures measurement values, units, reference ranges and visit context from the
-- OMOP CDM measurement table.
--
-- PARAMETERS:
-- - n_subjects: Number of subjects to sample from the cohort
-- - cohort_id_cases: ID of the cohort to sample from
-- - cohort_database_schema: Schema containing the cohort table
-- - cohort_table: Name of the cohort table
-- - cdm_database_schema: Schema containing the OMOP CDM tables
-- - seed: Random seed for reproducible sampling
--
DROP TABLE IF EXISTS #measurements;

CREATE TABLE #measurements (
    person_source_value VARCHAR(255),
    event_domain VARCHAR(255),
    visit_concept_id BIGINT,
    visit_source_concept_id BIGINT,
    start_date DATE,
    end_date DATE,
    concept_id BIGINT,
    source_concept_id BIGINT,
    source_value VARCHAR(255),
    value_as_number FLOAT,
    value_as_concept_id BIGINT,
    unit_concept_id BIGINT,
    range_low FLOAT,
    range_high FLOAT
);

INSERT INTO #measurements (
  person_source_value,
  event_domain,
  visit_concept_id,
  visit_source_concept_id,
  start_date,
  end_date,
  concept_id,
  source_concept_id,
  source_value,
  value_as_number,
  value_as_concept_id,
  unit_concept_id,
  range_low,
  range_high
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
)


SELECT sc.person_source_value,
       'Measurement' AS event_domain,
       vo.visit_concept_id,
       vo.visit_source_concept_id,
       vo.visit_start_date AS start_date,
       vo.visit_end_date AS end_date,
       me.measurement_concept_id AS concept_id,
       me.measurement_source_concept_id AS source_concept_id,
       me.measurement_source_value AS source_value,
       me.value_as_number,
       me.value_as_concept_id,
       me.unit_concept_id,
       me.range_low,
       me.range_high
FROM sample_cohort AS sc
    INNER JOIN @cdm_database_schema.measurement AS me
    ON sc.person_id = me.person_id
    JOIN @cdm_database_schema.visit_occurrence AS vo
    ON vo.visit_occurrence_id = me.visit_occurrence_id

