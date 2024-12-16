-- DESCRIPTION:
-- Creates prevalence table containing demographic stratified prevalence statistics for concepts.
-- Extracts prevalence data from a pre-computed table for all concepts used in the events,
-- measurements and drugs tables to enable prevalence analysis.
--
-- PARAMETERS:
-- - schema_sandbox: Schema containing the prevalence table
-- - prevalence_table: Name of the pre-computed prevalence table
--

DROP TABLE IF EXISTS #prevalence;

CREATE OR REPLACE TABLE #prevalence (
  source_concept_id BIGINT,
  sex VARCHAR(255),
  year_of_birth INT,
  age_decile FLOAT,
  n_persons_with_code BIGINT,
  n_persons_with_observation BIGINT

);

INSERT INTO #prevalence (
  source_concept_id,
  sex,
  year_of_birth,
  age_decile,
  n_persons_with_code,
  n_persons_with_observation
)

WITH concept_ids AS (
  SELECT source_concept_id
  FROM #events
  UNION DISTINCT
  SELECT source_concept_id
  FROM #measurements
  UNION DISTINCT
  SELECT source_concept_id
  FROM #drugs
)

SELECT pt.source_concept_id,
       pt.sex,
       pt.year_of_birth,
       pt.age_decile,
       pt.n_persons_with_code,
       pt.n_persons_in_observation
FROM @prevalence_table AS pt
JOIN concept_ids AS cs
 ON pt.source_concept_id = cs.source_concept_id;
