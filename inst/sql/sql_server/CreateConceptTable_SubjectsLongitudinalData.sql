-- DESCRIPTION:
-- Creates concept ancestor table containing hierarchical relationships between concepts.
-- Extracts concept ancestor relationships from OMOP CDM for all concepts used in the
-- events, measurements, drugs and eras tables to enable hierarchical analysis.
--
-- PARAMETERS:
-- - cdm_database_schema: Schema containing the OMOP CDM tables
--

-- Concept
DROP TABLE IF EXISTS #concept;

CREATE TABLE #concept (
  concept_id BIGINT,
  concept_name VARCHAR(255),
  domain_id VARCHAR(255),
  vocabulary_id VARCHAR(255),
  concept_class_id VARCHAR(255),
  standard_concept VARCHAR(255),
  concept_code VARCHAR(255)
);

INSERT INTO #concept (
  concept_id,
  concept_name,
  domain_id,
  vocabulary_id,
  concept_class_id,
  standard_concept,
  concept_code
)

WITH concept_ids AS (
  -- Events
  SELECT concept_id
  FROM #events
  UNION DISTINCT
  SELECT source_concept_id
  FROM #events
  UNION DISTINCT
  SELECT visit_concept_id
  FROM #events
  UNION DISTINCT
  SELECT visit_source_concept_id
  FROM #events
  UNION DISTINCT
  -- Measurements
  SELECT concept_id
  FROM #measurements
  UNION DISTINCT
  SELECT source_concept_id
  FROM #measurements
  UNION DISTINCT
  SELECT visit_concept_id
  FROM #measurements
  UNION DISTINCT
  SELECT visit_source_concept_id
  FROM #measurements
  UNION DISTINCT
  SELECT unit_concept_id
  FROM #measurements
  UNION DISTINCT
  SELECT value_as_concept_id
  FROM #measurements
  UNION DISTINCT
  -- Drugs
  SELECT concept_id
  FROM #drugs
  UNION DISTINCT
  SELECT source_concept_id
  FROM #drugs
  UNION DISTINCT
  SELECT visit_concept_id
  FROM #drugs
  UNION DISTINCT
  SELECT visit_source_concept_id
  FROM #drugs
  UNION DISTINCT
  -- Eras
  SELECT concept_id
  FROM #eras
)


SELECT 
  c.concept_id,
  c.concept_name,
  c.domain_id,
  c.vocabulary_id,
  c.concept_class_id,
  c.standard_concept,
  c.concept_code
 FROM @cdm_database_schema.concept AS c
JOIN concept_ids AS cs
ON c.concept_id = cs.concept_id
