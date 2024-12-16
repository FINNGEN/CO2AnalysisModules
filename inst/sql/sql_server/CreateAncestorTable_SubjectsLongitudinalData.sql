-- DESCRIPTION:
-- Creates concept ancestor table containing hierarchical relationships between concepts.
-- Extracts concept ancestor relationships from OMOP CDM for all concepts used in the
-- events, measurements, drugs and eras tables to enable hierarchical analysis.
--
-- PARAMETERS:
-- - cdm_database_schema: Schema containing the OMOP CDM tables
--

-- Concept ancestor
DROP TABLE IF EXISTS #concept_ancestor;

CREATE TABLE #concept_ancestor (
  ancestor_concept_id BIGINT,
  descendant_concept_id BIGINT,
  min_levels_of_separation BIGINT,
  max_levels_of_separation BIGINT
);

INSERT INTO #concept_ancestor (
  ancestor_concept_id,
  descendant_concept_id,
  min_levels_of_separation,
  max_levels_of_separation
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

SELECT ancestor_concept_id,
       descendant_concept_id,
       min_levels_of_separation,
       max_levels_of_separation
FROM (
    SELECT * FROM @cdm_database_schema.concept_ancestor
    JOIN concept_ids ON concept_ancestor.ancestor_concept_id = concept_ids.concept_id
    UNION DISTINCT
    SELECT * FROM @cdm_database_schema.concept_ancestor
    JOIN concept_ids ON concept_ancestor.descendant_concept_id = concept_ids.concept_id
) AS concept_ancestor_union;
