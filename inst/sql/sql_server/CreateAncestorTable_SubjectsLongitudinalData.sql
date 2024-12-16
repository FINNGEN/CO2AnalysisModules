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
  SELECT DISTINCT concept_id 
  FROM (
  -- Events
  SELECT concept_id
  FROM #events
  UNION 
  SELECT source_concept_id
  FROM #events
  UNION 
  SELECT visit_concept_id
  FROM #events
  UNION 
  SELECT visit_source_concept_id
  FROM #events
  UNION 
  -- Measurements
  SELECT concept_id
  FROM #measurements
  UNION 
  SELECT source_concept_id
  FROM #measurements
  UNION 
  SELECT visit_concept_id
  FROM #measurements
  UNION 
  SELECT visit_source_concept_id
  FROM #measurements
  UNION 
  SELECT unit_concept_id
  FROM #measurements
  UNION 
  SELECT value_as_concept_id
  FROM #measurements
  UNION 
  -- Drugs
  SELECT concept_id
  FROM #drugs
  UNION 
  SELECT source_concept_id
  FROM #drugs
  UNION 
  SELECT visit_concept_id
  FROM #drugs
  UNION 
  SELECT visit_source_concept_id
  FROM #drugs
  UNION 
  -- Eras
  SELECT concept_id
  FROM #eras
  )
)

SELECT DISTINCT ancestor_concept_id,
       descendant_concept_id,
       min_levels_of_separation,
       max_levels_of_separation
FROM (
    SELECT * FROM @cdm_database_schema.concept_ancestor
    JOIN concept_ids ON concept_ancestor.ancestor_concept_id = concept_ids.concept_id
    UNION 
    SELECT * FROM @cdm_database_schema.concept_ancestor
    JOIN concept_ids ON concept_ancestor.descendant_concept_id = concept_ids.concept_id
) AS concept_ancestor_union;
