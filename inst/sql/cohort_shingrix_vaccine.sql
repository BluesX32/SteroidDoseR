-- cohort_shingrix_vaccine.sql
-- Herpes zoster (Shingrix / Zostavax) vaccine cohort
--
-- Selects person_ids of patients who received the shingles vaccine,
-- identified via drug_exposure or procedure_occurrence.
--
-- Concept set derived from TrajectoryDashboard/inst/sql/templates/def_shingrix_vaccine.sql
-- Ancestor concepts: 44808679 (zoster vaccine live), 21601361 (recombinant zoster vaccine),
--                    706103 (varicella zoster vaccines)
-- Exclusions: 40213260, 706104, 40213255, 40213256 (antiviral treatments, not vaccines)
--
-- Parameters (SqlRender)
-- ----------------------
-- @cdm_schema    : schema containing OMOP CDM clinical tables
-- @vocab_schema  : schema containing concept / concept_ancestor vocabulary tables
-- @person_filter : comma-separated person_id list, or empty string '' to skip

WITH

-- ---------------------------------------------------------------------------
-- Concept set: zoster vaccine (with descendants, antiviral exclusions removed)
-- ---------------------------------------------------------------------------
vaccine_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (44808679, 21601361, 706103)
    AND invalid_reason IS NULL
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (44808679, 21601361, 706103)
    AND c.invalid_reason IS NULL
    AND ca.descendant_concept_id NOT IN (40213260, 706104, 40213255, 40213256)
)

-- ---------------------------------------------------------------------------
-- Final cohort: any patient with a vaccine record in drug_exposure or
-- procedure_occurrence, optionally restricted to @person_filter
-- ---------------------------------------------------------------------------
SELECT DISTINCT person_id
FROM (

  -- Vaccine administered as a drug / immunisation product
  SELECT de.person_id
  FROM @cdm_schema.drug_exposure de
  JOIN vaccine_concepts vc ON de.drug_concept_id = vc.concept_id
  {@person_filter != ''} ? {WHERE de.person_id IN (@person_filter)}

  UNION

  -- Vaccine recorded as a procedure
  SELECT po.person_id
  FROM @cdm_schema.procedure_occurrence po
  JOIN vaccine_concepts vc ON po.procedure_concept_id = vc.concept_id
  {@person_filter != ''} ? {WHERE po.person_id IN (@person_filter)}

) combined
;
