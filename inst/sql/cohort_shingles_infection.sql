-- cohort_shingles_infection.sql
-- Herpes zoster (shingles) infection cohort
--
-- Selects person_ids of patients who have any VZV / herpes zoster diagnosis,
-- optionally restricted to a pre-specified list of person_ids.
--
-- Intended use: identify which base-cohort patients ever had shingles.
-- No antiviral or immunosuppressant requirement (use cohort_VZV_antivirals.sql
-- for the stricter treated-shingles cohort).
--
-- Concept set: same VZV condition concepts used in cohort_VZV_antivirals.sql
--
-- Parameters (SqlRender)
-- ----------------------
-- @cdm_schema    : schema containing OMOP CDM clinical tables
-- @vocab_schema  : schema containing concept / concept_ancestor vocabulary tables
-- @person_filter : comma-separated person_id list, or empty string '' to skip

WITH

vzv_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741, 141374, 37165237,
    4221382, 4066727, 37165216, 4080937, 4299673, 37110753,
    4064036, 4067067, 40175007, 37165342, 4080929, 4063440,
    4272156, 4033204, 4033778, 4206461, 135618, 4033777
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741
  )
  AND c.invalid_reason IS NULL
)

SELECT DISTINCT co.person_id
FROM @cdm_schema.condition_occurrence co
JOIN vzv_concepts vc ON co.condition_concept_id = vc.concept_id
{@person_filter != ''} ? {WHERE co.person_id IN (@person_filter)}
;
