-- cohort_VZV_antivirals.sql
-- VZV / Herpes Zoster Antivirals cohort
--
-- Selects person_ids of patients who:
--   1. Have a VZV / herpes zoster diagnosis (index event)
--   2. Received an antiviral drug on or after the index date
--   3. Were on immunosuppressive therapy at any point in their observation period
--   4. Were age >= 18 at the time of the index event
--
-- This is the fixed (non-parameterized) version. For a JSON-driven version
-- that lets you swap concept sets and criteria, use build_cohort_sql() in R/cohort.R.
--
-- Parameters (SqlRender)
-- ----------------------
-- @cdm_schema   : schema containing OMOP CDM clinical tables
-- @vocab_schema : schema containing concept / concept_ancestor vocabulary tables
--
-- Usage in R
-- ----------
--   sql  <- SqlRender::readSql(system.file("sql", "cohort_VZV_antivirals.sql",
--                                           package = "SteroidDoseR"))
--   ids  <- DatabaseConnector::renderTranslateQuerySql(
--             conn, sql,
--             cdm_schema   = "Myositis_OMOP.dbo",
--             vocab_schema = "Myositis_OMOP.dbo",
--             snakeCaseToCamelCase = FALSE
--           )
--   COHORT_PERSON_IDS <- as.integer(ids[[1]])

WITH

-- ---------------------------------------------------------------------------
-- Concept set 1: VZV / herpes zoster conditions (with descendants)
-- ---------------------------------------------------------------------------
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
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4205455, 35205739, 443943, 138682, 45770836, 436336, 440329,
    45590840, 4151978, 192239, 381504, 45542548, 45556927,
    35205737, 35205738, 35205740, 35205741
  )
  AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Concept set 2: Antiviral drugs (acyclovir, valacyclovir, famciclovir,
--                                  and descendants)
-- ---------------------------------------------------------------------------
antiviral_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (1703687, 1703603, 1717704)
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (1703687, 1703603, 1717704)
    AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Concept set 3: Immunosuppressant / DMARD drugs (with descendants)
-- ---------------------------------------------------------------------------
immunosuppressant_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c
    ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087
  )
    AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Index events: earliest VZV diagnosis within an observation period
-- ---------------------------------------------------------------------------
index_events AS (
  SELECT
    co.person_id,
    MIN(co.condition_start_date)              AS index_date,
    MIN(op.observation_period_start_date)     AS op_start_date,
    MAX(op.observation_period_end_date)       AS op_end_date
  FROM @cdm_schema.condition_occurrence co
  JOIN vzv_concepts vc
    ON co.condition_concept_id = vc.concept_id
  JOIN @cdm_schema.observation_period op
    ON  co.person_id = op.person_id
    AND co.condition_start_date
          BETWEEN op.observation_period_start_date
              AND op.observation_period_end_date
  GROUP BY co.person_id
)

-- ---------------------------------------------------------------------------
-- Final cohort: apply all four inclusion rules
-- ---------------------------------------------------------------------------
SELECT DISTINCT ie.person_id
FROM index_events ie
JOIN @cdm_schema.person p
  ON p.person_id = ie.person_id

-- Rule 1: age >= 18 at index
WHERE YEAR(ie.index_date) - p.year_of_birth >= 18

-- Rule 2: antiviral drug on or after index date
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  JOIN antiviral_concepts ac
    ON de.drug_concept_id = ac.concept_id
  WHERE de.person_id = ie.person_id
    AND de.drug_exposure_start_date >= ie.index_date
)

-- Rule 3: immunosuppressant / DMARD therapy anytime in observation period
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  JOIN immunosuppressant_concepts ic
    ON de.drug_concept_id = ic.concept_id
  WHERE de.person_id = ie.person_id
    AND de.drug_exposure_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
)
;
