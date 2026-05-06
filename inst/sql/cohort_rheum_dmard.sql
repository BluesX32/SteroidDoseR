-- cohort_rheum_dmard.sql
-- Rheumatic disease + DMARD base cohort
--
-- Selects person_ids of patients who:
--   1. Have a rheumatic disease diagnosis (index event)
--   2. Have at least one DMARD / immunosuppressant drug exposure during
--      their observation period
--   3. Were age > 18 at the time of the index event
--
-- Concept sets derived from TrajectoryDashboard/inst/sql/templates/rheum-dmard-cohort-omop.sql
--
-- Parameters (SqlRender)
-- ----------------------
-- @cdm_schema   : schema containing OMOP CDM clinical tables
-- @vocab_schema : schema containing concept / concept_ancestor vocabulary tables

WITH

-- ---------------------------------------------------------------------------
-- Concept set: rheumatic disease conditions — exact match (no descendants)
-- Codesets 0, 3, 4, 5, 6 from the OHDSI phenotype definition
-- ---------------------------------------------------------------------------
rheum_exact AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    -- codeset 0: rheumatoid arthritis and related
    37016279, 4319305, 4300204, 4324123, 4066824, 432919, 606388, 46273369,
    4055640, 35208699, 45562709, 45567545, 257628, 606386, 255891, 46270384,
    35208826, 35208701, 45606214, 3321233, 45601434, 606430, 4145240, 4343923,
    35208700, 44819941, 4344158, 4149913, 45582126, 35208827, 45591820,
    -- codeset 3: myositis / inflammatory myopathy
    45548265, 45586838, 45606052, 45543436, 45572339, 45553046, 45591705,
    45562599, 45543443, 45562600, 45567422, 45567423, 45586845, 725373,
    45606064, 45538639, 45606063, 45543442, 45601289, 45572346, 45577117,
    45567425, 45577119, 45548271, 45548270, 45567426, 80809, 4117687, 4115161,
    4116440, 4116150, 4116151, 4117686, 4114439, 4116441, 45591700, 45572337,
    45596437, 45538633, 45548263, 45543435, 45582014, 45553045, 725370,
    45596438, 45548261, 45606051, 45572338, 45548262, 45596436, 45606050,
    45596439, 45562591, 45582015, 45567419, 45533697, 45567418, 45543434,
    45553044, 35208750, 37160562, 45567420, 45577104, 45572340, 45533702,
    45553051, 45533701, 45562593, 45572341, 725372, 45577109, 45557762,
    45606055, 45557763, 45601284, 45606053, 45606054, 45533703, 45577105,
    45577107, 45538635, 45591701, 45596442, 45606056, 35208753, 45586836,
    45557754, 45591686, 45572332, 45538631, 45567415, 45591694, 45548258,
    45548257, 45567413, 45596428, 45596427, 45572327, 4083556, 37207809,
    4035611,
    -- codeset 4: other connective tissue diseases
    36716891, 37017494, 1077506, 766408, 766409, 766411, 766410, 766402,
    37110375, 37205058, 40319772, 45548197, 46274123, 4064048, 437082,
    45548419, 45533841, 45586969, 45601454, 45548418, 45533840, 45553184,
    45543577, 45582150, 45567561,
    -- codeset 5: vasculitis and related
    4126439, 37397763, 4337524, 4128222, 134442, 4331739, 441928, 4105026,
    44811612, 40352976, 4027230,
    -- codeset 6: lupus and spondyloarthropathy
    314963, 35208820, 4343935, 35208821
  )
),

-- ---------------------------------------------------------------------------
-- Concept set: rheumatic disease conditions — with descendants
-- Codesets 1, 2, 7 from the OHDSI phenotype definition
-- ---------------------------------------------------------------------------
rheum_with_desc AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    -- codeset 1: RA and related (with descendants)
    4270868, 4005037, 80182, 4081250, 4344161,
    -- codeset 2: inflammatory arthritis (ancestor 42535714 + named)
    42535714, 4146124, 4096220, 37166813, 4236160, 37110370, 4137275,
    37110368, 37110369, 37167489,
    -- codeset 7: spondylitis / ankylosing spondylitis (with descendants)
    4305666, 313223, 4344493, 606328, 320749
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    4270868, 4005037, 80182, 4081250, 4344161,
    42535714,
    4305666, 313223, 4344493, 606328, 320749
  )
  AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Union: all rheumatic disease concept IDs
-- ---------------------------------------------------------------------------
rheum_concepts AS (
  SELECT concept_id FROM rheum_exact
  UNION
  SELECT concept_id FROM rheum_with_desc
),

-- ---------------------------------------------------------------------------
-- Concept set: DMARD / immunosuppressant drugs — with descendants
-- Codeset 8 from the OHDSI phenotype definition
-- ---------------------------------------------------------------------------
dmard_concepts AS (
  SELECT DISTINCT concept_id
  FROM @vocab_schema.concept
  WHERE concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087,  35603563
  )
  UNION
  SELECT DISTINCT ca.descendant_concept_id
  FROM @vocab_schema.concept_ancestor ca
  JOIN @vocab_schema.concept c ON ca.descendant_concept_id = c.concept_id
  WHERE ca.ancestor_concept_id IN (
    19014878, 19068900, 19003999, 1361580, 42904205, 40171288, 1305058,
    1101898,  1594587,  1310317,  1314273, 701470,   40236987, 45892883,
    746895,   1119119,  937368,   1151789, 1593700,  40161532, 1511348,
    1186087,  1777087,  35603563
  )
  AND c.invalid_reason IS NULL
),

-- ---------------------------------------------------------------------------
-- Index events: earliest rheumatic disease diagnosis within an observation period
-- ---------------------------------------------------------------------------
index_events AS (
  SELECT
    co.person_id,
    MIN(co.condition_start_date)              AS index_date,
    MIN(op.observation_period_start_date)     AS op_start_date,
    MAX(op.observation_period_end_date)       AS op_end_date
  FROM @cdm_schema.condition_occurrence co
  JOIN rheum_concepts rc
    ON co.condition_concept_id = rc.concept_id
  JOIN @cdm_schema.observation_period op
    ON  co.person_id = op.person_id
    AND co.condition_start_date
          BETWEEN op.observation_period_start_date
              AND op.observation_period_end_date
  GROUP BY co.person_id
)

-- ---------------------------------------------------------------------------
-- Final cohort: rheumatic disease + DMARD + age > 18
-- ---------------------------------------------------------------------------
SELECT DISTINCT ie.person_id
FROM index_events ie
JOIN @cdm_schema.person p
  ON p.person_id = ie.person_id

-- Rule 1: age >= 18 at index
WHERE YEAR(ie.index_date) - p.year_of_birth >= 18

-- Rule 2: DMARD / immunosuppressant exposure anytime in observation period
AND EXISTS (
  SELECT 1
  FROM @cdm_schema.drug_exposure de
  JOIN dmard_concepts dc
    ON de.drug_concept_id = dc.concept_id
  WHERE de.person_id = ie.person_id
    AND de.drug_exposure_start_date
          BETWEEN ie.op_start_date AND ie.op_end_date
)
;
