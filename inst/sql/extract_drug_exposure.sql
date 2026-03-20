-- extract_drug_exposure.sql
-- SqlRender-parameterized extraction of drug_exposure records for
-- corticosteroid dose calculation.
--
-- Parameters
-- ----------
-- @cdm_schema      : schema containing OMOP CDM tables
-- @start_date      : lower bound on drug_exposure_start_date (YYYY-MM-DD string)
-- @end_date        : upper bound on drug_exposure_start_date (YYYY-MM-DD string)
-- @concept_filter  : comma-separated drug_concept_id list, or empty string '' to skip
-- @person_filter   : comma-separated person_id list, or empty string '' to skip
--
-- All DBMS dialects are handled by SqlRender::translate() at query time.
-- Never modify this file to embed literal SQL Server / PostgreSQL syntax.

SELECT
    de.person_id,
    de.drug_exposure_id,
    de.drug_concept_id,
    de.drug_source_concept_id,
    CAST(de.drug_exposure_start_date AS DATE) AS drug_exposure_start_date,
    CAST(de.drug_exposure_end_date   AS DATE) AS drug_exposure_end_date,
    de.quantity,
    de.days_supply,
    de.sig,
    de.route_concept_id,
    de.dose_unit_source_value,
    de.drug_source_value,
    c.concept_name  AS drug_concept_name,
    ds.amount_value,
    ds.amount_unit_concept_id

FROM @cdm_schema.drug_exposure de

LEFT JOIN @cdm_schema.concept c
    ON de.drug_concept_id = c.concept_id

-- drug_strength is aggregated per drug_concept_id before joining so that
-- combination drugs (multiple ingredient rows) do not produce duplicate
-- drug_exposure rows. For single-ingredient corticosteroids MAX() is
-- equivalent to the actual value.
LEFT JOIN (
    SELECT
        drug_concept_id,
        MAX(amount_value)           AS amount_value,
        MAX(amount_unit_concept_id) AS amount_unit_concept_id
    FROM @cdm_schema.drug_strength
    WHERE amount_value IS NOT NULL
    GROUP BY drug_concept_id
) ds ON de.drug_concept_id = ds.drug_concept_id

WHERE de.drug_exposure_start_date >= CAST('@start_date' AS DATE)
  AND de.drug_exposure_start_date <= CAST('@end_date'   AS DATE)
  {@concept_filter != ''} ? {AND de.drug_concept_id IN (@concept_filter)}
  {@person_filter  != ''} ? {AND de.person_id IN (@person_filter)}
;
