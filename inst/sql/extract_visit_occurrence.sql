-- extract_visit_occurrence.sql
-- SqlRender-parameterized extraction of visit_occurrence records for
-- cross-sectional (point-in-time) dose comparison.
--
-- Parameters
-- ----------
-- @cdm_schema      : schema containing clinical OMOP CDM tables (visit_occurrence)
-- @start_date      : lower bound on visit_start_date (YYYY-MM-DD string)
-- @end_date        : upper bound on visit_start_date (YYYY-MM-DD string)
-- @concept_filter  : comma-separated visit_concept_id list, or empty string
--                   '' to skip -- e.g. 9202 for OMOP standard "Outpatient
--                   Visit". This defines what counts as an "office visit" for
--                   the study and should be confirmed with the study PI
--                   rather than left at any one default.
-- @person_filter   : comma-separated person_id list, or empty string '' to skip
--
-- All DBMS dialects are handled by SqlRender::translate() at query time.
-- Never modify this file to embed literal SQL Server / PostgreSQL syntax.

SELECT
    vo.person_id,
    vo.visit_occurrence_id,
    CAST(vo.visit_start_date AS DATE) AS visit_start_date,
    vo.visit_concept_id

FROM @cdm_schema.visit_occurrence vo

WHERE vo.visit_start_date >= CAST('@start_date' AS DATE)
  AND vo.visit_start_date <= CAST('@end_date'   AS DATE)
  {@concept_filter != ''} ? {AND vo.visit_concept_id IN (@concept_filter)}
  {@person_filter  != ''} ? {AND vo.person_id IN (@person_filter)}
;
