# cohort.R — JSON-driven OMOP cohort selection for SteroidDoseR
#
# Two public functions:
#   fetch_cohort_ids()  — connect, run, return integer vector of person_ids
#   build_cohort_sql()  — compile a JSON cohort config into an executable SQL string
#
# Bundled cohort definitions live in inst/json/. Reference them with:
#   system.file("json", "cohort_VZV_antivirals.json", package = "SteroidDoseR")
#
# A fixed (non-parameterized) SQL version of the VZV antivirals cohort is also
# provided in inst/sql/cohort_VZV_antivirals.sql for direct use with
# DatabaseConnector::renderTranslateQuerySql().

# ---------------------------------------------------------------------------
# Public: fetch_cohort_ids
# ---------------------------------------------------------------------------

#' Fetch person IDs for a JSON-defined OMOP cohort
#'
#' Reads a JSON cohort definition, builds the appropriate SQL query, executes
#' it, and returns an integer vector of `person_id` values suitable for use
#' as `COHORT_PERSON_IDS` in `CodeToRun.R`.
#'
#' @param conn A live `DatabaseConnector` connection (from
#'   `DatabaseConnector::connect()`).
#' @param json_path Path to a JSON cohort definition file.
#'   Use [system.file()] for bundled definitions:
#'   ```r
#'   system.file("json", "cohort_VZV_antivirals.json", package = "SteroidDoseR")
#'   ```
#' @param cdm_schema `character(1)`. OMOP CDM schema (e.g. `"Myositis_OMOP.dbo"`).
#' @param vocab_schema `character(1)`. Vocabulary schema. Defaults to
#'   `cdm_schema` (common when CDM and vocabulary share a schema).
#' @param dbms `character(1)`. Target SQL dialect for `SqlRender::translate()`.
#'   Default `"sql server"`. Other values: `"postgresql"`, `"spark"`,
#'   `"redshift"`, `"bigquery"`, `"snowflake"`.
#' @param verbose `logical(1)`. If `TRUE`, prints a summary message with the
#'   cohort name and patient count. Default `TRUE`.
#'
#' @return An integer vector of `person_id` values (may be `integer(0)` if the
#'   cohort is empty).
#'
#' @examples
#' \dontrun{
#' connectionDetails <- DatabaseConnector::createConnectionDetails(
#'   dbms             = "sql server",
#'   connectionString = jdbc_url,
#'   pathToDriver     = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#' )
#' conn <- DatabaseConnector::connect(connectionDetails)
#'
#' COHORT_PERSON_IDS <- fetch_cohort_ids(
#'   conn,
#'   json_path    = system.file("json", "cohort_VZV_antivirals.json",
#'                              package = "SteroidDoseR"),
#'   cdm_schema   = "Myositis_OMOP.dbo",
#'   vocab_schema = "Myositis_OMOP.dbo"
#' )
#'
#' DatabaseConnector::disconnect(conn)
#' }
#' @export
fetch_cohort_ids <- function(conn,
                              json_path,
                              cdm_schema,
                              vocab_schema = cdm_schema,
                              dbms         = "sql server",
                              verbose      = TRUE) {
  .check_cohort_packages()

  config <- .load_cohort_config(json_path)
  sql    <- build_cohort_sql(config,
                              cdm_schema   = cdm_schema,
                              vocab_schema = vocab_schema,
                              dbms         = dbms)

  result <- DatabaseConnector::querySql(conn, sql,
                                         snakeCaseToCamelCase = FALSE)
  ids <- as.integer(result[[1L]])

  if (verbose) {
    message(sprintf(
      "[cohort] '%s' — %d person_ids selected",
      config$cohort_name %||% basename(json_path),
      length(ids)
    ))
  }
  ids
}


# ---------------------------------------------------------------------------
# Public: build_cohort_sql
# ---------------------------------------------------------------------------

#' Build a cohort SQL query from a JSON configuration
#'
#' Compiles a JSON cohort definition into a SQL string that returns one column,
#' `person_id`, for each patient in the cohort. The SQL uses CTEs and is
#' translated to the target dialect via `SqlRender::translate()`.
#'
#' @param config A list parsed from a JSON cohort definition file, **or** a
#'   path to such a file (character string). The expected structure is:
#'
#'   ```json
#'   {
#'     "cohort_name": "...",
#'     "index_event": {
#'       "domain": "condition_occurrence",
#'       "concept_ids": [123, 456],
#'       "include_descendants": true
#'     },
#'     "required_exposures": [
#'       {
#'         "name": "...",
#'         "domain": "drug_exposure",
#'         "concept_ids": [789],
#'         "include_descendants": true,
#'         "timing": "on_or_after_index"
#'       }
#'     ],
#'     "inclusion_rules": [
#'       { "type": "age_min", "value": 18 }
#'     ]
#'   }
#'   ```
#'
#'   `timing` may be `"on_or_after_index"` (default) or `"anytime"` (within
#'   observation period). Omit `required_exposures` or `inclusion_rules` to
#'   skip those criteria.
#'
#' @param cdm_schema `character(1)`. OMOP CDM schema.
#' @param vocab_schema `character(1)`. Vocabulary schema. Defaults to
#'   `cdm_schema`.
#' @param dbms `character(1)`. Target SQL dialect. Default `"sql server"`.
#'
#' @return A character string containing the ready-to-execute SQL query.
#' @export
build_cohort_sql <- function(config,
                              cdm_schema,
                              vocab_schema = cdm_schema,
                              dbms         = "sql server") {
  .check_cohort_packages()

  if (is.character(config)) config <- .load_cohort_config(config)

  # ------------------------------------------------------------------
  # 1. Index event
  # ------------------------------------------------------------------
  ie             <- config$index_event
  index_ids      <- unique(as.integer(ie$concept_ids))
  index_desc     <- isTRUE(ie$include_descendants)
  index_domain   <- ie$domain        %||% "condition_occurrence"
  index_date_col <- ie$date_field    %||% .domain_date_col(index_domain)
  index_conc_col <- ie$concept_field %||% .domain_concept_col(index_domain)

  # ------------------------------------------------------------------
  # 2. Required exposures — build one CTE + one EXISTS clause per item
  # ------------------------------------------------------------------
  req_list  <- .normalise_exposures(config$required_exposures)
  req_ctes  <- character(length(req_list))
  req_exists <- character(length(req_list))

  for (i in seq_along(req_list)) {
    req         <- req_list[[i]]
    cte_nm      <- paste0("req_concepts_", i)
    req_ids     <- unique(as.integer(req$concept_ids))
    req_desc    <- isTRUE(req$include_descendants)
    req_domain  <- req$domain        %||% "drug_exposure"
    req_date    <- req$date_field    %||% .domain_date_col(req_domain)
    req_conc    <- req$concept_field %||% .domain_concept_col(req_domain)
    timing      <- req$timing        %||% "on_or_after_index"

    req_ctes[[i]] <- .cte_concept_set(cte_nm, req_ids, req_desc, vocab_schema)

    timing_clause <- if (timing == "on_or_after_index") {
      paste0("\n    AND de.", req_date, " >= ie.index_date")
    } else {
      paste0(
        "\n    AND de.", req_date,
        " BETWEEN ie.op_start_date AND ie.op_end_date"
      )
    }

    req_exists[[i]] <- sprintf(
      paste0(
        "\n-- Required: %s\n",
        "AND EXISTS (\n",
        "  SELECT 1\n",
        "  FROM %s.%s de\n",
        "  JOIN %s rc%d ON de.%s = rc%d.concept_id\n",
        "  WHERE de.person_id = ie.person_id%s\n",
        ")"
      ),
      req$name %||% paste0("exposure_", i),
      cdm_schema, req_domain,
      cte_nm, i, req_conc, i,
      timing_clause
    )
  }

  # ------------------------------------------------------------------
  # 3. Inclusion rules
  # ------------------------------------------------------------------
  age_min    <- NULL
  extra_where <- character(0)

  rules <- .normalise_rules(config$inclusion_rules)
  for (rule in rules) {
    if (identical(rule$type, "age_min")) {
      age_min <- as.integer(rule$value)
    }
  }

  age_join  <- if (!is.null(age_min))
    sprintf("\nJOIN %s.person p ON p.person_id = ie.person_id", cdm_schema)
  else ""

  age_where <- if (!is.null(age_min))
    sprintf("\nAND YEAR(ie.index_date) - p.year_of_birth >= %d", age_min)
  else ""

  # ------------------------------------------------------------------
  # 4. Assemble full SQL
  # ------------------------------------------------------------------
  all_ctes <- c(
    .cte_concept_set("index_concepts", index_ids, index_desc, vocab_schema),
    req_ctes,
    sprintf(
      paste0(
        "index_events AS (\n",
        "  SELECT\n",
        "    t.person_id,\n",
        "    MIN(t.%s)                             AS index_date,\n",
        "    MIN(op.observation_period_start_date) AS op_start_date,\n",
        "    MAX(op.observation_period_end_date)   AS op_end_date\n",
        "  FROM %s.%s t\n",
        "  JOIN index_concepts ic ON t.%s = ic.concept_id\n",
        "  JOIN %s.observation_period op\n",
        "    ON  t.person_id = op.person_id\n",
        "    AND t.%s BETWEEN op.observation_period_start_date\n",
        "                 AND op.observation_period_end_date\n",
        "  GROUP BY t.person_id\n",
        ")"
      ),
      index_date_col,
      cdm_schema, index_domain,
      index_conc_col,
      cdm_schema,
      index_date_col
    )
  )

  sql <- paste0(
    "WITH\n\n",
    paste(all_ctes, collapse = ",\n\n"),
    "\n\nSELECT DISTINCT ie.person_id\n",
    "FROM index_events ie",
    age_join, "\n",
    "WHERE 1 = 1",
    age_where,
    paste(req_exists, collapse = "")
  )

  SqlRender::translate(sql, targetDialect = dbms)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build a single CTE that resolves a concept set (with optional descendants)
#' @noRd
.cte_concept_set <- function(cte_name, concept_ids, include_descendants,
                               vocab_schema) {
  ids <- paste(unique(as.integer(concept_ids)), collapse = ", ")

  base <- sprintf(
    paste0(
      "%s AS (\n",
      "  SELECT DISTINCT concept_id\n",
      "  FROM %s.concept\n",
      "  WHERE concept_id IN (%s)"
    ),
    cte_name, vocab_schema, ids
  )

  if (isTRUE(include_descendants)) {
    base <- paste0(base, sprintf(
      paste0(
        "\n  UNION\n",
        "  SELECT DISTINCT ca.descendant_concept_id\n",
        "  FROM %s.concept_ancestor ca\n",
        "  JOIN %s.concept c ON ca.descendant_concept_id = c.concept_id\n",
        "  WHERE ca.ancestor_concept_id IN (%s)\n",
        "    AND c.invalid_reason IS NULL"
      ),
      vocab_schema, vocab_schema, ids
    ))
  }

  paste0(base, "\n)")
}

#' Default date column name for common OMOP domains
#' @noRd
.domain_date_col <- function(domain) {
  switch(domain,
    condition_occurrence = "condition_start_date",
    drug_exposure        = "drug_exposure_start_date",
    measurement          = "measurement_date",
    observation          = "observation_date",
    visit_occurrence     = "visit_start_date",
    procedure_occurrence = "procedure_date",
    "start_date"
  )
}

#' Default concept_id column name for common OMOP domains
#' @noRd
.domain_concept_col <- function(domain) {
  switch(domain,
    condition_occurrence = "condition_concept_id",
    drug_exposure        = "drug_concept_id",
    measurement          = "measurement_concept_id",
    observation          = "observation_concept_id",
    visit_occurrence     = "visit_concept_id",
    procedure_occurrence = "procedure_concept_id",
    "concept_id"
  )
}

#' Normalise required_exposures regardless of how jsonlite parsed the array
#' jsonlite may return a data.frame (homogeneous arrays) or a list of lists.
#' @noRd
.normalise_exposures <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())
  if (is.data.frame(x)) {
    lapply(seq_len(nrow(x)), function(i) {
      row <- as.list(x[i, , drop = FALSE])
      # concept_ids may be a list-column; unlist it
      row$concept_ids <- unlist(row$concept_ids)
      row
    })
  } else {
    x
  }
}

#' Normalise inclusion_rules regardless of jsonlite parse format
#' @noRd
.normalise_rules <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())
  if (is.data.frame(x)) lapply(seq_len(nrow(x)), function(i) as.list(x[i, ]))
  else x
}

#' Load and validate a JSON cohort config file
#' @noRd
.load_cohort_config <- function(json_path) {
  if (!file.exists(json_path)) {
    rlang::abort(sprintf("Cohort JSON file not found: %s", json_path))
  }
  config <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)

  if (is.null(config$index_event) || length(config$index_event$concept_ids) == 0L) {
    rlang::abort("Cohort JSON must have 'index_event.concept_ids' with at least one concept ID.")
  }
  config
}

#' Guard: check that required packages are available
#' @noRd
.check_cohort_packages <- function() {
  for (pkg in c("jsonlite", "DatabaseConnector", "SqlRender")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf(
        "Package '%s' is required. Install with: install.packages('%s')", pkg, pkg
      ))
    }
  }
}
