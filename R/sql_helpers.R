# sql_helpers.R
# Internal SQL rendering and query helpers.
# Requires DatabaseConnector + SqlRender (both in Suggests).
# Only called from the omop_connector path in connector.R.

#' Render and translate a SqlRender SQL template (internal)
#'
#' Reads a `.sql` file, substitutes `@param` tokens via
#' `SqlRender::render()`, then translates the result to the target DBMS
#' dialect via `SqlRender::translate()`.
#'
#' @param sql_path Absolute path to the `.sql` template file.
#' @param params Named list of parameter values (character or numeric).
#' @param dbms Target DBMS dialect string (e.g. `"postgresql"`, `"sql server"`,
#'   `"snowflake"`, `"bigquery"`).
#'
#' @return A single character string containing the rendered SQL.
#' @noRd
render_translate_sql <- function(sql_path, params, dbms) {
  if (!file.exists(sql_path)) {
    rlang::abort(paste0("SQL template not found: ", sql_path))
  }
  sql_raw  <- SqlRender::readSql(sql_path)
  rendered <- do.call(SqlRender::render, c(list(sql = sql_raw), params))
  SqlRender::translate(rendered, targetDialect = dbms)
}

#' Execute a parameterised SQL template against a connector (internal)
#'
#' Must be called with an `omop_connector` that has an active connection
#' (i.e. inside [with_connector()]). Renders `sql_path` with `params`,
#' translates to the connector's DBMS dialect, and returns the result as
#' a data frame.
#'
#' @param connector An `omop_connector` with `$conn` and `$dbms` set.
#' @param sql_path Path to the SqlRender `.sql` template.
#' @param params Named list of parameter substitutions.
#'
#' @return A data frame of query results (column names as returned by the
#'   DBMS -- callers should normalise with `tolower(names(df))`).
#' @noRd
query_omop <- function(connector, sql_path, params) {
  if (is.null(connector$conn)) {
    rlang::abort(
      "query_omop() requires an active connection. Use with_connector()."
    )
  }
  # SAFER/RJDBC path: the SQL is already plain Spark SQL built by
  # .fetch_drug_exposure_rjdbc(), so query_omop() is only called on the
  # DatabaseConnector path. Guard here as a safety net.
  if (isTRUE(connector$use_rjdbc)) {
    rlang::abort(paste0(
      "query_omop() should not be called on a SAFER/RJDBC connector.\n",
      "Use .fetch_drug_exposure_rjdbc() or DBI::dbGetQuery() directly."
    ))
  }
  sql <- render_translate_sql(sql_path, params, dbms = connector$dbms)
  DatabaseConnector::querySql(connector$conn, sql,
                               snakeCaseToCamelCase = FALSE)
}