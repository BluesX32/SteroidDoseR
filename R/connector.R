# connector.R
# Lightweight S3 abstraction over OMOP CDM data sources.
#
# Two connector types are provided:
#   - omop_connector : connects to a live OMOP CDM database via DatabaseConnector
#   - df_connector   : wraps an in-memory data.frame (used for tests and vignettes)
#
# Both types satisfy the same interface so all SteroidDoseR algorithms are
# database-agnostic. Only the omop_connector path requires DatabaseConnector +
# SqlRender to be installed; the df_connector path has no extra dependencies.

# ---------------------------------------------------------------------------
# Dependency guard
# ---------------------------------------------------------------------------

.check_db_packages <- function() {
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'DatabaseConnector' is required for OMOP CDM connectivity.\n",
      "Install it with: install.packages('DatabaseConnector')"
    ))
  }
  if (!requireNamespace("SqlRender", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'SqlRender' is required for cross-DBMS SQL translation.\n",
      "Install it with: install.packages('SqlRender')"
    ))
  }
}

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------

#' Create an OMOP CDM connector for live database access
#'
#' Constructs a connector object that holds connection details and schema
#' information for a live OMOP CDM database. The actual database connection
#' is opened lazily — only when a query is executed — and closed immediately
#' after via [with_connector()].
#'
#' Requires the **DatabaseConnector** and **SqlRender** packages (both on
#' CRAN). Connection details are created with
#' `DatabaseConnector::createConnectionDetails()`.
#'
#' @param connectionDetails A `connectionDetails` object created by
#'   `DatabaseConnector::createConnectionDetails()`.
#' @param cdm_schema `character(1)`. Schema containing the OMOP CDM tables
#'   (`drug_exposure`, `concept`, etc.).
#' @param vocab_schema `character(1)` or `NULL`. Schema containing OMOP
#'   vocabulary tables. Defaults to `cdm_schema`.
#' @param results_schema `character(1)` or `NULL`. Schema for cohort/results
#'   tables. Only needed if cohort-based filtering is used.
#' @param temp_schema `character(1)` or `NULL`. Schema for temp tables.
#'   Required on some DBMS (e.g. SQL Server) for `#temp` table creation.
#' @param cdm_version `character(1)`. OMOP CDM version. Default `"5.4"`.
#'
#' @return An object of class `c("omop_connector", "steroid_connector")`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cd <- DatabaseConnector::createConnectionDetails(
#'   dbms     = "postgresql",
#'   server   = "myserver/omop",
#'   user     = "omop_user",
#'   password = Sys.getenv("DB_PASSWORD"),
#'   port     = 5432
#' )
#' con <- create_omop_connector(cd, cdm_schema = "cdm_531")
#' doses <- calc_daily_dose_baseline(con, start_date = "2020-01-01")
#' }
create_omop_connector <- function(connectionDetails,
                                   cdm_schema,
                                   vocab_schema   = NULL,
                                   results_schema = NULL,
                                   temp_schema    = NULL,
                                   cdm_version    = "5.4") {
  if (missing(connectionDetails) || is.null(connectionDetails)) {
    rlang::abort("'connectionDetails' must be a DatabaseConnector connectionDetails object.")
  }
  if (missing(cdm_schema) || !nzchar(cdm_schema)) {
    rlang::abort("'cdm_schema' must be a non-empty character string.")
  }

  structure(
    list(
      type               = "omop",
      connectionDetails  = connectionDetails,
      cdm_schema         = cdm_schema,
      vocab_schema       = vocab_schema  %||% cdm_schema,
      results_schema     = results_schema,
      temp_schema        = temp_schema,
      cdm_version        = cdm_version,
      conn               = NULL,   # set by with_connector()
      dbms               = NULL,   # set by with_connector()
      capabilities       = NULL    # set by detect_capabilities()
    ),
    class = c("omop_connector", "steroid_connector")
  )
}

#' Create an in-memory data-frame connector (for tests and vignettes)
#'
#' Wraps a pre-constructed data frame in the standard connector interface.
#' No database or extra packages required. Suitable for unit tests and
#' vignette examples using synthetic data.
#'
#' The data frame must follow the [drug_df_contract]: at minimum it must
#' contain `person_id` and `drug_exposure_start_date`. All other columns are
#' optional (capabilities are auto-detected from column presence).
#'
#' @param drug_df A data frame matching (at least partially) the OMOP
#'   `drug_exposure` domain columns. See [drug_df_contract] for the full
#'   column list.
#'
#' @return An object of class `c("df_connector", "steroid_connector")`.
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id                = 1L,
#'   drug_concept_name        = "prednisone 5 MG oral tablet",
#'   drug_exposure_start_date = as.Date("2023-01-01"),
#'   drug_exposure_end_date   = as.Date("2023-03-01"),
#'   sig                      = "Take 1 tab daily",
#'   quantity                 = 90,
#'   days_supply              = 90,
#'   amount_value             = 5
#' )
#' con <- create_df_connector(df)
#' calc_daily_dose_baseline(con)
create_df_connector <- function(drug_df) {
  if (!is.data.frame(drug_df)) {
    rlang::abort("'drug_df' must be a data.frame.")
  }
  assert_required_cols(drug_df, c("person_id", "drug_exposure_start_date"), "drug_df")

  caps <- list(
    has_sig          = "sig"              %in% names(drug_df),
    has_days_supply  = "days_supply"      %in% names(drug_df),
    has_quantity     = "quantity"         %in% names(drug_df),
    has_route        = any(c("route_concept_id", "route_concept_name",
                             "route_source_value") %in% names(drug_df)),
    has_drug_source  = "drug_source_value" %in% names(drug_df),
    has_drug_concept = "drug_concept_id"   %in% names(drug_df)
  )

  structure(
    list(
      type         = "df",
      drug_df      = tibble::as_tibble(drug_df),
      capabilities = caps
    ),
    class = c("df_connector", "steroid_connector")
  )
}

# ---------------------------------------------------------------------------
# S3 print methods
# ---------------------------------------------------------------------------

#' @export
print.omop_connector <- function(x, ...) {
  cat("<omop_connector>\n")
  cat("  CDM schema    :", x$cdm_schema, "\n")
  cat("  CDM version   :", x$cdm_version, "\n")
  cat("  Connected     :", if (!is.null(x$conn)) "yes" else "no", "\n")
  if (!is.null(x$capabilities)) {
    caps <- x$capabilities
    cat("  Capabilities  :",
        paste(names(caps)[unlist(caps)], collapse = ", "), "\n")
  }
  invisible(x)
}

#' @export
print.df_connector <- function(x, ...) {
  cat("<df_connector>\n")
  cat("  Rows          :", nrow(x$drug_df), "\n")
  cat("  Persons       :", length(unique(x$drug_df$person_id)), "\n")
  caps <- x$capabilities
  cat("  Capabilities  :", paste(names(caps)[unlist(caps)], collapse = ", "), "\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Connection lifecycle
# ---------------------------------------------------------------------------

#' Execute a function within a managed database connection
#'
#' For `omop_connector`: opens a connection, runs `fn(connector)`, then
#' closes the connection — even if `fn` throws an error.
#'
#' For `df_connector`: runs `fn(connector)` directly (no connection needed).
#'
#' @param connector A `steroid_connector` object.
#' @param fn A function that accepts a single argument (the connector, with
#'   `$conn` set for omop connectors).
#' @param ... Additional arguments forwarded to `fn`.
#'
#' @return The return value of `fn(connector, ...)`.
#'
#' @export
with_connector <- function(connector, fn, ...) {
  UseMethod("with_connector")
}

#' @export
with_connector.omop_connector <- function(connector, fn, ...) {
  .check_db_packages()
  conn <- DatabaseConnector::connect(connector$connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  # Attach the active connection + dialect to a local copy of the connector
  active <- connector
  active$conn <- conn
  active$dbms <- DatabaseConnector::dbms(conn)

  fn(active, ...)
}

#' @export
with_connector.df_connector <- function(connector, fn, ...) {
  fn(connector, ...)
}

# ---------------------------------------------------------------------------
# Capability detection
# ---------------------------------------------------------------------------

#' Detect which drug_exposure fields are available in the data source
#'
#' Probes the data source and returns a named logical list indicating whether
#' each optional OMOP `drug_exposure` field is present. The result is stored
#' in `connector$capabilities` and used internally to select imputation
#' methods and fallback strategies.
#'
#' For `omop_connector`: executes lightweight `WHERE 1=0` probe queries
#' against the live database.
#'
#' For `df_connector`: inspects the column names of the wrapped data frame.
#'
#' @param connector A `steroid_connector` object.
#'
#' @return A modified `connector` with `$capabilities` populated:
#' \describe{
#'   \item{`has_sig`}{`TRUE` if `drug_exposure.sig` is available.}
#'   \item{`has_days_supply`}{`TRUE` if `days_supply` is available.}
#'   \item{`has_quantity`}{`TRUE` if `quantity` is available.}
#'   \item{`has_route`}{`TRUE` if `route_concept_id` is available.}
#'   \item{`has_drug_source`}{`TRUE` if `drug_source_value` is available.}
#'   \item{`has_drug_concept`}{`TRUE` if `drug_concept_id` is available.}
#' }
#'
#' @export
detect_capabilities <- function(connector) {
  UseMethod("detect_capabilities")
}

#' @export
detect_capabilities.df_connector <- function(connector) {
  df   <- connector$drug_df
  cols <- names(df)
  connector$capabilities <- list(
    has_sig          = "sig"               %in% cols,
    has_days_supply  = "days_supply"       %in% cols,
    has_quantity     = "quantity"          %in% cols,
    has_route        = any(c("route_concept_id", "route_concept_name",
                             "route_source_value") %in% cols),
    has_drug_source  = "drug_source_value" %in% cols,
    has_drug_concept = "drug_concept_id"   %in% cols
  )
  connector
}

#' @export
detect_capabilities.omop_connector <- function(connector) {
  .check_db_packages()

  probe_field <- function(active_con, field) {
    sql <- SqlRender::render(
      "SELECT @field FROM @cdm_schema.drug_exposure WHERE 1 = 0",
      field      = field,
      cdm_schema = active_con$cdm_schema
    )
    sql <- SqlRender::translate(sql, targetDialect = active_con$dbms)
    tryCatch({
      DatabaseConnector::querySql(active_con$conn, sql,
                                   snakeCaseToCamelCase = FALSE)
      TRUE
    }, error = function(e) FALSE)
  }

  caps <- with_connector(connector, function(active) {
    list(
      has_sig          = probe_field(active, "sig"),
      has_days_supply  = probe_field(active, "days_supply"),
      has_quantity     = probe_field(active, "quantity"),
      has_route        = probe_field(active, "route_concept_id"),
      has_drug_source  = probe_field(active, "drug_source_value"),
      has_drug_concept = probe_field(active, "drug_concept_id")
    )
  })

  connector$capabilities <- caps
  connector
}

# ---------------------------------------------------------------------------
# Data extraction
# ---------------------------------------------------------------------------

#' Fetch drug exposure records from a connector (internal)
#'
#' Dispatches on connector type. For `omop_connector`, renders and executes
#' `inst/sql/extract_drug_exposure.sql`. For `df_connector`, optionally
#' filters the stored data frame.
#'
#' This function is always called inside [with_connector()] so that
#' `omop_connector$conn` is guaranteed to be set.
#'
#' @param connector A `steroid_connector` (with `$conn` set for omop type).
#' @param drug_concept_ids Integer vector of `drug_concept_id` values to
#'   include, or `NULL` to include all.
#' @param person_ids Vector of `person_id` values to include, or `NULL`.
#' @param start_date Character/Date lower bound on `drug_exposure_start_date`.
#'   Default `"1900-01-01"`.
#' @param end_date Character/Date upper bound on `drug_exposure_start_date`.
#'   Default today.
#' @param sig_source `character(1)`. Which column to use as the SIG text.
#'   `"sig"` (default) uses `drug_exposure.sig`. `"drug_source_value"` aliases
#'   `drug_source_value` into `sig` when the native SIG column is absent or
#'   all-NA.
#'
#' @return A tibble conforming to the [drug_df_contract].
#' @noRd
fetch_drug_exposure <- function(connector,
                                 drug_concept_ids = NULL,
                                 person_ids       = NULL,
                                 start_date       = NULL,
                                 end_date         = NULL,
                                 sig_source       = "sig") {
  if (inherits(connector, "df_connector")) {
    return(.fetch_drug_exposure_df(connector, drug_concept_ids, person_ids,
                                   start_date, end_date, sig_source))
  }
  if (inherits(connector, "omop_connector")) {
    return(.fetch_drug_exposure_omop(connector, drug_concept_ids, person_ids,
                                     start_date, end_date, sig_source))
  }
  rlang::abort("connector must be a df_connector or omop_connector.")
}

.fetch_drug_exposure_df <- function(connector,
                                     drug_concept_ids = NULL,
                                     person_ids       = NULL,
                                     start_date       = NULL,
                                     end_date         = NULL,
                                     sig_source       = "sig") {
  df <- connector$drug_df

  if (!is.null(person_ids)) {
    df <- df[df$person_id %in% person_ids, , drop = FALSE]
  }
  if (!is.null(drug_concept_ids) && "drug_concept_id" %in% names(df)) {
    df <- df[df$drug_concept_id %in% drug_concept_ids, , drop = FALSE]
  }
  if (!is.null(start_date) && "drug_exposure_start_date" %in% names(df)) {
    df <- df[df$drug_exposure_start_date >= as.Date(start_date), , drop = FALSE]
  }
  if (!is.null(end_date) && "drug_exposure_start_date" %in% names(df)) {
    df <- df[df$drug_exposure_start_date <= as.Date(end_date), , drop = FALSE]
  }

  df <- .apply_sig_source(df, sig_source)
  tibble::as_tibble(df)
}

.fetch_drug_exposure_omop <- function(connector,
                                                drug_concept_ids = NULL,
                                                person_ids       = NULL,
                                                start_date       = NULL,
                                                end_date         = NULL,
                                                sig_source       = "sig") {
  # connector$conn and connector$dbms are set by with_connector()
  if (is.null(connector$conn)) {
    rlang::abort(
      "fetch_drug_exposure() must be called inside with_connector()."
    )
  }

  sd <- if (is.null(start_date)) "1900-01-01" else format(as.Date(start_date), "%Y-%m-%d")
  ed <- if (is.null(end_date))   format(Sys.Date(), "%Y-%m-%d") else format(as.Date(end_date), "%Y-%m-%d")

  concept_filter <- if (!is.null(drug_concept_ids))
    paste(as.integer(drug_concept_ids), collapse = ",")
  else ""

  person_filter  <- if (!is.null(person_ids))
    paste(person_ids, collapse = ",")
  else ""

  sql_path <- system.file("sql", "extract_drug_exposure.sql",
                           package = "SteroidDoseR")

  df <- query_omop(connector, sql_path, list(
    cdm_schema     = connector$cdm_schema,
    start_date     = sd,
    end_date       = ed,
    concept_filter = concept_filter,
    person_filter  = person_filter
  ))

  # DatabaseConnector may return uppercase column names; normalise
  names(df) <- tolower(names(df))

  # Date coercion
  for (dcol in c("drug_exposure_start_date", "drug_exposure_end_date")) {
    if (dcol %in% names(df)) df[[dcol]] <- as.Date(df[[dcol]])
  }

  df <- .apply_sig_source(df, sig_source)
  tibble::as_tibble(df)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Resolve first argument: returns a drug_df regardless of input type.
.resolve_drug_df <- function(connector_or_df,
                              drug_concept_ids = NULL,
                              person_ids       = NULL,
                              start_date       = NULL,
                              end_date         = NULL,
                              sig_source       = "sig") {

  if (is.data.frame(connector_or_df)) {
    return(tibble::as_tibble(connector_or_df))
  }

  if (!inherits(connector_or_df, "steroid_connector")) {
    rlang::abort(paste0(
      "First argument must be a data.frame, df_connector, or omop_connector. ",
      "Got: ", class(connector_or_df)[[1L]]
    ))
  }

  if (inherits(connector_or_df, "omop_connector")) {
    return(with_connector(connector_or_df, function(active) {
      fetch_drug_exposure(active,
                          drug_concept_ids = drug_concept_ids,
                          person_ids       = person_ids,
                          start_date       = start_date,
                          end_date         = end_date,
                          sig_source       = sig_source)
    }))
  }

  # df_connector — no connection lifecycle needed
  fetch_drug_exposure(connector_or_df,
                      drug_concept_ids = drug_concept_ids,
                      person_ids       = person_ids,
                      start_date       = start_date,
                      end_date         = end_date,
                      sig_source       = sig_source)
}

# Alias drug_source_value into sig when sig_source != "sig" or sig is absent.
.apply_sig_source <- function(df, sig_source) {
  if (sig_source == "drug_source_value") {
    if (!"sig" %in% names(df) || all(is.na(df[["sig"]]))) {
      if ("drug_source_value" %in% names(df)) {
        df[["sig"]] <- df[["drug_source_value"]]
      }
    }
  }
  df
}

# NULL-coalescing operator (used above)
`%||%` <- function(a, b) if (!is.null(a)) a else b