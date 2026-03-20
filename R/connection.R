# connection.R
# High-level OMOP CDM connection helpers.
#
# create_omop_connection() is the primary entry point: it reads all settings
# from environment variables (or accepts explicit arguments), builds a
# platform-appropriate JDBC connection string, and returns a live
# DatabaseConnector connection with schema metadata stored as attributes.
#
# Platform-specific builders:
#   create_sqlserver_connection()  — SQL Server + Windows AD / NTLM
#   create_databricks_connection() — Databricks / Spark with Arrow option
#
# Convenience wrapper:
#   create_connection_from_env()   — read everything from a .env file

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Flexible Database Connection Management
#'
#' Functions to create database connections supporting multiple platforms
#' including SQL Server (with Windows AD authentication), Databricks,
#' PostgreSQL, and more.
#'
#' @name create_connection
NULL

#' Create a database connection with platform-specific optimisations
#'
#' Supports multiple database platforms with appropriate configurations:
#' - SQL Server: Standard auth and Windows AD (Integrated Security)
#' - Databricks/Spark: JDBC with Arrow optimisation
#' - PostgreSQL/Redshift: Standard JDBC connections
#' - BigQuery: For All of Us environment
#'
#' All parameters can be omitted when `use_env = TRUE` (the default); the
#' function then reads them from the environment variables listed below.
#'
#' ## Environment variables
#'
#' | Variable | Description |
#' |---|---|
#' | `SQL_DBMS` / `DB_TYPE` / `OMOP_ENV` | DBMS type (default: `"sql server"`) |
#' | `SQL_SERVER` / `DB_SERVER` | Server address |
#' | `SQL_DATABASE` / `DB_DATABASE` | Database name |
#' | `SQL_PORT` / `DB_PORT` | Port number |
#' | `SQL_USER` / `DB_USER` | Username |
#' | `SQL_PASSWORD` / `DB_PASSWORD` | Password |
#' | `SQL_JDBC_PATH` / `JDBC_DRIVER_PATH` | Path to JDBC driver folder |
#' | `SQL_CDM_SCHEMA` / `CDM_SCHEMA` | CDM schema (default: `"dbo"`) |
#' | `SQL_VOCABULARY_SCHEMA` / `VOCABULARY_SCHEMA` | Vocabulary schema |
#' | `SQL_RESULTS_SCHEMA` / `RESULTS_SCHEMA` | Results schema |
#' | `USE_WINDOWS_AUTH` | `"true"` / `"1"` / `"yes"` to use Windows AD auth |
#' | `SQL_CDM_DATABASE` | Prepended to CDM schema as `database.schema` |
#' | `DB_EXTRA_SETTINGS` | Extra JDBC settings string (Databricks `HTTPPath`) |
#' | `ENABLE_ARROW` | `"TRUE"` to enable Databricks Arrow optimisation |
#'
#' @param dbms Character: `"sql server"`, `"spark"`, `"databricks"`,
#'   `"postgresql"`, `"redshift"`, etc.
#' @param server Server name or address.
#' @param database Database name.
#' @param port Port number (optional; defaults are applied per platform).
#' @param user Username (optional for Windows auth).
#' @param password Password (optional for Windows auth).
#' @param use_windows_auth Logical. Use Windows AD authentication (SQL Server
#'   only). Default `FALSE`.
#' @param connectionString Full JDBC connection string (optional; skips
#'   auto-build).
#' @param pathToDriver Path to the JDBC driver folder.
#' @param cdm_schema CDM schema name.
#' @param vocabulary_schema Vocabulary schema (defaults to `cdm_schema`).
#' @param results_schema Results schema (optional).
#' @param extraSettings Additional JDBC settings string (e.g., Databricks
#'   `httpPath`).
#' @param use_env Logical. Load unset parameters from environment variables.
#'   Default `TRUE`.
#'
#' @return A `DatabaseConnectorConnection` object (from
#'   `DatabaseConnector::connect()`) with the following attributes set:
#'   `dbms`, `cdm_schema`, `vocabulary_schema`, `results_schema`, `server`,
#'   `database`. Pass this directly to `calc_daily_dose_baseline()`,
#'   `run_pipeline()`, etc.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # SQL Server with Windows AD authentication
#' con <- create_omop_connection(
#'   dbms             = "sql server",
#'   server           = "server.domain.com",
#'   database         = "OMOP_CDM",
#'   use_windows_auth = TRUE,
#'   cdm_schema       = "dbo"
#' )
#'
#' # SQL Server with standard authentication
#' con <- create_omop_connection(
#'   dbms       = "sql server",
#'   server     = "server.domain.com",
#'   database   = "OMOP_CDM",
#'   user       = "username",
#'   password   = "password",
#'   cdm_schema = "dbo"
#' )
#'
#' # Databricks with token authentication
#' con <- create_omop_connection(
#'   dbms          = "databricks",
#'   server        = "workspace.cloud.databricks.com",
#'   database      = "default",
#'   user          = "token",
#'   password      = Sys.getenv("DATABRICKS_TOKEN"),
#'   cdm_schema    = "omop.data",
#'   extraSettings = "httpPath=/sql/1.0/warehouses/warehouse_id"
#' )
#'
#' # Simplest: load everything from environment variables / .env file
#' con <- create_connection_from_env(".env")
#'
#' # Use the connection with SteroidDoseR pipeline functions
#' episodes <- run_pipeline(con, method = "baseline")
#' DatabaseConnector::disconnect(con)
#' }
create_omop_connection <- function(
    dbms              = NULL,
    server            = NULL,
    database          = NULL,
    port              = NULL,
    user              = NULL,
    password          = NULL,
    use_windows_auth  = FALSE,
    connectionString  = NULL,
    pathToDriver      = NULL,
    cdm_schema        = NULL,
    vocabulary_schema = NULL,
    results_schema    = NULL,
    extraSettings     = NULL,
    use_env           = TRUE
) {
  .check_db_packages()

  # ------------------------------------------------------------------
  # Load unset parameters from environment variables
  # ------------------------------------------------------------------
  if (use_env) {
    if (is.null(dbms)) {
      dbms <- Sys.getenv("SQL_DBMS")
      if (!nzchar(dbms)) dbms <- Sys.getenv("DB_TYPE")
      if (!nzchar(dbms)) dbms <- Sys.getenv("OMOP_ENV")
      if (!nzchar(dbms)) dbms <- "sql server"
    }

    if (is.null(server)) {
      server <- Sys.getenv("SQL_SERVER")
      if (!nzchar(server)) server <- Sys.getenv("DB_SERVER")
    }

    if (is.null(database)) {
      database <- Sys.getenv("SQL_DATABASE")
      if (!nzchar(database)) database <- Sys.getenv("DB_DATABASE")
    }

    if (is.null(port)) {
      port_str <- Sys.getenv("SQL_PORT")
      if (!nzchar(port_str)) port_str <- Sys.getenv("DB_PORT")
      if (nzchar(port_str)) port <- as.numeric(port_str)
    }

    if (is.null(user) && !use_windows_auth) {
      user <- Sys.getenv("SQL_USER")
      if (!nzchar(user)) user <- Sys.getenv("DB_USER")
      if (!nzchar(user)) user <- NULL
    }

    if (is.null(password) && !use_windows_auth) {
      password <- Sys.getenv("SQL_PASSWORD")
      if (!nzchar(password)) password <- Sys.getenv("DB_PASSWORD")
      if (!nzchar(password)) password <- NULL
    }

    if (is.null(pathToDriver)) {
      pathToDriver <- Sys.getenv("SQL_JDBC_PATH")
      if (!nzchar(pathToDriver)) pathToDriver <- Sys.getenv("JDBC_DRIVER_PATH")
      if (!nzchar(pathToDriver)) pathToDriver <- "jdbc_drivers"
    }

    if (is.null(cdm_schema)) {
      cdm_schema <- Sys.getenv("SQL_CDM_SCHEMA")
      if (!nzchar(cdm_schema)) cdm_schema <- Sys.getenv("CDM_SCHEMA")
      if (!nzchar(cdm_schema)) cdm_schema <- "dbo"
    }

    if (is.null(vocabulary_schema)) {
      vocabulary_schema <- Sys.getenv("SQL_VOCABULARY_SCHEMA")
      if (!nzchar(vocabulary_schema)) vocabulary_schema <- Sys.getenv("VOCABULARY_SCHEMA")
      if (!nzchar(vocabulary_schema)) vocabulary_schema <- NULL
    }

    if (is.null(results_schema)) {
      results_schema <- Sys.getenv("SQL_RESULTS_SCHEMA")
      if (!nzchar(results_schema)) results_schema <- Sys.getenv("RESULTS_SCHEMA")
      if (!nzchar(results_schema)) results_schema <- NULL
    }

    if (is.null(extraSettings)) {
      extraSettings <- Sys.getenv("DB_EXTRA_SETTINGS")
      if (!nzchar(extraSettings)) extraSettings <- NULL
    }

    # Windows auth flag from env
    env_win_auth <- Sys.getenv("USE_WINDOWS_AUTH", "false")
    if (tolower(env_win_auth) %in% c("true", "1", "yes")) {
      use_windows_auth <- TRUE
      if (is.null(user) || !nzchar(user %||% "")) {
        win_user <- Sys.getenv("SQL_WINDOWS_USER")
        if (nzchar(win_user)) user <- win_user
      }
      if (is.null(password) || !nzchar(password %||% "")) {
        win_pw <- Sys.getenv("SQL_WINDOWS_PASSWORD")
        if (nzchar(win_pw)) password <- win_pw
      }
    }

    # Combine separate database + schema env vars (e.g. "MyDB" + "dbo" → "MyDB.dbo")
    cdm_database <- Sys.getenv("SQL_CDM_DATABASE")
    results_database <- Sys.getenv("SQL_RESULTS_DATABASE")

    if (nzchar(cdm_database) && !grepl("\\.", cdm_schema)) {
      cdm_schema        <- paste0(cdm_database, ".", cdm_schema)
      if (!is.null(vocabulary_schema) && !grepl("\\.", vocabulary_schema))
        vocabulary_schema <- paste0(cdm_database, ".", vocabulary_schema)
    }
    if (nzchar(results_database) && !is.null(results_schema) &&
        !grepl("\\.", results_schema)) {
      results_schema <- paste0(results_database, ".", results_schema)
    }
  }

  # ------------------------------------------------------------------
  # Validate
  # ------------------------------------------------------------------
  if (is.null(server) || !nzchar(server)) {
    rlang::abort(
      "Server is required. Set SQL_SERVER in .env or supply the `server` argument."
    )
  }

  # ------------------------------------------------------------------
  # Normalise DBMS name + default port
  # ------------------------------------------------------------------
  dbms <- tolower(dbms)
  if (dbms == "databricks") dbms <- "spark"

  if (is.null(port)) {
    port <- switch(dbms,
      "sql server" = 1433L,
      "postgresql" = 5432L,
      "spark"      = 443L,
      "oracle"     = 1521L,
      "redshift"   = 5439L,
      NULL
    )
  }

  if (is.null(vocabulary_schema)) vocabulary_schema <- cdm_schema

  # ------------------------------------------------------------------
  # Build platform-specific connectionDetails
  # ------------------------------------------------------------------
  connectionDetails <- if (dbms == "sql server") {
    .create_sqlserver_connection(
      server           = server,
      database         = database,
      port             = port,
      user             = user,
      password         = password,
      use_windows_auth = use_windows_auth,
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    )
  } else if (dbms == "spark") {
    .create_databricks_connection(
      server           = server,
      database         = database,
      port             = port,
      user             = user,
      password         = password,
      connectionString = connectionString,
      pathToDriver     = pathToDriver,
      extraSettings    = extraSettings
    )
  } else {
    DatabaseConnector::createConnectionDetails(
      dbms             = dbms,
      server           = if (!is.null(server)) paste0(server, "/", database) else NULL,
      port             = port,
      user             = user,
      password         = password,
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    )
  }

  # ------------------------------------------------------------------
  # Open connection and attach metadata
  # ------------------------------------------------------------------
  message(sprintf("Connecting to %s database...", dbms))
  connection <- DatabaseConnector::connect(connectionDetails)

  attr(connection, "dbms")               <- dbms
  attr(connection, "cdm_schema")         <- cdm_schema
  attr(connection, "vocabulary_schema")  <- vocabulary_schema
  attr(connection, "results_schema")     <- results_schema
  attr(connection, "server")             <- server
  attr(connection, "database")           <- database

  if (dbms == "spark") {
    .configure_spark_connection(connection, results_schema)
  }

  message("\u2713 Connection successful")
  connection
}

#' Create connection from environment variables or a .env file
#'
#' Loads settings from a `.env` file (if it exists) then calls
#' [create_omop_connection()] with `use_env = TRUE`. This is the simplest
#' entry point when all configuration is stored in environment variables.
#'
#' @param env_file Path to the `.env` file. Default `".env"` (project root).
#'
#' @return A `DatabaseConnectorConnection` object (see [create_omop_connection()]).
#' @export
create_connection_from_env <- function(env_file = ".env") {
  if (file.exists(env_file)) {
    .load_env_file(env_file)
    message("\u2713 Loaded environment variables from ", env_file)
  }
  create_omop_connection(use_env = TRUE)
}

# ---------------------------------------------------------------------------
# Internal platform builders
# ---------------------------------------------------------------------------

#' Build connectionDetails for SQL Server (supports Windows AD / NTLM)
#' @noRd
.create_sqlserver_connection <- function(server,
                                         database,
                                         port,
                                         user,
                                         password,
                                         use_windows_auth,
                                         connectionString,
                                         pathToDriver) {
  if (!is.null(connectionString)) {
    return(DatabaseConnector::createConnectionDetails(
      dbms             = "sql server",
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    ))
  }

  jdbc_url <- sprintf(
    paste0("jdbc:sqlserver://%s:%d;database=%s;",
           "encrypt=true;trustServerCertificate=true;"),
    server, port, database
  )

  if (use_windows_auth) {
    if (!is.null(user) && nzchar(user) && !is.null(password) && nzchar(password)) {
      # NTLM with explicit domain credentials (cross-platform / domain auth)
      jdbc_url <- paste0(
        jdbc_url,
        "integratedSecurity=false;authenticationScheme=NTLM;"
      )
      message("  Using Windows AD with NTLM authentication")
      DatabaseConnector::createConnectionDetails(
        dbms             = "sql server",
        connectionString = jdbc_url,
        user             = user,
        password         = password,
        pathToDriver     = pathToDriver
      )
    } else {
      # True integrated security (Windows host, proper sqljdbc_auth.dll setup)
      jdbc_url <- paste0(jdbc_url, "integratedSecurity=true;")
      message("  Using Windows integrated security")
      DatabaseConnector::createConnectionDetails(
        dbms             = "sql server",
        connectionString = jdbc_url,
        pathToDriver     = pathToDriver
      )
    }
  } else {
    DatabaseConnector::createConnectionDetails(
      dbms             = "sql server",
      connectionString = jdbc_url,
      user             = user,
      password         = password,
      pathToDriver     = pathToDriver
    )
  }
}

#' Build connectionDetails for Databricks / Spark
#' @noRd
.create_databricks_connection <- function(server,
                                          database,
                                          port,
                                          user,
                                          password,
                                          connectionString,
                                          pathToDriver,
                                          extraSettings) {
  # Ensure rJava is initialised (needed for Arrow memory management)
  if (requireNamespace("rJava", quietly = TRUE)) {
    tryCatch(rJava::.jinit(), error = function(e) invisible(NULL))
  }

  if (!is.null(connectionString)) {
    return(DatabaseConnector::createConnectionDetails(
      dbms             = "spark",
      connectionString = connectionString,
      pathToDriver     = pathToDriver
    ))
  }

  jdbc_url <- sprintf("jdbc:databricks://%s:%d;", server, port)

  if (!is.null(extraSettings)) {
    extra <- gsub("^;|;$", "", extraSettings)
    jdbc_url <- paste0(jdbc_url, extra, ";")
  }

  if (!is.null(user) && !is.null(password)) {
    jdbc_url <- paste0(
      jdbc_url,
      "AuthMech=3;",
      "UID=", user, ";",
      "PWD=", password, ";"
    )
  }

  jdbc_url <- paste0(jdbc_url, "UseNativeQuery=0;")

  enable_arrow <- toupper(Sys.getenv("ENABLE_ARROW", "FALSE"))
  if (enable_arrow %in% c("TRUE", "1", "YES")) {
    jdbc_url <- paste0(jdbc_url, "EnableArrow=1;")
    message("  Arrow optimisation enabled (requires proper JVM configuration)")
  } else {
    jdbc_url <- paste0(jdbc_url, "EnableArrow=0;")
  }

  if (interactive()) {
    batch_size  <- Sys.getenv("DATABASE_CONNECTOR_BATCH_SIZE", "10000")
    bulk_upload <- toupper(Sys.getenv("DATABASE_CONNECTOR_BULK_UPLOAD", "FALSE"))
    message(sprintf("  Batch processing size: %s rows", batch_size))
    if (bulk_upload %in% c("TRUE", "1", "YES"))
      message("  Bulk upload: enabled (DatabaseConnector)")
  }

  DatabaseConnector::createConnectionDetails(
    dbms             = "spark",
    connectionString = jdbc_url,
    pathToDriver     = pathToDriver
  )
}

#' Apply Spark/Databricks session options after connecting
#' @noRd
.configure_spark_connection <- function(connection, results_schema) {
  options(dbplyr.compute.defaults = list(temporary = FALSE))
  options(dbplyr.temp_prefix       = "temp_")
  if (!is.null(results_schema))
    options(sqlRenderTempEmulationSchema = results_schema)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# .env file loader
# ---------------------------------------------------------------------------

#' Parse a .env file and call Sys.setenv() for each key=value pair
#' @noRd
.load_env_file <- function(env_file) {
  lines <- readLines(env_file, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#")) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) next
    key   <- trimws(parts[[1L]])
    value <- trimws(paste(parts[-1L], collapse = "="))
    value <- gsub("^['\"]|['\"]$", "", value)   # strip surrounding quotes
    do.call(Sys.setenv, stats::setNames(list(value), key))
  }
}