# connection.R
# High-level OMOP CDM connection helpers.
#
# create_omop_connection() is the primary entry point: it reads all settings
# from environment variables (or accepts explicit arguments), builds a
# platform-appropriate JDBC connection string, and returns a live
# DatabaseConnector connection with schema metadata stored as attributes.
#
# Platform-specific builders:
#   create_sqlserver_connection()  -- SQL Server + Windows AD / NTLM
#   create_databricks_connection() -- Databricks / Spark with Arrow option
#
# Convenience wrapper:
#   create_connection_from_env()   -- read everything from a .env file

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
#' @return An `omop_connector` object (`c("omop_connector", "steroid_connector")`).
#'   The database connection is lazy -- opened per-query and closed immediately
#'   after. No manual `disconnect()` call is needed. Pass directly to
#'   `calc_daily_dose_baseline()`, `run_pipeline()`, etc.
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
  }

  # ------------------------------------------------------------------
  # Sanitise schema values -- strip any leading = signs that can arise
  # from double-equals in .env files (KEY==value) or shell export
  # contamination (export KEY=value parsed as value "=value").
  # ------------------------------------------------------------------
  cdm_schema        <- sub("^=+", "", trimws(cdm_schema        %||% ""))
  vocabulary_schema <- if (!is.null(vocabulary_schema))
    sub("^=+", "", trimws(vocabulary_schema)) else NULL
  results_schema    <- if (!is.null(results_schema))
    sub("^=+", "", trimws(results_schema)) else NULL

  # ------------------------------------------------------------------
  # Auto-prefix cdm_schema with the database name when no dot is present.
  # This lets users set SQL_CDM_SCHEMA=dbo and SQL_DATABASE=Myositis_OMOP
  # and get the correct three-part name "Myositis_OMOP.dbo" automatically.
  # If cdm_schema already contains a dot (e.g. "Myositis_OMOP.dbo") or
  # database is unknown, no change is made.
  # ------------------------------------------------------------------
  if (!grepl("\\.", cdm_schema) && !is.null(database) && nzchar(database)) {
    cdm_schema <- paste0(database, ".", cdm_schema)
  }
  if (!is.null(vocabulary_schema) &&
      !grepl("\\.", vocabulary_schema) &&
      !is.null(database) && nzchar(database)) {
    vocabulary_schema <- paste0(database, ".", vocabulary_schema)
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
  # Return an omop_connector (lazy connection managed by with_connector)
  #
  # We intentionally do NOT open a live connection here and store metadata
  # as attributes on the DatabaseConnectorConnection object.  S4 connection
  # objects from DatabaseConnector 7.x do not reliably round-trip arbitrary
  # attributes set via attr<-, which corrupts cdm_schema when SqlRender
  # reads it back.  The omop_connector S3 list stores all metadata in plain
  # named slots, and with_connector() opens/closes the JDBC connection
  # per-query, preventing connection leaks.
  # ------------------------------------------------------------------
  if (dbms == "spark") {
    .configure_spark_connection(NULL, results_schema)
  }

  # Open the connection now so it is available immediately and reused by
  # every subsequent pipeline call without re-connecting each time.
  conn <- DatabaseConnector::connect(connectionDetails)

  message(sprintf(
    "\u2713 omop_connector ready  |  dbms: %s  |  server: %s  |  cdm_schema: %s",
    dbms, server %||% "<unset>", cdm_schema
  ))

  con <- create_omop_connector(
    connectionDetails = connectionDetails,
    cdm_schema        = cdm_schema,
    vocab_schema      = vocabulary_schema,
    results_schema    = results_schema
  )
  con$conn  <- conn
  con$dbms  <- DatabaseConnector::dbms(conn)
  con
}

#' Create connection from environment variables or a .env file
#'
#' Loads settings from a `.env` file (if it exists) then calls
#' [create_omop_connection()] with `use_env = TRUE`. This is the simplest
#' entry point when all configuration is stored in environment variables.
#'
#' @param env_file Path to the `.env` file. Default `".env"` (project root).
#'
#' @return An `omop_connector` object (see [create_omop_connection()]).
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

#' Apply Spark/Databricks session options (connection argument is unused but kept
#' for API consistency; options are session-level and don't require a live conn).
#' @noRd
.configure_spark_connection <- function(connection, results_schema) {
  options(dbplyr.compute.defaults = list(temporary = FALSE))
  options(dbplyr.temp_prefix       = "temp_")
  if (!is.null(results_schema))
    options(sqlRenderTempEmulationSchema = results_schema)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# SAFER / REACH Databricks connection  (rJava + RJDBC + DBI)
# ---------------------------------------------------------------------------

#' Create a Databricks connection for the SAFER / REACH HPC environment
#'
#' Uses `rJava`, `RJDBC`, and `DBI` instead of `DatabaseConnector` to connect
#' to a Databricks SQL Warehouse. This is the native approach for the Johns
#' Hopkins SAFER / REACH HPC cluster where the Databricks JDBC driver is
#' accessed directly without the OHDSI DatabaseConnector layer.
#'
#' The returned connector satisfies the same interface as the one produced by
#' [create_omop_connection()]: pass it directly to [run_pipeline()],
#' [fetch_drug_exposure()], [detect_capabilities()], etc.
#' Call [disconnect_connector()] when finished.
#'
#' ## Required environment variables
#'
#' | Variable | Description |
#' |---|---|
#' | `DATABRICKS_SERVER_HOSTNAME` | Workspace hostname (e.g. `adb-1234.7.azuredatabricks.net`) |
#' | `DATABRICKS_HTTP_PATH` | SQL Warehouse HTTP path (e.g. `/sql/1.0/warehouses/abc123`) |
#' | `DATABRICKS_TOKEN` | Personal Access Token |
#' | `DATABRICKS_JDBC_JAR` | Path to Databricks JDBC jar (default: `~/jdbc/databricks-jdbc.jar`) |
#' | `DATABRICKS_CDM_SCHEMA` | CDM schema in `catalog.schema` format (e.g. `deid.omop`) |
#' | `DATABRICKS_VOCAB_SCHEMA` | Vocabulary schema (defaults to `cdm_schema`) |
#' | `DATABRICKS_RESULTS_SCHEMA` | Results schema (optional) |
#'
#' ## Prerequisites
#'
#' Install the required R packages and the Databricks JDBC driver.
#' On SAFER / REACH HPC, follow the notebook
#' `notebooks/08_databricks_R_connect.qmd` in the REACH-Templates repository:
#'
#' ```r
#' install.packages(c("rJava", "RJDBC", "DBI"))
#' # Download driver from Maven Central:
#' # https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/
#' # Place the jar at ~/jdbc/databricks-jdbc-2.6.36.jar
#' ```
#'
#' @param server_hostname Databricks workspace hostname.
#'   Strip the leading `https://` if present.
#' @param http_path SQL Warehouse or cluster HTTP path.
#' @param token Personal Access Token (`dapi...`).
#' @param jdbc_jar Path to the Databricks JDBC driver `.jar`. Auto-detected
#'   from common locations when `NULL`: `~/jdbc/databricks-jdbc*.jar` and the
#'   package's bundled `jdbc_drivers/databricks/` folder.
#' @param cdm_schema CDM schema in `catalog.schema` format, e.g. `"deid.omop"`.
#' @param vocab_schema Vocabulary schema (defaults to `cdm_schema`).
#' @param results_schema Results / scratch schema (optional).
#' @param use_env Logical. Load unset parameters from environment variables.
#'   Default `TRUE`.
#'
#' @return An `omop_connector` object (`c("omop_connector", "steroid_connector")`)
#'   with an open RJDBC/DBI connection and `$use_rjdbc = TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simplest: load everything from R.env (SAFER/REACH convention)
#' con <- create_connection_from_safer_env("R.env")
#' drug_df <- with_connector(con, function(active) {
#'   fetch_drug_exposure(active, start_date = "2020-01-01")
#' })
#' episodes <- run_pipeline(drug_df, method = "baseline")
#' disconnect_connector(con)
#'
#' # Explicit arguments
#' con <- create_safer_connection(
#'   server_hostname = "adb-1234.7.azuredatabricks.net",
#'   http_path       = "/sql/1.0/warehouses/abc123",
#'   token           = Sys.getenv("DATABRICKS_TOKEN"),
#'   cdm_schema      = "deid.omop"
#' )
#' }
create_safer_connection <- function(
    server_hostname = NULL,
    http_path       = NULL,
    token           = NULL,
    jdbc_jar        = NULL,
    cdm_schema      = NULL,
    vocab_schema    = NULL,
    results_schema  = NULL,
    use_env         = TRUE
) {
  .check_rjdbc_packages()

  # ------------------------------------------------------------------
  # Load unset parameters from environment variables
  # ------------------------------------------------------------------
  if (use_env) {
    if (is.null(server_hostname) || !nzchar(server_hostname %||% "")) {
      v <- Sys.getenv("DATABRICKS_SERVER_HOSTNAME")
      if (nzchar(v)) server_hostname <- v
    }
    if (is.null(http_path) || !nzchar(http_path %||% "")) {
      v <- Sys.getenv("DATABRICKS_HTTP_PATH")
      if (nzchar(v)) http_path <- v
    }
    if (is.null(token) || !nzchar(token %||% "")) {
      v <- Sys.getenv("DATABRICKS_TOKEN")
      if (nzchar(v)) token <- v
    }
    if (is.null(jdbc_jar)) {
      v <- Sys.getenv("DATABRICKS_JDBC_JAR")
      if (nzchar(v)) {
        jdbc_jar <- path.expand(v)
      } else {
        # Auto-detect from common locations
        candidates <- c(
          path.expand("~/jdbc/databricks-jdbc-2.6.36.jar"),
          path.expand("~/jdbc/databricks-jdbc.jar"),
          file.path(getwd(), "jdbc_drivers", "databricks", "DatabricksJDBC.jar")
        )
        found <- candidates[file.exists(candidates)]
        if (length(found) > 0L) jdbc_jar <- found[[1L]]
      }
    }
    if (is.null(cdm_schema) || !nzchar(cdm_schema %||% "")) {
      v <- Sys.getenv("DATABRICKS_CDM_SCHEMA")
      if (!nzchar(v)) v <- Sys.getenv("CDM_SCHEMA")
      if (nzchar(v)) cdm_schema <- v
    }
    if (is.null(vocab_schema) || !nzchar(vocab_schema %||% "")) {
      v <- Sys.getenv("DATABRICKS_VOCAB_SCHEMA")
      if (nzchar(v)) vocab_schema <- v
    }
    if (is.null(results_schema) || !nzchar(results_schema %||% "")) {
      v <- Sys.getenv("DATABRICKS_RESULTS_SCHEMA")
      if (nzchar(v)) results_schema <- v
    }
  }

  # ------------------------------------------------------------------
  # Validate
  # ------------------------------------------------------------------
  if (!nzchar(server_hostname %||% ""))
    rlang::abort(paste0(
      "Databricks server hostname is required.\n",
      "Set DATABRICKS_SERVER_HOSTNAME in your env file ",
      "or supply the `server_hostname` argument."
    ))
  if (!nzchar(http_path %||% ""))
    rlang::abort(paste0(
      "Databricks HTTP path is required.\n",
      "Set DATABRICKS_HTTP_PATH in your env file ",
      "or supply the `http_path` argument."
    ))
  if (!nzchar(token %||% ""))
    rlang::abort(paste0(
      "Databricks access token is required.\n",
      "Set DATABRICKS_TOKEN in your env file ",
      "or supply the `token` argument."
    ))
  if (is.null(jdbc_jar) || !file.exists(jdbc_jar))
    rlang::abort(paste0(
      "Databricks JDBC driver jar not found.\n",
      "Download from Maven Central and place at ~/jdbc/databricks-jdbc-2.6.36.jar,\n",
      "or set DATABRICKS_JDBC_JAR in your env file.\n",
      "URL: https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/"
    ))
  if (!nzchar(cdm_schema %||% ""))
    rlang::abort(paste0(
      "CDM schema is required (e.g. 'deid.omop').\n",
      "Set DATABRICKS_CDM_SCHEMA in your env file ",
      "or supply the `cdm_schema` argument."
    ))

  # ------------------------------------------------------------------
  # Defaults
  # ------------------------------------------------------------------
  if (is.null(vocab_schema) || !nzchar(vocab_schema)) vocab_schema <- cdm_schema

  # Strip leading https:// and trailing slash from hostname
  server_hostname <- sub("^https?://", "", server_hostname)
  server_hostname <- sub("/$", "", server_hostname)

  # ------------------------------------------------------------------
  # Build JDBC URL  (SAFER / REACH format)
  # ------------------------------------------------------------------
  jdbc_url <- paste0(
    "jdbc:databricks://", server_hostname, ":443;",
    "transportMode=http;ssl=1;",
    "httpPath=", http_path, ";",
    "AuthMech=3;UID=token;PWD=", token
  )

  # ------------------------------------------------------------------
  # Initialise RJDBC driver and open connection
  # ------------------------------------------------------------------
  drv <- tryCatch(
    RJDBC::JDBC(
      driverClass = "com.databricks.client.jdbc.Driver",
      classPath   = jdbc_jar
    ),
    error = function(e) rlang::abort(paste0(
      "Failed to initialise Databricks JDBC driver.\n",
      "JAR path: ", jdbc_jar, "\n",
      "Error: ", conditionMessage(e)
    ))
  )

  conn <- tryCatch(
    DBI::dbConnect(drv, jdbc_url),
    error = function(e) rlang::abort(paste0(
      "Failed to connect to Databricks at ", server_hostname, ".\n",
      "Error: ", conditionMessage(e)
    ))
  )

  message(sprintf(
    "\u2713 safer_connector ready  |  server: %s  |  cdm_schema: %s",
    server_hostname, cdm_schema
  ))

  # ------------------------------------------------------------------
  # Build and return the omop_connector (use_rjdbc = TRUE)
  # ------------------------------------------------------------------
  structure(
    list(
      type           = "omop",
      connectionDetails = NULL,   # not used for RJDBC path
      cdm_schema     = cdm_schema,
      vocab_schema   = vocab_schema,
      results_schema = results_schema,
      temp_schema    = NULL,
      cdm_version    = "5.4",
      conn           = conn,
      dbms           = "spark",
      capabilities   = NULL,
      use_rjdbc      = TRUE
    ),
    class = c("omop_connector", "steroid_connector")
  )
}

#' Create a SAFER/REACH Databricks connection from an env file
#'
#' Loads credentials from a `.env` or `R.env` file and calls
#' [create_safer_connection()]. This is the recommended entry point for
#' scripts running on the Johns Hopkins SAFER / REACH HPC cluster.
#'
#' @param env_file Path to the credentials file. Default `"R.env"` (the SAFER
#'   convention); falls back to `".env"` if `"R.env"` does not exist.
#'
#' @return An `omop_connector` object with `$use_rjdbc = TRUE`.
#' @export
create_connection_from_safer_env <- function(env_file = "R.env") {
  candidates <- unique(c(env_file, "R.env", ".env"))
  loaded <- FALSE
  for (f in candidates) {
    if (file.exists(f)) {
      .load_env_file(f)
      message("\u2713 Loaded environment variables from ", f)
      loaded <- TRUE
      break
    }
  }
  if (!loaded) {
    message("No env file found (tried: ", paste(candidates, collapse = ", "),
            "). Proceeding with system environment variables.")
  }
  create_safer_connection(use_env = TRUE)
}

# ---------------------------------------------------------------------------
# .env file loader
# ---------------------------------------------------------------------------

#' Parse a .env file and call Sys.setenv() for each key=value pair.
#'
#' Handles the common case where values contain = (e.g. connection strings).
#' The key is everything before the FIRST =; the value is everything after.
#' Surrounding quotes and leading/trailing whitespace are stripped from both.
#' Lines starting with # or that contain no = are silently skipped.
#' @noRd
.load_env_file <- function(env_file) {
  lines <- readLines(env_file, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#")) next
    # Split on the FIRST = only so that values containing = are preserved.
    eq_pos <- regexpr("=", line, fixed = TRUE)
    if (eq_pos < 1L) next
    key   <- trimws(substr(line, 1L, eq_pos - 1L))
    value <- trimws(substr(line, eq_pos + 1L, nchar(line)))
    value <- gsub("^['\"]|['\"]$", "", value)   # strip surrounding quotes
    if (!nzchar(key)) next
    do.call(Sys.setenv, stats::setNames(list(value), key))
  }
}