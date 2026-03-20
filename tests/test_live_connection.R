# test_live_connection.R
# Manual script to verify the package OMOP connector against a live database.
# Run interactively; not part of the automated testthat suite.
#
# The package uses DatabaseConnector::createConnectionDetails() — the OHDSI
# standard — so any supported DBMS (SQL Server, PostgreSQL, Redshift, etc.)
# works here without site-specific changes beyond the four config lines below.

library(DatabaseConnector)
library(SteroidDoseR)

# ---------------------------------------------------------------------------
# 0. Site configuration via environment variables
#    Set these before running — never hardcode credentials or paths in scripts.
#
#    Required:
#      JDBC_DRIVER_PATH   path to folder containing your JDBC driver JAR
#      OMOP_SERVER        database server address
#
#    Optional (fall back to JHU defaults when testing locally):
#      OMOP_CDM_SCHEMA     default: "Myositis_OMOP.dbo"
#      OMOP_RESULTS_SCHEMA default: "Myositis_OMOP.Results"
# ---------------------------------------------------------------------------
jdbc_path    <- Sys.getenv("JDBC_DRIVER_PATH")
server       <- Sys.getenv("OMOP_SERVER",
                            unset = "esmpmdbpr4.esm.johnshopkins.edu")
cdm_schema   <- Sys.getenv("OMOP_CDM_SCHEMA",   unset = "Myositis_OMOP.dbo")
write_schema <- Sys.getenv("OMOP_RESULTS_SCHEMA", unset = "Myositis_OMOP.Results")

if (nchar(jdbc_path) == 0L) {
  stop(
    "JDBC_DRIVER_PATH is not set. Set it before running this script:\n",
    "  Sys.setenv(JDBC_DRIVER_PATH = '/path/to/jdbc')\n",
    "Download drivers with: ",
    "DatabaseConnector::downloadJdbcDrivers('sql server', pathToDriver = ...)"
  )
}

# ---------------------------------------------------------------------------
# 1. Create connectionDetails (OHDSI standard)
# ---------------------------------------------------------------------------
connection_details <- DatabaseConnector::createConnectionDetails(
  dbms         = "sql server",
  server       = server,
  pathToDriver = jdbc_path
)

# ---------------------------------------------------------------------------
# 2. Wrap in the package S3 connector
# ---------------------------------------------------------------------------
omop_con <- create_omop_connector(
  connectionDetails = connection_details,
  cdm_schema        = cdm_schema,
  results_schema    = write_schema
)
print(omop_con)   # <omop_connector>, Connected: no (lazy)

# ---------------------------------------------------------------------------
# 3. Detect available columns (probes the live DB)
# ---------------------------------------------------------------------------
message("\n--- Detecting capabilities ---")
omop_con <- detect_capabilities(omop_con)
print(omop_con$capabilities)

# ---------------------------------------------------------------------------
# 4. Fetch a sample of drug_exposure rows
# ---------------------------------------------------------------------------
message("\n--- Fetching sample drug_exposure rows ---")
sample_df <- with_connector(omop_con, function(active) {
  fetch_drug_exposure(
    active,
    start_date = "2015-01-01",
    end_date   = "2023-12-31"
  )
})
message(sprintf(
  "Fetched %d rows, %d unique persons",
  nrow(sample_df), length(unique(sample_df$person_id))
))
print(head(sample_df))

# ---------------------------------------------------------------------------
# 5. Baseline imputation
# ---------------------------------------------------------------------------
message("\n--- Baseline imputation ---")
baseline_out <- calc_daily_dose_baseline(sample_df)
print(table(baseline_out$imputation_method))
message(sprintf(
  "Coverage: %.1f%% non-missing",
  100 * mean(!is.na(baseline_out$daily_dose_mg_imputed))
))

# ---------------------------------------------------------------------------
# 6. NLP SIG parser
# ---------------------------------------------------------------------------
message("\n--- NLP SIG parser ---")
nlp_out <- calc_daily_dose_nlp(sample_df)
print(table(nlp_out$parsed_status, useNA = "ifany"))

# ---------------------------------------------------------------------------
# 7. Full pipeline to episodes
# ---------------------------------------------------------------------------
message("\n--- Episode building ---")
episodes <- run_pipeline(
  sample_df,
  method       = "baseline",
  return_level = "episode",
  gap_days     = 30L
)
message(sprintf(
  "Built %d episodes from %d persons",
  nrow(episodes), length(unique(episodes$person_id))
))
print(head(episodes))

# ---------------------------------------------------------------------------
# 8. Direct DB probe via DatabaseConnector
# ---------------------------------------------------------------------------
message("\n--- Direct DB probe ---")
with_connector(omop_con, function(active) {
  conn <- active$conn

  db_name <- DatabaseConnector::querySql(
    conn, "SELECT DB_NAME() AS current_db;"
  )
  message("Current DB: ", db_name$CURRENT_DB)

  top_row <- DatabaseConnector::querySql(
    conn,
    sprintf("SELECT TOP 1 * FROM %s.drug_exposure;", cdm_schema)
  )
  message("drug_exposure columns: ", paste(names(top_row), collapse = ", "))

  has_sig <- tryCatch({
    DatabaseConnector::querySql(
      conn,
      sprintf("SELECT TOP 0 sig FROM %s.drug_exposure;", cdm_schema)
    )
    TRUE
  }, error = function(e) FALSE)
  message("sig column present: ", has_sig)
})

message("\n=== Live connection test complete ===")