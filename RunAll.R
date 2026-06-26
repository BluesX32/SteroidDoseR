# RunAll.R
# SteroidDoseR — Single Entry Point
#
# Sets all shared parameters, then sources the three method scripts and the
# comparison script in order.  Run this file to reproduce the full analysis.
#
# To run only part of the analysis, source the individual scripts directly
# (each can run standalone — see the comments at the top of each file).
#
#   CodeToRun.R    — Baseline + NLP methods
#   LLMtoRun.R    — LLM extraction from progress notes
#   CompareToRun.R — Three-way comparison against gold standard

devtools::install_local(getwd())
if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)
library(ggplot2)

# ===========================================================================
# SHARED PARAMETERS  —  edit here; all three scripts inherit these values
# ===========================================================================

USE_SYNTHETIC     <- FALSE          # TRUE = bundled synthetic data, no DB needed
START_DATE        <- "2015-01-01"
END_DATE          <- "2025-12-31"
GAP_DAYS          <- 30L            # episode-bridging tolerance (days)
COHORT_PERSON_IDS <- NULL           # integer vector of person_ids, or NULL = all

GOLD_STD_PATH  <- "/your/path/to/gold-standard.csv"
GOLD_NEG_PATH  <- "/your/path/to/gold-negative.csv"

OUTPUT_DIR <- file.path(getwd(), "output")
RUN_DIR    <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

# ===========================================================================
# DATABASE CONNECTION  —  fill in ONE option (skip when USE_SYNTHETIC = TRUE)
# Both CodeToRun.R and LLMtoRun.R will reuse the `conn` created here.
# ===========================================================================
if (!USE_SYNTHETIC) {

  # ── Option B: DatabaseConnector (OHDSI standard) ──────────────────────────
  # library(DatabaseConnector); library(SqlRender)
  # DB_DIALECT   <- "sql server"
  # cdm_schema   <- "database.dbo"
  # vocab_schema <- "database.dbo"
  # connectionDetails <- DatabaseConnector::createConnectionDetails(
  #   dbms             = DB_DIALECT,
  #   connectionString = "",
  #   pathToDriver     = ""
  # )
  # conn <- DatabaseConnector::connect(connectionDetails)

  # ── Option C: DBI / odbc  (Databricks, SQL Server, PostgreSQL, etc.) ──────
  # library(DBI); library(odbc); library(SqlRender)
  # DB_DIALECT   <- "spark"
  # cdm_schema   <- "catalog.schema"
  # vocab_schema <- "catalog.schema"
  # conn <- DBI::dbConnect(
  #   odbc::odbc(),
  #   Driver          = "Simba Spark ODBC Driver",
  #   Host            = "",
  #   Port            = 443,
  #   HTTPPath        = "",
  #   AuthMech        = 3,
  #   UID             = "token",
  #   PWD             = "",
  #   SSL             = 1,
  #   ThriftTransport = 2
  # )

}

# ===========================================================================
# RUN  —  order matters: CodeToRun feeds LLMtoRun; both feed CompareToRun
# ===========================================================================

# Sentinel that suppresses per-script disconnects and param defaults
.RUNALL_ACTIVE <- TRUE

message("\n", strrep("=", 60))
message("STEP 1/3  Baseline + NLP  (CodeToRun.R)")
message(strrep("=", 60))
source("CodeToRun.R")

message("\n", strrep("=", 60))
message("STEP 2/3  LLM extraction  (LLMtoRun.R)")
message(strrep("=", 60))
source("LLMtoRun.R")

message("\n", strrep("=", 60))
message("STEP 3/3  Three-way comparison  (CompareToRun.R)")
message(strrep("=", 60))
source("CompareToRun.R")

# ===========================================================================
# DISCONNECT
# ===========================================================================
if (!USE_SYNTHETIC && exists("conn")) {
  tryCatch(
    if (inherits(conn, "DatabaseConnectorConnection")) {
      DatabaseConnector::disconnect(conn)
    } else {
      DBI::dbDisconnect(conn)
    },
    error = function(e) NULL
  )
}

message("\n=== Full analysis complete.  Results in: ", RUN_DIR, " ===")
