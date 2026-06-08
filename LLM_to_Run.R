# LLM_to_Run.R
# SteroidDoseR — LLM Dosage Extraction from Clinical Progress Notes
#
# Extracts medication dosage information from free-text progress notes using
# a locally-running ollama model. No cloud API key required.
#
# Workflow
# --------
# STEP 1 — Connection  : connect to your OMOP CDM database (Option B or C)
# STEP 2 — SQL [USER]  : write your own SQL to pull progress notes
# STEP 3 — LLM extract : send each note to ollama; parse structured JSON output
# STEP 4 — Save        : write results to a timestamped CSV
#
# Prerequisites
# -------------
#   ollama running locally:   https://ollama.com
#   Pull a model first:       ollama pull llama3.2   (or any model you prefer)
#   R packages:               httr2, jsonlite, dplyr, readr, cli
#
# Usage
# -----
#   source("LLM_to_Run.R")        # interactive (RStudio)
#   Rscript LLM_to_Run.R          # batch

if (!interactive()) quit(status = 0L, save = "no")

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------

OLLAMA_URL   <- "http://localhost:11434"   # ollama REST endpoint
OLLAMA_MODEL <- "llama3.2"                 # change to any model you have pulled
                                            # e.g. "mistral", "phi3", "gemma2"

BATCH_SIZE   <- 50L                        # notes per progress message
MAX_TOKENS   <- 512L                       # max tokens in the LLM response
TIMEOUT_SEC  <- 120L                       # per-request timeout in seconds

OUTPUT_DIR   <- file.path(getwd(), "output", "llm_notes")

# Optional: restrict to a cohort of person_ids (integer vector or NULL = all)
COHORT_PERSON_IDS <- NULL

# ---------------------------------------------------------------------------
# Column mapping — set these to match the column names your SQL returns.
# The script renames them internally so the rest of the pipeline is unchanged.
# ---------------------------------------------------------------------------
COL_PERSON_ID <- "person_id"   # unique patient identifier
COL_NOTE_ID   <- "note_id"     # unique note identifier
COL_NOTE_TEXT <- "note_text"   # free-text note content
# Any additional columns your SQL returns are passed through to the output as-is.

# ---------------------------------------------------------------------------
# 1. Package setup
# ---------------------------------------------------------------------------

needed <- c("httr2", "jsonlite", "dplyr", "readr", "cli")
to_install <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(needed, library, character.only = TRUE))

# ---------------------------------------------------------------------------
# 2. Database connection
# ---------------------------------------------------------------------------
# Uncomment ONE option below.  Both options produce a `conn` object and a
# `query_db()` helper used in Step 3. Leave commented if you load notes from
# a local CSV instead (see STEP 3 alternative below).

# ── Option A: DBI / odbc  (Databricks, SQL Server, PostgreSQL, etc.) ──────
# library(DBI); library(odbc)
# DB_DIALECT   <- "spark"                    # SqlRender targetDialect
# cdm_schema   <- "catalog.schema"           # e.g. "hive_metastore.my_omop_cdm"
# conn <- DBI::dbConnect(
#   odbc::odbc(),
#   Driver          = "Simba Spark ODBC Driver",
#   Host            = "",                    # Databricks workspace hostname
#   Port            = 443,
#   HTTPPath        = "",                    # SQL Warehouse HTTP path
#   AuthMech        = 3,
#   UID             = "token",
#   PWD             = "",                    # Personal Access Token
#   SSL             = 1,
#   ThriftTransport = 2
# )

# ── Option B: DatabaseConnector (OHDSI standard) ──────────────────────────
# library(DatabaseConnector); library(SqlRender)
# DB_DIALECT   <- "sql server"              # "postgresql", "redshift", ...
# cdm_schema   <- "database.dbo"
# connectionDetails <- DatabaseConnector::createConnectionDetails(
#   dbms             = DB_DIALECT,
#   connectionString = "",
#   pathToDriver     = ""
# )
# conn <- DatabaseConnector::connect(connectionDetails)

# Query helper — normalises DBI and DatabaseConnector into one function.
# Supports SqlRender template parameters via `...` (same signature as CodeToRun.R).
# query_db(sql, param = value, ...) → data.frame
query_db <- function(sql, ...) {
  if (inherits(conn, "DatabaseConnectorConnection")) {
    DatabaseConnector::renderTranslateQuerySql(
      conn, sql, ..., snakeCaseToCamelCase = FALSE
    )
  } else {
    DBI::dbGetQuery(
      conn,
      SqlRender::translate(SqlRender::render(sql, ...), targetDialect = DB_DIALECT)
    )
  }
}

# ---------------------------------------------------------------------------
# STEP 2 — [USER] Write your SQL here to extract progress notes
# ---------------------------------------------------------------------------
# Your query can return any column names — set COL_* in Section 0 to match.
# Any extra columns (note_date, visit_occurrence_id, etc.) are passed through
# to the output unchanged.
#
# SqlRender template parameters are supported via query_db():
#   notes_df <- query_db(notes_sql, cdm_schema = cdm_schema)
#   Then use @cdm_schema.note in your SQL.

# ── [USER SECTION START] ──────────────────────────────────────────────────
notes_sql <- "
  -- WRITE YOUR SQL HERE
  -- Set COL_PERSON_ID / COL_NOTE_ID / COL_NOTE_TEXT in Section 0 to match
  -- whatever column names you select here.
  SELECT
    person_id,
    note_id,
    note_text
  FROM cdm.note
  WHERE note_type_concept_id IN (44814637, 44814638)
  LIMIT 200
"
# ── [USER SECTION END] ────────────────────────────────────────────────────

# Load notes: use query_db() for a live DB or read a local CSV as the alternative.
#   Alternative — load from CSV:
#     notes_df <- readr::read_csv("/path/to/your/notes.csv")

notes_df        <- query_db(notes_sql)
names(notes_df) <- tolower(names(notes_df))

# Validate that the user-configured column names exist in the result
user_cols    <- c(COL_PERSON_ID, COL_NOTE_ID, COL_NOTE_TEXT)
missing_cols <- setdiff(tolower(user_cols), names(notes_df))
if (length(missing_cols) > 0) {
  stop(
    "notes_df is missing columns: ", paste(missing_cols, collapse = ", "), "\n",
    "Update COL_PERSON_ID / COL_NOTE_ID / COL_NOTE_TEXT in Section 0 to match ",
    "the column names returned by your SQL.\n",
    "Columns present: ", paste(names(notes_df), collapse = ", ")
  )
}

# Rename to canonical internal names so the rest of the pipeline is uniform
notes_df <- dplyr::rename(
  notes_df,
  person_id = !!tolower(COL_PERSON_ID),
  note_id   = !!tolower(COL_NOTE_ID),
  note_text = !!tolower(COL_NOTE_TEXT)
)

# Optional cohort filter
if (!is.null(COHORT_PERSON_IDS)) {
  notes_df <- dplyr::filter(notes_df, person_id %in% COHORT_PERSON_IDS)
}

cli::cli_alert_info("Notes loaded: {nrow(notes_df)} rows | {dplyr::n_distinct(notes_df$person_id)} unique patients")

# ---------------------------------------------------------------------------
# STEP 3 — LLM extraction helpers
# ---------------------------------------------------------------------------

# Extraction prompt — asks the model to return a JSON array only.
# Adjust the drug_name field description to target your specific medications.
.make_prompt <- function(note_text) {
  sprintf(
    paste0(
      "You are a clinical NLP assistant. Extract every corticosteroid (or ",
      "immunosuppressant) dosage mentioned in the progress note below.\n\n",
      "Return ONLY a JSON array with no markdown fences, no explanation. ",
      "Each element must have exactly these keys:\n",
      "  drug_name   : medication name (string or null)\n",
      "  dose_value  : numeric dose amount (number or null)\n",
      "  dose_unit   : unit string such as \"mg\", \"g\", \"mcg\" (string or null)\n",
      "  frequency   : dosing frequency such as \"daily\", \"BID\", \"weekly\" (string or null)\n",
      "  route       : route of administration such as \"oral\", \"IV\", \"IM\" (string or null)\n",
      "  dose_text   : exact verbatim phrase from the note (string)\n\n",
      "If no corticosteroid or immunosuppressant dosage is present, return [].\n\n",
      "Progress note:\n%s"
    ),
    note_text
  )
}

# Call the local ollama /api/chat endpoint for one note.
# Returns parsed list from JSON or NULL on failure.
.call_ollama <- function(note_text) {
  body <- list(
    model    = OLLAMA_MODEL,
    messages = list(list(role = "user", content = .make_prompt(note_text))),
    stream   = FALSE,
    options  = list(num_predict = MAX_TOKENS, temperature = 0)
  )

  resp <- tryCatch(
    httr2::request(paste0(OLLAMA_URL, "/api/chat")) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(TIMEOUT_SEC) |>
      httr2::req_error(is_error = \(r) FALSE) |>   # don't throw; handle below
      httr2::req_perform(),
    error = function(e) {
      warning("ollama request failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(resp) || httr2::resp_status(resp) != 200L) {
    return(NULL)
  }

  raw_json  <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  llm_text  <- raw_json[["message"]][["content"]]
  llm_text
}

# Parse the LLM text output into a tidy data frame row.
# Returns a list-of-rows (may be multiple drugs per note).
.parse_llm_output <- function(llm_text, person_id, note_id) {

  fallback <- data.frame(
    person_id   = person_id,
    note_id     = note_id,
    drug_name   = NA_character_,
    dose_value  = NA_real_,
    dose_unit   = NA_character_,
    frequency   = NA_character_,
    route       = NA_character_,
    dose_text   = NA_character_,
    parse_status = "no_output",
    llm_response = NA_character_,
    stringsAsFactors = FALSE
  )

  if (is.null(llm_text) || !nzchar(trimws(llm_text))) {
    return(fallback)
  }

  # Strip markdown code fences if the model added them despite instructions
  cleaned <- gsub("```(?:json)?|```", "", llm_text, perl = TRUE)
  cleaned <- trimws(cleaned)

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed) || (is.data.frame(parsed) && nrow(parsed) == 0) ||
      (is.list(parsed) && length(parsed) == 0)) {
    fallback$parse_status <- "empty"
    fallback$llm_response <- llm_text
    return(fallback)
  }

  # Coerce to data frame regardless of whether model returned a list or array
  if (!is.data.frame(parsed)) {
    parsed <- tryCatch(as.data.frame(parsed), error = function(e) NULL)
    if (is.null(parsed)) {
      fallback$parse_status <- "parse_error"
      fallback$llm_response <- llm_text
      return(fallback)
    }
  }

  expected <- c("drug_name", "dose_value", "dose_unit", "frequency", "route", "dose_text")
  for (col in expected) {
    if (!col %in% names(parsed)) parsed[[col]] <- NA
  }

  parsed$person_id    <- person_id
  parsed$note_id      <- note_id
  parsed$parse_status <- "ok"
  parsed$llm_response <- llm_text

  parsed$dose_value   <- suppressWarnings(as.numeric(parsed$dose_value))

  parsed[, c("person_id", "note_id", expected, "parse_status", "llm_response")]
}

# ---------------------------------------------------------------------------
# STEP 3b — Run extraction over all notes
# ---------------------------------------------------------------------------

cli::cli_h1("LLM dosage extraction  ({OLLAMA_MODEL})")
cli::cli_alert_info("Sending {nrow(notes_df)} notes to ollama at {OLLAMA_URL}")

results_list <- vector("list", nrow(notes_df))

for (i in seq_len(nrow(notes_df))) {
  row <- notes_df[i, ]

  llm_text         <- .call_ollama(row$note_text)
  results_list[[i]] <- .parse_llm_output(llm_text, row$person_id, row$note_id)

  if (i %% BATCH_SIZE == 0L || i == nrow(notes_df)) {
    n_ok <- sum(vapply(results_list[seq_len(i)], function(r) {
      !is.null(r) && isTRUE(r$parse_status[1] == "ok")
    }, logical(1)))
    cli::cli_progress_message(
      "[{i}/{nrow(notes_df)}]  successful extractions so far: {n_ok}"
    )
  }
}

results_df <- dplyr::bind_rows(results_list)

# ---------------------------------------------------------------------------
# STEP 3c — Summary
# ---------------------------------------------------------------------------

cli::cli_h2("Extraction summary")

status_tbl <- table(results_df$parse_status, useNA = "ifany")
print(status_tbl)

ok_df <- dplyr::filter(results_df, parse_status == "ok", !is.na(dose_value))
cli::cli_alert_info(
  "Records with a numeric dose: {nrow(ok_df)} / {nrow(results_df)}"
)

if (nrow(ok_df) > 0) {
  cat("\nTop drugs found (n >= 2):\n")
  ok_df |>
    dplyr::count(drug_name, sort = TRUE) |>
    dplyr::filter(n >= 2L) |>
    print()

  cat("\nDose value distribution:\n")
  print(summary(ok_df$dose_value))
}

# ---------------------------------------------------------------------------
# STEP 4 — Save results
# ---------------------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

readr::write_csv(results_df, file.path(RUN_DIR, "llm_dose_extractions.csv"))
readr::write_csv(ok_df,      file.path(RUN_DIR, "llm_dose_extractions_ok.csv"))

writeLines(c(
  "LLM_to_Run.R — Run Parameters",
  strrep("=", 40),
  sprintf("Run time:            %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("OLLAMA_MODEL:        %s", OLLAMA_MODEL),
  sprintf("OLLAMA_URL:          %s", OLLAMA_URL),
  sprintf("MAX_TOKENS:          %d", MAX_TOKENS),
  sprintf("TIMEOUT_SEC:         %d", TIMEOUT_SEC),
  sprintf("Total notes:         %d", nrow(notes_df)),
  sprintf("Total result rows:   %d", nrow(results_df)),
  sprintf("Rows with dose (ok): %d", nrow(ok_df)),
  sprintf("COHORT_PERSON_IDS:   %s",
          if (is.null(COHORT_PERSON_IDS)) "NULL (all patients)"
          else sprintf("%d person IDs", length(COHORT_PERSON_IDS)))
), con = file.path(RUN_DIR, "params.txt"))

cli::cli_alert_success("Results saved to: {RUN_DIR}")

# ---------------------------------------------------------------------------
# Disconnect (live DB only)
# ---------------------------------------------------------------------------
if (exists("conn")) {
  tryCatch(
    if (inherits(conn, "DatabaseConnectorConnection")) {
      DatabaseConnector::disconnect(conn)
    } else {
      DBI::dbDisconnect(conn)
    },
    error = function(e) NULL
  )
}

cli::cli_alert_success("Done.")
