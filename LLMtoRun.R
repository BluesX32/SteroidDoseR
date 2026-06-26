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
#   source("LLMtoRun.R")        # interactive (RStudio)
#   Rscript LLMtoRun.R          # batch

if (!interactive()) quit(status = 0L, save = "no")

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------

OLLAMA_URL   <- "http://localhost:11434"   # ollama REST endpoint
OLLAMA_MODEL <- "llama3.2"                 # change to any model you have pulled
                                            # e.g. "mistral", "phi3", "gemma2"

MAX_TOKENS        <- 256L                  # max tokens in the LLM response (no reasoning → shorter)
TIMEOUT_SEC       <- 60L                   # per-request timeout in seconds
N_WORKERS         <- 4L                    # parallel ollama workers (set to 1 to disable)
CHECKPOINT_EVERY  <- 25L                   # flush intermediate CSV to disk every N notes

OUTPUT_DIR   <- file.path(getwd(), "output", "llm_notes")

# Optional: restrict to a cohort of person_ids (integer vector or NULL = all)
COHORT_PERSON_IDS <- NULL

# ---------------------------------------------------------------------------
# Column mapping — OMOP CDM NOTE table standard column names.
# Change only if your database uses non-standard column aliases.
# ---------------------------------------------------------------------------
COL_PERSON_ID   <- "person_id"             # unique patient identifier
COL_NOTE_ID     <- "note_id"               # unique note identifier
COL_NOTE_TEXT   <- "note_text"             # free-text note content (source for LLM)
COL_NOTE_DATE   <- "note_date"             # note date (DATE)
COL_NOTE_TITLE  <- "note_title"            # note title / chief complaint
COL_NOTE_TYPE   <- "note_type_concept_id"  # concept ID classifying note type
COL_NOTE_CLASS  <- "note_class_concept_id" # concept ID for note class (e.g. progress note)

# ---------------------------------------------------------------------------
# 1. Package setup
# ---------------------------------------------------------------------------

needed <- c("httr2", "jsonlite", "dplyr", "readr", "cli", "future", "future.apply")
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
  if (!exists("conn")) {
    stop("No database connection found. Run the connection block in Section 2 first.")
  }

  # Catch the "external pointer is not valid" error that occurs when conn is a
  # leftover object from a previous R session that has since been restarted.
  is_valid <- tryCatch(
    if (inherits(conn, "DatabaseConnectorConnection")) {
      DatabaseConnector::dbIsValid(conn)
    } else {
      DBI::dbIsValid(conn)
    },
    error = function(e) FALSE
  )
  if (!is_valid) {
    stop(
      "Database connection is no longer valid (stale external pointer).\n",
      "Re-run the connection block in Section 2 to open a fresh `conn`, then retry."
    )
  }

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
  -- OMOP CDM NOTE table — all standard columns are selected so note metadata
  -- (date, title, type, class) is carried through to the output CSV.
  --
  -- note_type_concept_id 32831 = 'Progress note' (OMOP standard)
  -- Change or remove the WHERE clause to include other note types.
  SELECT
    n.note_id,
    n.person_id,
    n.note_date,
    n.note_datetime,
    n.note_type_concept_id,
    n.note_class_concept_id,
    n.note_title,
    n.note_text
  FROM @cdm_schema.note n
  WHERE n.note_type_concept_id = 32831
    AND n.note_text IS NOT NULL
    AND TRIM(n.note_text) <> ''
  LIMIT 200
"
# ── [USER SECTION END] ────────────────────────────────────────────────────

# Load notes: use query_db() for a live DB or read a local CSV as the alternative.
#   Alternative — load from CSV:
#     notes_df <- readr::read_csv("/path/to/your/notes.csv")

notes_df        <- query_db(notes_sql, cdm_schema = cdm_schema)
names(notes_df) <- tolower(names(notes_df))

# Validate required OMOP NOTE columns
required_cols <- c(COL_PERSON_ID, COL_NOTE_ID, COL_NOTE_TEXT)
missing_cols  <- setdiff(tolower(required_cols), names(notes_df))
if (length(missing_cols) > 0) {
  stop(
    "notes_df is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
    "Columns present: ", paste(names(notes_df), collapse = ", ")
  )
}

# Rename to canonical internal names so the rest of the pipeline is uniform.
# Optional OMOP NOTE metadata columns are renamed only when present.
rename_map <- c(
  person_id             = tolower(COL_PERSON_ID),
  note_id               = tolower(COL_NOTE_ID),
  note_text             = tolower(COL_NOTE_TEXT),
  note_date             = tolower(COL_NOTE_DATE),
  note_title            = tolower(COL_NOTE_TITLE),
  note_type_concept_id  = tolower(COL_NOTE_TYPE),
  note_class_concept_id = tolower(COL_NOTE_CLASS)
)
rename_map <- rename_map[rename_map %in% names(notes_df)]
notes_df   <- dplyr::rename(notes_df, !!!rename_map)

# Optional cohort filter
if (!is.null(COHORT_PERSON_IDS)) {
  notes_df <- dplyr::filter(notes_df, person_id %in% COHORT_PERSON_IDS)
}

cli::cli_alert_info("Notes loaded: {nrow(notes_df)} rows | {dplyr::n_distinct(notes_df$person_id)} unique patients")

# ---------------------------------------------------------------------------
# STEP 3 — LLM extraction helpers
# ---------------------------------------------------------------------------

# Extraction prompt — direct, no-reasoning instruction for fastest response.
.make_prompt <- function(note_text) {
  paste0(
    "Task: extract every corticosteroid dosage from the clinical note.\n",
    "Output ONLY a JSON array. No prose. No reasoning. No markdown fences.\n",
    "Each object must have exactly these keys (use null when unknown):\n",
    "  drug_name, dose_value (number|null), dose_unit, frequency, route, dose_text\n",
    "If none found, output: []\n\n",
    "NOTE:\n", note_text
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

# Parse the LLM text output into a tidy data frame.
# `meta` is a named list of OMOP NOTE metadata (note_date, note_title, etc.)
# carried through from the source row; each field is appended to every output row.
.parse_llm_output <- function(llm_text, person_id, note_id, meta = list()) {

  meta_df <- as.data.frame(meta, stringsAsFactors = FALSE, check.names = FALSE)

  make_fallback <- function(status) {
    out <- data.frame(
      person_id    = person_id,
      note_id      = note_id,
      drug_name    = NA_character_,
      dose_value   = NA_real_,
      dose_unit    = NA_character_,
      frequency    = NA_character_,
      route        = NA_character_,
      dose_text    = NA_character_,
      parse_status = status,
      llm_response = if (is.null(llm_text)) NA_character_ else llm_text,
      stringsAsFactors = FALSE
    )
    if (nrow(meta_df) > 0) cbind(out, meta_df) else out
  }

  if (is.null(llm_text) || !nzchar(trimws(llm_text))) {
    return(make_fallback("no_output"))
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
    return(make_fallback("empty"))
  }

  # Coerce to data frame regardless of whether model returned a list or array
  if (!is.data.frame(parsed)) {
    parsed <- tryCatch(as.data.frame(parsed), error = function(e) NULL)
    if (is.null(parsed)) return(make_fallback("parse_error"))
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

  out <- parsed[, c("person_id", "note_id", expected, "parse_status", "llm_response")]
  if (nrow(meta_df) > 0) cbind(out, meta_df[rep(1L, nrow(out)), , drop = FALSE]) else out
}

# ---------------------------------------------------------------------------
# STEP 3b — Setup: output directory, log file, parallel plan
# ---------------------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
RUN_DIR  <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)
LOG_FILE <- file.path(RUN_DIR, "extraction.log")

# Timestamped logger — writes to both console and log file
.log <- function(msg) {
  line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

# OMOP NOTE metadata columns to carry through to the output
NOTE_META_COLS <- intersect(
  c("note_date", "note_datetime", "note_type_concept_id",
    "note_class_concept_id", "note_title"),
  names(notes_df)
)

# Process one note; returns list(result, elapsed_sec, status, n_drugs)
.process_note <- function(row, meta_cols) {
  t0   <- proc.time()[["elapsed"]]
  meta <- as.list(row[meta_cols])

  llm_text <- .call_ollama(row[["note_text"]])
  result   <- .parse_llm_output(llm_text, row[["person_id"]], row[["note_id"]], meta)

  list(
    result   = result,
    elapsed  = round(proc.time()[["elapsed"]] - t0, 1),
    status   = result$parse_status[[1]],
    n_drugs  = sum(!is.na(result$dose_value) & result$parse_status == "ok")
  )
}

# ---------------------------------------------------------------------------
# STEP 3c — Parallel extraction with timestamped logging + checkpoints
# ---------------------------------------------------------------------------

n_notes <- nrow(notes_df)
cli::cli_h1("LLM dosage extraction  ({OLLAMA_MODEL})")
.log(sprintf("Notes: %d | Workers: %d | Model: %s | URL: %s",
             n_notes, N_WORKERS, OLLAMA_MODEL, OLLAMA_URL))

future::plan(future::multisession, workers = N_WORKERS)
on.exit(future::plan(future::sequential), add = TRUE)

results_list  <- vector("list", n_notes)
batch_indices <- split(seq_len(n_notes),
                       ceiling(seq_len(n_notes) / CHECKPOINT_EVERY))

for (batch in batch_indices) {
  batch_rows <- notes_df[batch, ]

  # Run this batch in parallel — each worker calls ollama independently
  batch_out <- future.apply::future_lapply(
    seq_len(nrow(batch_rows)),
    function(j) .process_note(batch_rows[j, ], NOTE_META_COLS),
    future.seed    = NULL,
    future.globals = TRUE
  )

  # Log and store results sequentially after each batch
  for (j in seq_along(batch)) {
    i   <- batch[[j]]
    out <- batch_out[[j]]
    results_list[[i]] <- out$result

    .log(sprintf("[%d/%d] note_id=%-10s person_id=%-8s | %-11s | drugs=%-2d | %.1fs",
                 i, n_notes,
                 notes_df$note_id[[i]], notes_df$person_id[[i]],
                 out$status, out$n_drugs, out$elapsed))
  }

  # Checkpoint — flush processed results to disk
  done   <- results_list[!vapply(results_list, is.null, logical(1))]
  chk_df <- dplyr::bind_rows(done)
  readr::write_csv(chk_df, file.path(RUN_DIR, "checkpoint.csv"))
  .log(sprintf("--- checkpoint: %d/%d notes saved ---", max(batch), n_notes))
}

results_df <- dplyr::bind_rows(results_list)

# ---------------------------------------------------------------------------
# STEP 3d — Summary
# ---------------------------------------------------------------------------

cli::cli_h2("Extraction summary")
print(table(results_df$parse_status, useNA = "ifany"))

ok_df <- dplyr::filter(results_df, parse_status == "ok", !is.na(dose_value))
.log(sprintf("Records with a numeric dose: %d / %d", nrow(ok_df), nrow(results_df)))

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
# STEP 4 — Save final results
# ---------------------------------------------------------------------------

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
  sprintf("N_WORKERS:           %d", N_WORKERS),
  sprintf("CHECKPOINT_EVERY:    %d", CHECKPOINT_EVERY),
  sprintf("Source table:        OMOP CDM NOTE (note_text, type=32831)"),
  sprintf("Note meta columns:   %s",
          if (length(NOTE_META_COLS) == 0) "none"
          else paste(NOTE_META_COLS, collapse = ", ")),
  sprintf("Total notes:         %d", n_notes),
  sprintf("Total result rows:   %d", nrow(results_df)),
  sprintf("Rows with dose (ok): %d", nrow(ok_df)),
  sprintf("COHORT_PERSON_IDS:   %s",
          if (is.null(COHORT_PERSON_IDS)) "NULL (all patients)"
          else sprintf("%d person IDs", length(COHORT_PERSON_IDS)))
), con = file.path(RUN_DIR, "params.txt"))

.log(sprintf("Results saved to: %s", RUN_DIR))

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
