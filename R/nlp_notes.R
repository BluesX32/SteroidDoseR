# nlp_notes.R
# medspaCy-based clinical note parser for steroid dose extraction.
#
# Extends the NLP stack to handle unstructured clinical note text (e.g.
# "Patient continued on prednisone 40 mg daily, tapered to 20 mg.").
# The regex SIG parser (parse_sig_one_advanced) runs first; medspaCy fires
# only for rows the regex cannot resolve (taper / free_text / no_parse / empty).
#
# Python dependencies (not auto-installed):
#   pip install medspacy scispacy
#   python -m spacy download en_core_sci_sm

# ---------------------------------------------------------------------------
# Package environment for pipeline caching
# ---------------------------------------------------------------------------

.notes_env <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Dependency guard
# ---------------------------------------------------------------------------

#' @noRd
.check_medspacy <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'reticulate' is required for clinical note parsing.\n",
      "Install it with: install.packages('reticulate')"
    ))
  }
  if (!reticulate::py_module_available("medspacy")) {
    rlang::abort(paste0(
      "Python package 'medspacy' is not available.\n",
      "Install it with:\n",
      "  pip install medspacy scispacy\n",
      "  python -m spacy download en_core_sci_sm\n",
      "Then restart your R session."
    ))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Lazy pipeline loader (one Python load per R session)
# ---------------------------------------------------------------------------

#' @noRd
.get_nlp_pipeline <- function() {
  if (!is.null(.notes_env$pipeline)) {
    return(.notes_env$pipeline)
  }
  .check_medspacy()
  py_file <- system.file("python", "medspacy_pipeline.py", package = "SteroidDoseR")
  reticulate::source_python(py_file)
  .notes_env$pipeline <- reticulate::py$load_pipeline()
  .notes_env$extract_fn <- reticulate::py$extract_steroid_doses
  .notes_env$pipeline
}

# ---------------------------------------------------------------------------
# Python call wrapper
# ---------------------------------------------------------------------------

#' @noRd
.call_medspacy <- function(text) {
  if (is.null(.notes_env$extract_fn)) .get_nlp_pipeline()
  tryCatch(
    .notes_env$extract_fn(text),
    error = function(e) list()
  )
}

# ---------------------------------------------------------------------------
# Entity list → one-row tibble
# ---------------------------------------------------------------------------

#' @noRd
.entities_to_row <- function(entities, note_text) {
  if (length(entities) == 0L) {
    return(.empty_parse_row(note_text, "notes_no_drug") |>
             dplyr::mutate(
               note_entity_count    = 0L,
               note_negation_flag   = FALSE,
               note_uncertainty_flag = FALSE,
               note_section         = NA_character_
             ))
  }

  # Filter: prefer current, non-negated, non-historical entities.
  # Priority: current > uncertain > historical > negated
  active  <- Filter(function(e) !e$negated && !e$historical && !e$uncertain, entities)
  use_set <- if (length(active) > 0L) active else entities

  # Pick the entity with the highest dose (most clinically salient)
  doses <- sapply(use_set, function(e) if (is.null(e$dose_mg)) -Inf else e$dose_mg)
  best  <- use_set[[which.max(doses)]]

  # Detect flags
  any_negated   <- any(sapply(entities, function(e) isTRUE(e$negated)))
  any_uncertain <- any(sapply(entities, function(e) isTRUE(e$uncertain)))
  any_taper     <- any(sapply(entities, function(e) isTRUE(e$taper_flag)))

  dose_mg      <- if (is.null(best$dose_mg))      NA_real_ else as.numeric(best$dose_mg)
  freq_per_day <- if (is.null(best$freq_per_day)) NA_real_ else as.numeric(best$freq_per_day)
  dur_days     <- if (is.null(best$duration_days)) NA_real_ else as.numeric(best$duration_days)
  section      <- if (is.null(best$section) || is.na(best$section)) NA_character_
                  else as.character(best$section)

  # Per-administration mg and daily dose
  mg_per_admin <- dose_mg  # entities report per-dose amounts
  daily_mg     <- if (!is.na(mg_per_admin) && !is.na(freq_per_day)) {
    mg_per_admin * freq_per_day
  } else if (!is.na(mg_per_admin) && is.na(freq_per_day)) {
    # Assume daily when frequency is absent but dose is present
    mg_per_admin
  } else {
    NA_real_
  }

  # Status
  status <- dplyr::case_when(
    any_negated && length(active) == 0L ~ "notes_negated",
    any_uncertain && length(active) == 0L ~ "notes_uncertain",
    !any_negated && length(active) == 0L && any(sapply(entities, function(e) isTRUE(e$historical))) ~ "notes_historical",
    is.na(daily_mg) ~ "notes_no_dose",
    TRUE ~ "notes_ok"
  )

  # Historical / negated / uncertain mentions should not contribute a current dose
  if (status %in% c("notes_historical", "notes_negated", "notes_uncertain")) {
    daily_mg <- NA_real_
  }

  tibble::tibble(
    sig_raw              = note_text,
    tablets              = NA_real_,
    freq_per_day         = freq_per_day,
    mg_per_admin         = mg_per_admin,
    mg_total_flag        = FALSE,
    duration_days        = dur_days,
    taper_flag           = any_taper,
    prn_flag             = FALSE,
    free_text_flag       = TRUE,
    daily_dose_mg        = daily_mg,
    parsed_status        = status,
    note_entity_count    = length(entities),
    note_negation_flag   = any_negated,
    note_uncertainty_flag = any_uncertain,
    note_section         = section
  )
}

# ---------------------------------------------------------------------------
# Single-record note parser implementation
# ---------------------------------------------------------------------------

#' @noRd
.parse_note_one_impl <- function(note_text) {
  if (is.null(note_text) || (length(note_text) == 1L && is.na(note_text)) ||
      nchar(trimws(as.character(note_text))) == 0L) {
    return(.empty_parse_row(note_text, "empty") |>
             dplyr::mutate(
               note_entity_count    = 0L,
               note_negation_flag   = FALSE,
               note_uncertainty_flag = FALSE,
               note_section         = NA_character_
             ))
  }

  entities <- .call_medspacy(as.character(note_text))
  .entities_to_row(entities, note_text)
}

# ---------------------------------------------------------------------------
# Exported: single-record note parser
# ---------------------------------------------------------------------------

#' Parse one clinical note string with medspaCy
#'
#' Extracts steroid dose information from unstructured clinical note text using
#' a medspaCy NLP pipeline. The function **never throws an error** -- malformed
#' or empty inputs return a row of `NA`s with an appropriate `parsed_status`.
#'
#' Requires Python, medspaCy, and (optionally) the scispaCy biomedical model:
#' ```
#' pip install medspacy scispacy
#' python -m spacy download en_core_sci_sm
#' ```
#'
#' @param note_text `character(1)`. Raw clinical note text.
#'
#' @return A one-row `tibble` with the same columns as [parse_sig_one()] plus:
#' \describe{
#'   \item{note_entity_count}{Number of steroid entities found.}
#'   \item{note_negation_flag}{`TRUE` if the steroid mention is negated.}
#'   \item{note_uncertainty_flag}{`TRUE` if the mention is uncertain/hedged.}
#'   \item{note_section}{Detected note section (`"history"`, `"assessment"`,
#'     `"plan"`, etc.), or `NA`.}
#' }
#'
#' `parsed_status` is one of:
#' \describe{
#'   \item{`"notes_ok"`}{Entity found with extractable dose.}
#'   \item{`"notes_negated"`}{Drug negated — patient is NOT on it.}
#'   \item{`"notes_uncertain"`}{Hedged mention (may/possibly).}
#'   \item{`"notes_historical"`}{Drug mentioned only in past context.}
#'   \item{`"notes_no_dose"`}{Steroid found but no dose amount.}
#'   \item{`"notes_no_drug"`}{No steroid entity found.}
#'   \item{`"empty"`}{Input was blank or `NA`.}
#' }
#'
#' @seealso [parse_note_one()], [calc_daily_dose_nlp_notes()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' parse_note_one("Patient continued on prednisone 40 mg daily.")
#' parse_note_one("Patient is NOT on any steroids.")
#' parse_note_one("May consider starting methylprednisolone 1 g IV.")
#' parse_note_one(NA_character_)
#' }
parse_note_one <- function(note_text) {
  tryCatch(
    .parse_note_one_impl(note_text),
    error = function(e) {
      .empty_parse_row(note_text, "error") |>
        dplyr::mutate(
          note_entity_count    = 0L,
          note_negation_flag   = FALSE,
          note_uncertainty_flag = FALSE,
          note_section         = NA_character_
        )
    }
  )
}

# ---------------------------------------------------------------------------
# Exported: vectorised note parser
# ---------------------------------------------------------------------------

#' Apply `parse_note_one()` to every row of a data frame
#'
#' @param drug_df A data frame containing a clinical note column.
#' @param note_col `character(1)`. Name of the note text column.
#'   Default: `"clinical_note"`.
#'
#' @return `drug_df` with parsed columns from [parse_note_one()] appended
#'   (excluding the `sig_raw` duplicate).
#'
#' @seealso [parse_note_one()], [calc_daily_dose_nlp_notes()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   person_id     = 1:2,
#'   clinical_note = c(
#'     "Patient on prednisone 40 mg daily.",
#'     "No steroids currently prescribed."
#'   )
#' )
#' parse_notes(df, note_col = "clinical_note")
#' }
parse_notes <- function(drug_df, note_col = "clinical_note") {
  assert_required_cols(drug_df, note_col, "drug_df")

  parsed <- purrr::map_dfr(drug_df[[note_col]], parse_note_one) |>
    dplyr::select(-"sig_raw")

  dplyr::bind_cols(drug_df, parsed)
}

# ---------------------------------------------------------------------------
# Exported: full notes NLP pipeline
# ---------------------------------------------------------------------------

#' Compute daily steroid doses from clinical note text (medspaCy method)
#'
#' Extends [calc_daily_dose_nlp_advanced()] with a clinical note parsing step.
#' The pipeline runs in this order:
#'
#' 1. **Regex SIG parser** ([parse_sig_one_advanced()]) on the `sig` column —
#'    fast path for structured prescription instructions.
#' 2. **medspaCy** on `note_col` — for rows the regex could not resolve
#'    (`taper`, `free_text`, `no_parse`, `empty` statuses).
#' 3. **Strength fallback** — `amount_value` (unit-checked) → drug concept name.
#' 4. **Baseline structural fallback** (M1/M3/M4) — for rows still `NA`.
#' 5. **Dose plausibility cap** (`max_daily_dose_mg`).
#'
#' Requires Python + medspaCy installed (see [parse_note_one()] for setup).
#'
#' @param connector_or_df A `steroid_connector` or a data frame.
#' @param note_col `character(1)`. Column containing clinical note text.
#'   Default: `"clinical_note"`. Rows with `NA` in this column skip the note
#'   parsing step.
#' @param sig_col `character(1)`. Column with the SIG text. Default: `"sig"`.
#' @param drug_name_col `character(1)`. Column with the drug name.
#'   Default: `"drug_concept_name"`.
#' @param filter_oral `logical(1)`. If `TRUE` (default), only oral-route
#'   records are kept.
#' @param max_daily_dose_mg `numeric(1)` or `NULL`. Upper plausibility bound.
#'   Default: `2000`. Set to `NULL` to disable.
#' @param baseline_fallback `logical(1)`. If `TRUE`, carries through a
#'   pre-existing `daily_dose_mg_orig` column for failed records. Default `FALSE`.
#' @param equiv_table Optional prednisone-equivalency table (columns
#'   `drug_name_std`, `pred_equiv_factor`). Default: built-in table.
#' @param drug_name_map Optional data frame for site-specific drug name
#'   overrides, passed to [standardize_drug_name()]. Default: `NULL`.
#' @param drug_concept_ids,person_ids,start_date,end_date,sig_source
#'   Connector-path filtering arguments. Ignored for plain data frames.
#'
#' @return A data frame with the same columns as [calc_daily_dose_nlp_advanced()]
#'   plus note-specific columns added by [parse_note_one()]:
#'   `note_entity_count`, `note_negation_flag`, `note_uncertainty_flag`,
#'   `note_section`.
#'
#' @seealso [parse_note_one()], [parse_notes()],
#'   [calc_daily_dose_nlp_advanced()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'   person_id                = 1L,
#'   drug_concept_name        = "prednisone 5 MG oral tablet",
#'   route_concept_name       = "oral",
#'   sig                      = NA_character_,
#'   clinical_note            = paste(
#'     "Assessment: Patient continued on prednisone 40 mg daily.",
#'     "Plan: Taper to 20 mg over 4 weeks."
#'   ),
#'   drug_exposure_start_date = as.Date("2023-01-01"),
#'   drug_exposure_end_date   = as.Date("2023-06-01")
#' )
#' calc_daily_dose_nlp_notes(df, note_col = "clinical_note")
#' }
calc_daily_dose_nlp_notes <- function(connector_or_df,
                                      note_col          = "clinical_note",
                                      sig_col           = "sig",
                                      drug_name_col     = "drug_concept_name",
                                      filter_oral       = TRUE,
                                      max_daily_dose_mg = 2000,
                                      baseline_fallback = FALSE,
                                      equiv_table       = NULL,
                                      drug_name_map     = NULL,
                                      drug_concept_ids  = NULL,
                                      person_ids        = NULL,
                                      start_date        = NULL,
                                      end_date          = NULL,
                                      sig_source        = "sig") {

  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date, sig_source)

  assert_required_cols(drug_df, drug_name_col, "drug_df")

  # Insert NA sig column if absent
  if (!sig_col %in% names(drug_df)) {
    drug_df[[sig_col]] <- NA_character_
  }

  # Insert NA note column if absent (note parsing will skip all rows gracefully)
  if (!note_col %in% names(drug_df)) {
    drug_df[[note_col]] <- NA_character_
  }

  # --- standardise drug names --------------------------------------------------
  drug_df <- drug_df |>
    dplyr::mutate(drug_name_std = standardize_drug_name(.data[[drug_name_col]],
                                                         drug_name_map = drug_name_map))

  # --- non-steroid exclusion ---------------------------------------------------
  .etbl          <- if (is.null(equiv_table)) .pred_equiv_table else equiv_table
  known_steroids <- .etbl$drug_name_std[!is.na(.etbl$drug_name_std) & !is.na(.etbl$equiv_factor)]
  drug_df <- drug_df[drug_df$drug_name_std %in% known_steroids, ]

  # --- oral-route filter -------------------------------------------------------
  if (filter_oral) {
    rc <- if ("route_concept_name" %in% names(drug_df)) drug_df$route_concept_name else NULL
    rs <- if ("route_source_value" %in% names(drug_df)) drug_df$route_source_value else NULL
    dc <- if ("drug_concept_name"  %in% names(drug_df)) drug_df$drug_concept_name  else NULL
    ds <- if ("drug_source_value"  %in% names(drug_df)) drug_df$drug_source_value  else NULL

    if (is.null(rc) && is.null(rs) && is.null(dc) && is.null(ds)) {
      rlang::warn("No route column found; skipping oral-route filter.")
    } else {
      route_class <- classify_route(rc, rs, dc, ds)
      drug_df <- drug_df[route_class == "oral" | is.na(route_class), ]
    }
  }

  if (nrow(drug_df) == 0L) {
    rlang::warn(if (filter_oral)
      "No oral corticosteroid records found after filtering."
    else
      "No corticosteroid records found after filtering.")
    return(drug_df)
  }

  # Preserve any pre-existing daily_dose_mg (M1 source) before parse_sig_advanced
  # appends its own daily_dose_mg — bind_cols would create a name collision otherwise.
  if ("daily_dose_mg" %in% names(drug_df)) {
    drug_df[[".daily_dose_mg_m1"]] <- drug_df[["daily_dose_mg"]]
    drug_df[["daily_dose_mg"]]     <- NULL
  }

  # ---------------------------------------------------------------------------
  # Step 1: Regex SIG parser (fast path)
  # ---------------------------------------------------------------------------
  result <- parse_sig_advanced(drug_df, sig_col = sig_col)

  # Add note-specific columns as NA for non-note rows
  result <- result |>
    dplyr::mutate(
      note_entity_count    = NA_integer_,
      note_negation_flag   = NA,
      note_uncertainty_flag = NA,
      note_section         = NA_character_
    )

  # ---------------------------------------------------------------------------
  # Step 2: medspaCy on rows the regex could not resolve
  # ---------------------------------------------------------------------------
  regex_failed <- result$parsed_status %in% c("taper", "free_text", "no_parse", "empty")
  has_note     <- !is.na(result[[note_col]])
  needs_notes  <- regex_failed & has_note

  if (any(needs_notes, na.rm = TRUE)) {
    note_rows <- result[needs_notes, ]

    note_parsed <- purrr::map_dfr(note_rows[[note_col]], parse_note_one)

    # Splice note-parsed columns back into result for the affected rows
    note_cols_to_update <- c(
      "freq_per_day", "mg_per_admin", "mg_total_flag", "duration_days",
      "taper_flag", "prn_flag", "free_text_flag", "daily_dose_mg",
      "parsed_status", "note_entity_count", "note_negation_flag",
      "note_uncertainty_flag", "note_section"
    )
    for (col in note_cols_to_update) {
      if (col %in% names(note_parsed)) {
        result[[col]][needs_notes] <- note_parsed[[col]]
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Step 3: Strength fallback (amount_value → drug concept name)
  # ---------------------------------------------------------------------------
  has_no_mg <- result$parsed_status %in% c("no_parse", "notes_no_dose") &
               is.na(result$mg_per_admin)

  if (any(has_no_mg, na.rm = TRUE)) {
    av <- if ("amount_value" %in% names(result)) {
      raw_av <- safe_as_numeric(result$amount_value)
      if ("amount_unit_concept_id" %in% names(result)) {
        uid <- safe_as_numeric(result$amount_unit_concept_id)
        dplyr::if_else(!is.na(uid) & uid != 8576L, NA_real_, raw_av)
      } else raw_av
    } else {
      rep(NA_real_, nrow(result))
    }

    name_col <- intersect(c("drug_concept_name", "drug_source_value"), names(result))
    sn <- if (length(name_col) > 0L) {
      stringr::str_match(
        stringr::str_to_lower(as.character(result[[name_col[[1L]]]])),
        "(\\d+(?:\\.\\d+)?)\\s*mg"
      )[, 2L] |> safe_as_numeric()
    } else {
      rep(NA_real_, nrow(result))
    }

    strength_fb <- dplyr::coalesce(av, sn)

    result <- result |>
      dplyr::mutate(
        needs_mg_fb   = has_no_mg & !is.na(strength_fb),
        mg_per_admin  = dplyr::if_else(
          .data$needs_mg_fb,
          strength_fb * dplyr::coalesce(.data$tablets, 1),
          .data$mg_per_admin
        ),
        daily_dose_mg = dplyr::if_else(
          .data$needs_mg_fb & !is.na(.data$freq_per_day),
          .data$mg_per_admin * .data$freq_per_day,
          .data$daily_dose_mg
        ),
        parsed_status = dplyr::if_else(
          .data$needs_mg_fb & !is.na(.data$daily_dose_mg),
          "ok",
          .data$parsed_status
        )
      ) |>
      dplyr::select(-"needs_mg_fb")
  }

  # ---------------------------------------------------------------------------
  # Step 4: Baseline structural fallback (M1/M3/M4) for still-NA rows
  # ---------------------------------------------------------------------------
  still_na <- is.na(result$daily_dose_mg)
  if (any(still_na, na.rm = TRUE)) {
    orig_status  <- result$parsed_status[still_na]
    bl_input     <- result[still_na, ]
    # Restore the saved M1 column so calc_daily_dose_baseline can use it
    if (".daily_dose_mg_m1" %in% names(bl_input)) {
      bl_input[["daily_dose_mg"]]     <- bl_input[[".daily_dose_mg_m1"]]
      bl_input[[".daily_dose_mg_m1"]] <- NULL
    }
    bl <- calc_daily_dose_baseline(
      bl_input,
      filter_oral       = FALSE,
      m2_sig_parse      = "none",
      max_daily_dose_mg = max_daily_dose_mg,
      equiv_table       = equiv_table,
      drug_name_map     = drug_name_map,
      methods           = c("original", "actual_duration", "supply_based")
    )
    result$daily_dose_mg[still_na] <- bl$daily_dose_mg_imputed
    fb_label <- paste0("fallback_", bl$imputation_method)
    fb_label[bl$imputation_method == "missing"] <- orig_status[bl$imputation_method == "missing"]
    result$parsed_status[still_na] <- fb_label
  }

  # ---------------------------------------------------------------------------
  # Step 5: Dose plausibility cap
  # ---------------------------------------------------------------------------
  if (!is.null(max_daily_dose_mg) && is.finite(max_daily_dose_mg)) {
    implausible <- !is.na(result$daily_dose_mg) &
                   result$daily_dose_mg > max_daily_dose_mg
    if (any(implausible, na.rm = TRUE)) {
      rlang::warn(sprintf(
        paste0(
          "%d record(s) have daily_dose_mg > %.0f mg/day and were set to NA.\n",
          "Pass max_daily_dose_mg = NULL to disable this cap."
        ),
        sum(implausible), max_daily_dose_mg
      ))
      result$daily_dose_mg[implausible] <- NA_real_
      result$parsed_status[implausible] <- "implausible"
    }
  }

  # Drop the internal M1 stash column if it wasn't consumed by baseline fallback
  if (".daily_dose_mg_m1" %in% names(result)) {
    result[[".daily_dose_mg_m1"]] <- NULL
  }

  result
}
