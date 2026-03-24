# nlp_advanced.R
# Enhanced rule-based NLP SIG parser with taper decomposition.
#
# Extends the rule-based engine in nlp.R with:
#   - Word-form tablet counts  ("one tablet", "half a tablet", "1/2 tablet")
#   - Fractional tablets       ("1/2", "1/2", "1 and a half tablets")
#   - Weekly / monthly freqs   ("once weekly", "twice a week", "monthly")
#   - Generalised every-N-days ("every 3 days", "q48h", "q72h")
#   - Generalised every-N-hours ("every 4 hours", "q4h")
#   - Taper schedule parser    (parse_taper_schedule) -- explicit multi-step
#                               and decrement-by patterns
#   - Taper expansion          (expand_tapers = TRUE in calc_daily_dose_nlp_advanced)
#   - Dose plausibility cap    (max_daily_dose_mg, matching baseline)
#
# All public functions are never-error: malformed / empty input returns a
# row of NAs with an appropriate parsed_status.

# ---------------------------------------------------------------------------
# Internal helper: word-form -> numeric
# ---------------------------------------------------------------------------

#' @noRd
.parse_word_num <- function(x) {
  dplyr::case_when(
    x %in% c("half", "1/2", "\u00bd")      ~ 0.5,
    x %in% c("one", "a", "an")             ~ 1,
    x == "two"                              ~ 2,
    x == "three"                            ~ 3,
    x == "four"                             ~ 4,
    x == "five"                             ~ 5,
    x == "six"                              ~ 6,
    TRUE                                    ~ suppressWarnings(as.numeric(x))
  )
}

# ---------------------------------------------------------------------------
# Internal helper: enhanced tablet extraction
# ---------------------------------------------------------------------------

# Handles numeric, decimal, fractional (1/2, 1/2), word-form, and
# "X and a half" compounds.  `s` must already be normalised (lowercase).
#' @noRd
.extract_tablets_adv <- function(s) {
  unit_pat <- "(?:tablets?|tabs?|pills?|capsules?|caps?)"

  # 1. "N and a half tablets" -- numeric lead
  m <- stringr::str_match(
    s, paste0("(\\d+(?:\\.\\d+)?)\\s+and\\s+(?:a\\s+)?half\\s+", unit_pat)
  )
  if (!is.na(m[1L, 1L])) return(safe_as_numeric(m[1L, 2L]) + 0.5)

  # 2. Plain numeric, decimal, or fractional: "2 tablets", "1.5 tabs", "1/2 tab"
  m <- stringr::str_match(
    s, paste0("(\\d+(?:\\.\\d+)?|1/2|\u00bd)\\s*", unit_pat)
  )
  if (!is.na(m[1L, 1L])) {
    v <- m[1L, 2L]
    if (v %in% c("1/2", "\u00bd")) return(0.5)
    return(safe_as_numeric(v))
  }

  # 3. "word and a half tablets" -- word lead
  m <- stringr::str_match(
    s, paste0("\\b(one|two|three|four|five|six|half|a|an)\\s+and\\s+(?:a\\s+)?half\\s+", unit_pat)
  )
  if (!is.na(m[1L, 1L])) return(.parse_word_num(m[1L, 2L]) + 0.5)

  # 4. Plain word-form: "one tablet", "half a tablet", "a tablet"
  m <- stringr::str_match(
    s, paste0("\\b(one|two|three|four|five|six|half|a|an)\\s*", unit_pat)
  )
  if (!is.na(m[1L, 1L])) return(.parse_word_num(m[1L, 2L]))

  NA_real_
}

# ---------------------------------------------------------------------------
# Internal: implementation of the advanced single-record parser
# ---------------------------------------------------------------------------

#' @noRd
.parse_sig_one_adv_impl <- function(sig_text) {
  if (is.null(sig_text) ||
      (length(sig_text) == 1L && is.na(sig_text)) ||
      nchar(trimws(as.character(sig_text))) == 0L) {
    return(.empty_parse_row(sig_text, "empty"))
  }

  s <- .norm_sig(sig_text)
  s <- .preprocess_sig(s)

  # ---- Flags ----------------------------------------------------------------
  prn_flag <- stringr::str_detect(
    s, "\\bprn\\b|as needed|when needed|if needed"
  )
  free_text_flag <- stringr::str_detect(
    s, "as directed|use as directed|see attach|per md|per ng|per physician|per doctor|per provider"
  )
  taper_flag <- stringr::str_detect(
    s, "taper|decreas|reducing|reduce by|\\bdrop\\b|\\bthen\\b.*\\bmg\\b|\\bthen\\b.*\\btabs?\\b|alternate day|\\bqod\\b|every other day"
  )

  # ---- Tablets (enhanced) --------------------------------------------------
  tablets <- .extract_tablets_adv(s)
  # Default to 1 when SIG contains no explicit tablet count.
  tablets <- dplyr::if_else(is.na(tablets), 1, tablets)

  # ---- Frequency (enhanced) ------------------------------------------------
  freq <- .extract_freq(s)

  # ---- Duration ------------------------------------------------------------
  dur_match <- stringr::str_match(
    s, "(?:for|x)\\s*(\\d+(?:\\.\\d+)?)\\s*(day|days|wk|wks|week|weeks|mo|mos|month|months)"
  )
  dur_num  <- safe_as_numeric(dur_match[, 2L])
  dur_unit <- dur_match[, 3L]

  duration_days <- dplyr::case_when(
    is.na(dur_num)                               ~ NA_real_,
    stringr::str_detect(dur_unit, "^day")        ~ dur_num,
    stringr::str_detect(dur_unit, "^wk|^week")  ~ dur_num * 7,
    stringr::str_detect(dur_unit, "^mo")         ~ dur_num * 30,
    TRUE                                         ~ NA_real_
  )

  # ---- mg extraction (same hierarchy as parse_sig_one) --------------------
  mg_total <- stringr::str_match(
    s, "\\((\\d+(?:\\.\\d+)?)\\s*mg\\s*total\\)"
  )[, 2L] |> safe_as_numeric()

  mg_per_dose <- stringr::str_match(
    s, "\\((\\d+(?:\\.\\d+)?)\\s*mg\\s*per\\s*dose\\)"
  )[, 2L] |> safe_as_numeric()

  mg_paren_plain <- if (is.na(mg_per_dose) && is.na(mg_total)) {
    stringr::str_match(s, "\\((\\d+(?:\\.\\d+)?)\\s*mg\\)")[, 2L] |>
      safe_as_numeric()
  } else NA_real_

  # 3.5. Bare "X mg/day", "X mg per day", "X mg a day" — explicit daily total.
  #      Must precede mg_bare so these strings don't get per-tablet treatment.
  mg_per_day <- if (is.na(mg_per_dose) && is.na(mg_total) && is.na(mg_paren_plain)) {
    stringr::str_match(
      s,
      "(?<!\\()\\b(\\d+(?:\\.\\d+)?)\\s*mg\\s*(?:/\\s*day|per\\s*day|a\\s*day)\\b"
    )[, 2L] |> safe_as_numeric()
  } else NA_real_

  mg_bare <- if (is.na(mg_per_dose) && is.na(mg_total) && is.na(mg_paren_plain) &&
                 is.na(mg_per_day)) {
    stringr::str_match(s, "(?<!\\()\\b(\\d+(?:\\.\\d+)?)\\s*mg\\b")[, 2L] |>
      safe_as_numeric()
  } else NA_real_

  # ---- per-administration mg -----------------------------------------------
  mg_per_admin <- dplyr::case_when(
    !is.na(mg_total)                              ~ NA_real_,
    !is.na(mg_per_day)                            ~ NA_real_,
    !is.na(mg_per_dose)                           ~ mg_per_dose,
    !is.na(mg_paren_plain) & !is.na(tablets)     ~ mg_paren_plain * tablets,
    !is.na(mg_paren_plain)                        ~ mg_paren_plain,
    !is.na(mg_bare) & !is.na(tablets)            ~ mg_bare * tablets,
    !is.na(mg_bare)                               ~ mg_bare,
    TRUE                                          ~ NA_real_
  )

  # ---- daily dose mg -------------------------------------------------------
  daily_mg <- dplyr::case_when(
    !is.na(mg_total)    ~ mg_total,
    !is.na(mg_per_day)  ~ mg_per_day,
    !is.na(mg_per_admin) & !is.na(freq) ~ mg_per_admin * freq,
    !is.na(mg_per_admin) & is.na(freq) &
      stringr::str_detect(s,
        "daily|every day|once daily|\\bqd\\b|with breakfast|every morning|qam|every 24 hours?") ~ mg_per_admin,
    TRUE ~ NA_real_
  )

  # ---- parsed_status -------------------------------------------------------
  status <- dplyr::case_when(
    free_text_flag ~ "free_text",
    taper_flag     ~ "taper",
    prn_flag       ~ "prn",
    !is.na(daily_mg) ~ "ok",
    TRUE ~ "no_parse"
  )

  tibble::tibble(
    sig_raw        = sig_text,
    tablets        = tablets,
    freq_per_day   = freq,
    mg_per_admin   = mg_per_admin,
    mg_total_flag  = !is.na(mg_total) | !is.na(mg_per_day),
    duration_days  = duration_days,
    taper_flag     = taper_flag,
    prn_flag       = prn_flag,
    free_text_flag = free_text_flag,
    daily_dose_mg  = daily_mg,
    parsed_status  = status
  )
}

# ---------------------------------------------------------------------------
# Exported: single-record advanced parser
# ---------------------------------------------------------------------------

#' Parse one SIG string with enhanced vocabulary (advanced NLP)
#'
#' Drop-in replacement for [parse_sig_one()] that adds:
#' - Word-form tablet counts: `"one tablet"`, `"half a tablet"`, `"1/2 tab"`
#' - Weekly / monthly frequencies: `"once weekly"`, `"twice a week"`, `"monthly"`
#' - Generalised every-N-days: `"every 3 days"`, `"q48h"`, `"q72h"`
#' - Sub-daily every-N-hours: `"every 4 hours"`, `"q4h"`
#'
#' Returns the same columns as [parse_sig_one()] for full compatibility.
#' The function **never throws an error**.
#'
#' @param sig_text `character(1)`. Raw SIG string.
#'
#' @return A one-row `tibble` with the same columns as [parse_sig_one()].
#'
#' @seealso [parse_sig_one()], [parse_taper_schedule()],
#'   [calc_daily_dose_nlp_advanced()]
#'
#' @export
#'
#' @examples
#' parse_sig_one_advanced("Take one tablet daily")
#' parse_sig_one_advanced("Take half a tablet (5 mg) once weekly")
#' parse_sig_one_advanced("Take 2 tabs every 3 days")
#' parse_sig_one_advanced("40 mg daily x 4 weeks then 20 mg daily x 4 weeks")
parse_sig_one_advanced <- function(sig_text) {
  tryCatch(
    .parse_sig_one_adv_impl(sig_text),
    error = function(e) .empty_parse_row(sig_text, "error")
  )
}

# ---------------------------------------------------------------------------
# Exported: vectorised advanced parser
# ---------------------------------------------------------------------------

#' Apply `parse_sig_one_advanced()` to every row of a data frame
#'
#' Drop-in replacement for [parse_sig()] using the enhanced vocabulary.
#'
#' @param drug_df A data frame containing a SIG text column.
#' @param sig_col `character(1)`. Name of the SIG column. Default: `"sig"`.
#'
#' @return `drug_df` with parsed columns from [parse_sig_one_advanced()]
#'   appended (excluding the `sig_raw` duplicate).
#'
#' @seealso [parse_sig()], [parse_sig_one_advanced()]
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id = 1:2,
#'   sig = c("Take one tablet (5 mg) daily", "Take half tab every 3 days")
#' )
#' parse_sig_advanced(df)
parse_sig_advanced <- function(drug_df, sig_col = "sig") {
  assert_required_cols(drug_df, sig_col, "drug_df")

  parsed <- purrr::map_dfr(drug_df[[sig_col]], parse_sig_one_advanced) |>
    dplyr::select(-"sig_raw")

  dplyr::bind_cols(drug_df, parsed)
}

# ---------------------------------------------------------------------------
# Exported: taper schedule decomposer
# ---------------------------------------------------------------------------

#' Decompose a taper SIG string into a per-step dose schedule
#'
#' Attempts to parse a steroid taper instruction into a table of dose steps,
#' each with its daily dose, frequency, duration, and start/end day offsets
#' relative to the prescription start date.
#'
#' Two strategies are attempted in order:
#' 1. **Explicit multi-step** -- SIG contains two or more `"X mg ... for N
#'    units"` blocks separated by `"then"`, commas, or semicolons.
#' 2. **Decrement pattern** -- SIG states a starting dose and a fixed
#'    decrement: `"60 mg daily then decrease by 10 mg every week"`.
#'
#' Returns `NULL` when neither strategy produces >= 2 parseable steps.
#'
#' @param sig_text `character(1)`. The raw SIG string.
#'
#' @return A `tibble` with one row per taper step and columns:
#' \describe{
#'   \item{step}{Integer step number (1, 2, ...).}
#'   \item{daily_dose_mg}{Daily dose for this step (mg/day).}
#'   \item{freq_per_day}{Inferred doses per day (1 = daily by default).}
#'   \item{duration_days}{Length of this step in days.}
#'   \item{step_start_day}{Days from prescription start to step start (0-based).}
#'   \item{step_end_day}{Days from prescription start to step end (inclusive).}
#' }
#' Returns `NULL` when the SIG cannot be decomposed into >= 2 steps.
#'
#' @export
#'
#' @examples
#' parse_taper_schedule(
#'   "60 mg daily for 2 weeks, then 40 mg daily for 2 weeks, then 20 mg daily for 2 weeks"
#' )
#' parse_taper_schedule(
#'   "start 60 mg then decrease by 10 mg every week"
#' )
#' parse_taper_schedule("Take 1 tablet daily")  # returns NULL
parse_taper_schedule <- function(sig_text) {
  if (is.null(sig_text) || is.na(sig_text) ||
      nchar(trimws(as.character(sig_text))) == 0L) {
    return(NULL)
  }

  s <- .norm_sig(sig_text)

  # Must have at least some taper signal or multiple mg amounts
  has_taper_word <- stringr::str_detect(s, "then|taper|decreas|reduce|drop")
  has_multi_mg   <- length(stringr::str_extract_all(s, "\\d+(?:\\.\\d+)?\\s*mg")[[1L]]) >= 2L
  if (!has_taper_word && !has_multi_mg) return(NULL)

  steps <- tryCatch(.parse_explicit_taper(s), error = function(e) NULL)
  if (!is.null(steps) && nrow(steps) >= 2L) return(steps)

  tryCatch(.parse_decrement_taper(s), error = function(e) NULL)
}

# ---------------------------------------------------------------------------
# Internal: explicit multi-step taper parser
# ---------------------------------------------------------------------------

#' @noRd
.parse_explicit_taper <- function(s) {
  # Split on: ", then", " then ", ";", or a comma before a digit+mg pattern
  pieces <- stringr::str_split(
    s,
    ",?\\s+then\\s+|\\s*;\\s*|,\\s*(?=\\d+(?:\\.\\d+)?\\s*mg)"
  )[[1L]]
  pieces <- stringr::str_squish(pieces)
  pieces <- pieces[nchar(pieces) > 0L]

  if (length(pieces) < 2L) return(NULL)

  # Regex: <dose> mg [optional stuff <=50 chars, non-greedy] for/x <N> <unit>
  step_re <- paste0(
    "(\\d+(?:\\.\\d+)?)\\s*mg",  # dose
    "(?:/day|/d(?:ay)?)?",        # optional /day
    "[^.!?;]{0,50}?",             # optional route/freq qualifier (non-greedy)
    "\\b(?:for|x)\\b\\s*",        # duration separator
    "(\\d+(?:\\.\\d+)?)\\s*",    # N
    "(days?|wks?|weeks?|mos?|months?)"  # unit
  )

  rows <- lapply(pieces, function(p) {
    m <- stringr::str_match(p, step_re)
    if (is.na(m[1L, 1L])) return(NULL)

    dose_mg  <- safe_as_numeric(m[1L, 2L])
    dur_num  <- safe_as_numeric(m[1L, 3L])
    dur_unit <- m[1L, 4L]

    freq <- .extract_freq(p)
    if (is.na(freq)) freq <- 1  # taper steps are assumed daily unless stated

    dur_days <- dplyr::case_when(
      stringr::str_detect(dur_unit, "^day")       ~ dur_num,
      stringr::str_detect(dur_unit, "^wk|^week")  ~ dur_num * 7,
      stringr::str_detect(dur_unit, "^mo")         ~ dur_num * 30,
      TRUE                                         ~ NA_real_
    )

    if (is.na(dose_mg) || is.na(dur_days)) return(NULL)

    # In taper SIGs, the mg figure is conventionally the daily total.
    # If "per dose" is explicitly stated and freq > 1, multiply.
    is_per_dose <- stringr::str_detect(p, "per\\s*dose|each\\s*dose")
    daily_mg    <- if (is_per_dose && !is.na(freq) && freq > 1) dose_mg * freq else dose_mg

    tibble::tibble(
      daily_dose_mg = daily_mg,
      freq_per_day  = freq,
      duration_days = dur_days
    )
  })

  rows <- rows[!sapply(rows, is.null)]
  if (length(rows) < 2L) return(NULL)

  dplyr::bind_rows(rows) |>
    dplyr::mutate(
      step           = dplyr::row_number(),
      step_start_day = cumsum(dplyr::lag(.data$duration_days, default = 0)),
      step_end_day   = .data$step_start_day + .data$duration_days - 1
    ) |>
    dplyr::select("step", "daily_dose_mg", "freq_per_day",
                  "duration_days", "step_start_day", "step_end_day")
}

# ---------------------------------------------------------------------------
# Internal: decrement-pattern taper parser
# ---------------------------------------------------------------------------

#' @noRd
.parse_decrement_taper <- function(s) {
  # Pattern: "60 mg daily then decrease by 10 mg every week" or
  #          "start at 60 mg, taper by 5 mg each month"

  # Starting dose: at the beginning of the string, or after "start/begin/continue"
  start_m <- stringr::str_match(
    s,
    "(?:^\\s*|(?:start(?:ing)?|begin(?:ning)?|continue)\\s+(?:at\\s+)?)(\\d+(?:\\.\\d+)?)\\s*mg"
  )
  start_dose <- safe_as_numeric(start_m[1L, 2L])
  if (is.na(start_dose)) return(NULL)

  # Decrement: "decrease/reduce/taper/drop [OPTIONAL WORDS] [by] X mg every/each/per [N] UNIT"
  # - Allows "drop dose by 0.5 mg" (≤ 2 intervening words before "by")
  # - Allows "every 4 weeks" (captures optional interval number before unit)
  dec_m <- stringr::str_match(
    s,
    "(?:decrease|decreas|reduce|taper|drop)\\s+(?:\\w+\\s+){0,2}(?:by\\s+)?(\\d+(?:\\.\\d+)?)\\s*mg\\s+(?:each|every|per)\\s*(\\d+)?\\s*(days?|wks?|weeks?|mos?|months?)"
  )
  if (is.na(dec_m[1L, 1L])) return(NULL)

  dec_amount    <- safe_as_numeric(dec_m[1L, 2L])
  interval_num  <- safe_as_numeric(dec_m[1L, 3L])   # e.g. 4 from "every 4 weeks"; NA → 1
  dec_unit      <- dec_m[1L, 4L]

  if (is.na(interval_num)) interval_num <- 1

  dec_days_base <- dplyr::case_when(
    stringr::str_detect(dec_unit, "^day")       ~ 1,
    stringr::str_detect(dec_unit, "^wk|^week")  ~ 7,
    stringr::str_detect(dec_unit, "^mo")         ~ 30,
    TRUE                                         ~ NA_real_
  )
  dec_days <- dec_days_base * interval_num

  if (is.na(dec_days) || is.na(dec_amount) ||
      dec_amount <= 0 || dec_amount >= start_dose) {
    return(NULL)
  }

  # Generate descending dose steps down to the decrement amount
  doses <- seq(from = start_dose, to = dec_amount, by = -dec_amount)
  if (length(doses) < 2L) return(NULL)

  tibble::tibble(
    step           = seq_along(doses),
    daily_dose_mg  = doses,
    freq_per_day   = 1,
    duration_days  = dec_days,
    step_start_day = (seq_along(doses) - 1L) * dec_days,
    step_end_day   = seq_along(doses) * dec_days - 1
  )
}

# ---------------------------------------------------------------------------
# Internal: expand taper rows into per-step rows
# ---------------------------------------------------------------------------

#' @noRd
.expand_taper_records <- function(result, sig_col) {
  is_taper <- !is.na(result$parsed_status) & result$parsed_status == "taper"

  # Add step columns to all rows with NA (filled in for expanded rows only)
  result <- result |>
    dplyr::mutate(
      taper_step     = NA_integer_,
      step_start_day = NA_real_,
      step_end_day   = NA_real_
    )

  if (!any(is_taper, na.rm = TRUE)) return(result)

  non_taper_part <- result[!is_taper, ]
  taper_part     <- result[ is_taper, ]

  expanded_list <- lapply(seq_len(nrow(taper_part)), function(i) {
    row      <- taper_part[i, ]
    sig_text <- if (sig_col %in% names(row)) as.character(row[[sig_col]]) else NA_character_
    steps    <- tryCatch(parse_taper_schedule(sig_text), error = function(e) NULL)

    if (is.null(steps) || nrow(steps) == 0L) {
      return(row)  # keep original row; taper_step = NA signals no decomposition
    }

    n_steps  <- nrow(steps)
    rep_rows <- row[rep(1L, n_steps), ]

    rep_rows$taper_step     <- steps$step
    rep_rows$step_start_day <- steps$step_start_day
    rep_rows$step_end_day   <- steps$step_end_day
    rep_rows$daily_dose_mg  <- steps$daily_dose_mg
    rep_rows$freq_per_day   <- steps$freq_per_day
    rep_rows$duration_days  <- steps$duration_days
    rep_rows$parsed_status  <- rep("taper_ok", n_steps)

    rep_rows
  })

  dplyr::bind_rows(non_taper_part, dplyr::bind_rows(expanded_list))
}

# ---------------------------------------------------------------------------
# Exported: full advanced NLP pipeline
# ---------------------------------------------------------------------------

#' Compute daily steroid doses using the advanced NLP method
#'
#' Extends [calc_daily_dose_nlp()] with:
#' - Enhanced vocabulary: word-form tablets, weekly/monthly frequencies,
#'   generalised every-N-days and every-N-hours patterns.
#' - Taper schedule decomposition: when `expand_tapers = TRUE`, records
#'   flagged as taper are decomposed into per-step rows, each with its own
#'   `daily_dose_mg`, `taper_step`, `step_start_day`, and `step_end_day`.
#' - Dose plausibility cap: records above `max_daily_dose_mg` are set to
#'   `NA` with a warning (matching the Baseline method).
#'
#' The first argument accepts either a **connector** (created by
#' [create_omop_connector()] or [create_df_connector()]) or a plain
#' **data frame**.
#'
#' @param connector_or_df A `steroid_connector` or a data frame.
#' @param drug_name_col `character(1)`. Column with the drug name.
#'   Default: `"drug_concept_name"`.
#' @param sig_col `character(1)`. Column with the SIG text. Default: `"sig"`.
#' @param filter_oral `logical(1)`. If `TRUE` (default), only oral-route
#'   records are kept.
#' @param expand_tapers `logical(1)`. If `TRUE` (default), taper records with a
#'   parseable schedule are expanded into multiple rows -- one per dose step --
#'   with columns `taper_step`, `step_start_day`, and `step_end_day` added.
#'   Records whose taper cannot be decomposed keep `taper_step = NA`.
#'   Default: `FALSE`.
#' @param max_daily_dose_mg `numeric(1)`. Records with `daily_dose_mg` above
#'   this value are set to `NA` with a diagnostic warning. Pass `NULL` to
#'   disable. Default: `2000`.
#' @param baseline_fallback `logical(1)`. If `TRUE`, carries through an
#'   existing `daily_dose_mg_orig` column for failed records. Default: `FALSE`.
#' @param equiv_table Optional data frame with the prednisone-equivalency table.
#'   Must contain columns `drug_name_std` and `pred_equiv_factor`. When `NULL`
#'   (default), the built-in `.pred_equiv_table` is used.
#' @param drug_name_map Optional data frame passed to [standardize_drug_name()]
#'   for site-specific drug name overrides. Default: `NULL`.
#' @param drug_concept_ids,person_ids,start_date,end_date,sig_source
#'   Connector-path filtering arguments. Ignored when `connector_or_df` is a
#'   data frame.
#'
#' @return A data frame with the same columns as [calc_daily_dose_nlp()] plus:
#' - When `expand_tapers = TRUE`: `taper_step` (int), `step_start_day`
#'   (num), `step_end_day` (num) -- present on every row, `NA` for
#'   non-taper records.
#'
#' @seealso [calc_daily_dose_nlp()], [parse_taper_schedule()],
#'   [parse_sig_one_advanced()]
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id              = 1L,
#'   drug_concept_name      = "prednisone 5 MG oral tablet",
#'   route_concept_name     = "oral",
#'   sig                    = "Take one tablet (5 mg) daily",
#'   drug_exposure_start_date = as.Date("2023-01-01"),
#'   drug_exposure_end_date   = as.Date("2023-03-01")
#' )
#' calc_daily_dose_nlp_advanced(df)
#'
#' # With taper expansion
#' df_taper <- tibble::tibble(
#'   person_id              = 1L,
#'   drug_concept_name      = "prednisone 10 MG oral tablet",
#'   route_concept_name     = "oral",
#'   sig                    = "60 mg daily for 2 weeks, then 40 mg daily for 2 weeks, then 20 mg daily for 2 weeks",
#'   drug_exposure_start_date = as.Date("2023-01-01"),
#'   drug_exposure_end_date   = as.Date("2023-07-01")
#' )
#' calc_daily_dose_nlp_advanced(df_taper, expand_tapers = TRUE)
calc_daily_dose_nlp_advanced <- function(connector_or_df,
                                         drug_name_col     = "drug_concept_name",
                                         sig_col           = "sig",
                                         filter_oral       = TRUE,
                                         expand_tapers     = TRUE,
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

  if (!sig_col %in% names(drug_df)) {
    drug_df[[sig_col]] <- NA_character_
  }

  # --- standardise drug names (always) ----------------------------------------
  drug_df <- drug_df |>
    dplyr::mutate(drug_name_std = standardize_drug_name(.data[[drug_name_col]],
                                                         drug_name_map = drug_name_map))

  # --- non-steroid exclusion (always) ------------------------------------------
  .etbl          <- if (is.null(equiv_table)) .pred_equiv_table else equiv_table
  known_steroids <- .etbl$drug_name_std[!is.na(.etbl$drug_name_std) & !is.na(.etbl$equiv_factor)]
  drug_df <- drug_df[drug_df$drug_name_std %in% known_steroids, ]

  # --- filter to oral corticosteroids ------------------------------------------
  if (filter_oral) {
    rc <- if ("route_concept_name" %in% names(drug_df)) drug_df$route_concept_name else NULL
    rs <- if ("route_source_value" %in% names(drug_df)) drug_df$route_source_value else NULL
    ds <- if ("drug_source_value"  %in% names(drug_df)) drug_df$drug_source_value  else NULL

    if (is.null(rc) && is.null(rs) && is.null(ds)) {
      rlang::warn("No route column found; skipping oral-route filter.")
    } else {
      route_class <- classify_route(rc, rs, ds)
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

  # --- parse SIG strings (enhanced) -----------------------------------------
  result <- parse_sig_advanced(drug_df, sig_col = sig_col)

  # --- strength fallback: amount_value / drug concept name -------------------
  # Same logic as calc_daily_dose_nlp(): when the SIG has freq but no mg,
  # look up the per-tablet strength in amount_value or the drug concept name.
  has_no_mg <- result$parsed_status == "no_parse" & is.na(result$mg_per_admin)
  if (any(has_no_mg, na.rm = TRUE)) {
    av <- if ("amount_value" %in% names(result))
      safe_as_numeric(result$amount_value)
    else
      rep(NA_real_, nrow(result))

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

  # --- dose plausibility cap -------------------------------------------------
  if (!is.null(max_daily_dose_mg) && is.finite(max_daily_dose_mg)) {
    implausible <- !is.na(result$daily_dose_mg) &
                   result$daily_dose_mg > max_daily_dose_mg
    if (any(implausible, na.rm = TRUE)) {
      rlang::warn(sprintf(
        paste0(
          "%d record(s) have daily_dose_mg > %.0f mg/day and were set to NA.\n",
          "Possible causes: SIG with total-course dose, unit mismatch, or ",
          "data-entry error.\nPass max_daily_dose_mg = NULL to disable this cap."
        ),
        sum(implausible), max_daily_dose_mg
      ))
      result$daily_dose_mg[implausible] <- NA_real_
      result$parsed_status[implausible] <- "implausible"
    }
  }

  # --- taper expansion -------------------------------------------------------
  if (expand_tapers) {
    result <- .expand_taper_records(result, sig_col = sig_col)
  }

  # --- optional baseline fallback (legacy: use pre-existing column) ----------
  if (baseline_fallback && "daily_dose_mg_orig" %in% names(drug_df)) {
    result <- result |>
      dplyr::mutate(
        daily_dose_mg = dplyr::coalesce(
          .data$daily_dose_mg,
          safe_as_numeric(.data$daily_dose_mg_orig)
        )
      )
  }

  # --- structural fallback: baseline M1/M3/M4 for records still NA ----------
  still_na <- is.na(result$daily_dose_mg) &
              result$parsed_status %in% c("no_parse", "empty")
  if (any(still_na, na.rm = TRUE)) {
    bl <- calc_daily_dose_baseline(
      result[still_na, ],
      filter_oral       = FALSE,
      m2_sig_parse      = "none",
      max_daily_dose_mg = max_daily_dose_mg,
      equiv_table       = equiv_table,
      drug_name_map     = drug_name_map,
      methods           = c("original", "actual_duration", "supply_based")
    )
    result$daily_dose_mg[still_na] <- bl$daily_dose_mg_imputed
    fb_label <- paste0("fallback_", bl$imputation_method)
    fb_label[bl$imputation_method == "missing"] <- "no_parse"
    result$parsed_status[still_na] <- fb_label
  }

  result
}
