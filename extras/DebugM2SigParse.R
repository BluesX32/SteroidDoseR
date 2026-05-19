# extras/DebugM2SigParse.R
#
# Imputation coverage funnel for calc_daily_dose_baseline()
#
# Answers two questions:
#   1. How many oral tablet records need imputation (M1 not available)?
#   2. Of those, how many get successfully imputed and reach final analysis?
#
# Run AFTER CodeToRun.R has created `drug_df` in the same R session.
# Requires the SteroidDoseR package to be loaded (devtools::load_all(".") is fine).

stopifnot(exists("drug_df"), is.data.frame(drug_df))
library(dplyr)

cat("\n==============================================================\n")
cat("  Baseline imputation coverage funnel\n")
cat("==============================================================\n\n")

# ---------------------------------------------------------------------------
# Step 0 — raw records
# ---------------------------------------------------------------------------
n_raw <- nrow(drug_df)
cat(sprintf("[0] Raw drug_df records: %d\n\n", n_raw))

# ---------------------------------------------------------------------------
# Step 1 — known-steroid filter (same logic as baseline.R step 0b)
# ---------------------------------------------------------------------------
df1 <- drug_df
if (!"drug_name_std" %in% names(df1)) {
  name_col <- intersect(c("drug_concept_name", "drug_source_value"), names(df1))
  if (length(name_col) > 0L)
    df1 <- df1 |> mutate(drug_name_std = standardize_drug_name(.data[[name_col[[1L]]]]))
}

etbl          <- SteroidDoseR:::.pred_equiv_table
known_steroids <- etbl$drug_name_std[!is.na(etbl$drug_name_std) & !is.na(etbl$equiv_factor)]
df1 <- df1[df1$drug_name_std %in% known_steroids, ]
n_steroid <- nrow(df1)
cat(sprintf("[1] After known-steroid filter: %d  (dropped %d non-steroid records)\n\n",
            n_steroid, n_raw - n_steroid))

# ---------------------------------------------------------------------------
# Step 2 — oral route filter (same logic as baseline.R step 0c)
# ---------------------------------------------------------------------------
rc2 <- if ("route_concept_name" %in% names(df1)) df1$route_concept_name else NULL
rs2 <- if ("route_source_value"  %in% names(df1)) df1$route_source_value  else NULL
ds2 <- if ("drug_source_value"   %in% names(df1)) df1$drug_source_value   else NULL

if (is.null(rc2) && is.null(rs2) && is.null(ds2)) {
  cat("[2] No route column found — oral filter skipped.\n\n")
  df2 <- df1
} else {
  route_class <- SteroidDoseR:::classify_route(rc2, rs2, ds2)
  df2 <- df1[route_class == "oral" | is.na(route_class), ]
  n_oral <- nrow(df2)

  route_tbl <- table(route_class, useNA = "ifany")
  cat(sprintf("[2] After oral-route filter: %d oral records  (dropped %d non-oral)\n",
              n_oral, n_steroid - n_oral))
  cat("    Route class breakdown across steroid records:\n")
  for (nm in names(route_tbl))
    cat(sprintf("      %-12s : %d\n", nm, route_tbl[nm]))
  cat("\n")
}

N <- nrow(df2)   # denominator for all percentages below

# ---------------------------------------------------------------------------
# Step 3 — M1: pre-existing daily dose
# ---------------------------------------------------------------------------
cat("[3] M1 (original) — pre-existing daily dose column\n")
dd_col <- intersect(c("daily_dose", "daily_dose_mg"), names(df2))
if (length(dd_col) == 0L) {
  cat("    No daily_dose / daily_dose_mg column in data.\n")
  cat(sprintf("    M1 available: 0 / %d  (0%%)\n\n", N))
  n_m1 <- 0L
} else {
  dd_col <- dd_col[[1L]]
  dd_vals <- suppressWarnings(as.numeric(df2[[dd_col]]))
  n_m1 <- sum(!is.na(dd_vals) & dd_vals > 0, na.rm = TRUE)
  cat(sprintf("    Column: '%s'  |  non-NA: %d  |  > 0 (usable): %d\n",
              dd_col, sum(!is.na(dd_vals)), n_m1))
  cat(sprintf("    M1 available: %d / %d  (%.1f%%)\n\n", n_m1, N, 100 * n_m1 / N))
}

# Records that need imputation (M1 not available)
dd_vals_full <- if (length(dd_col) == 0L) rep(NA_real_, N) else suppressWarnings(as.numeric(df2[[dd_col]]))
need_imputation <- is.na(dd_vals_full) | dd_vals_full <= 0
n_need <- sum(need_imputation)
cat(sprintf("    => Records needing M2/M3/M4 imputation: %d / %d  (%.1f%%)\n\n",
            n_need, N, 100 * n_need / N))

# Subset to records that actually need imputation
df_need <- df2[need_imputation, ]

# ---------------------------------------------------------------------------
# Step 4 — strength_mg availability (needed by M2, M3, M4)
# ---------------------------------------------------------------------------
cat("[4] strength_mg — tablet/capsule strength (needed by M2, M3, M4)\n")
av      <- if ("amount_value"           %in% names(df_need)) suppressWarnings(as.numeric(df_need$amount_value))          else rep(NA_real_,    nrow(df_need))
av_unit <- if ("amount_unit_concept_id" %in% names(df_need)) df_need$amount_unit_concept_id                              else rep(NA_integer_, nrow(df_need))
av_mg <- dplyr::if_else(
  is.na(av_unit) | as.integer(av_unit) == 0L | as.integer(av_unit) == 8576L,
  av, NA_real_
)
sv <- if ("drug_source_value" %in% names(df_need)) df_need$drug_source_value else rep(NA_character_, nrow(df_need))
str_src <- suppressWarnings(as.numeric(
  stringr::str_extract(
    stringr::str_extract(stringr::str_to_lower(as.character(sv)), "(\\d+(?:\\.\\d+)?)\\s*mg"),
    "\\d+(?:\\.\\d+)?"
  )
))
strength_mg <- dplyr::coalesce(av_mg, str_src)

n_str_av    <- sum(!is.na(av_mg))
n_str_src   <- sum(!is.na(str_src))
n_str_total <- sum(!is.na(strength_mg))

cat(sprintf("    From amount_value (unit-checked):          %d / %d  (%.1f%%)\n",
            n_str_av, n_need, 100 * n_str_av / max(n_need, 1)))
cat(sprintf("    From drug_source_value (mg pattern):       %d / %d  (%.1f%%)\n",
            n_str_src, n_need, 100 * n_str_src / max(n_need, 1)))
cat(sprintf("    strength_mg available (either source):     %d / %d  (%.1f%%)\n\n",
            n_str_total, n_need, 100 * n_str_total / max(n_need, 1)))

if ("amount_unit_concept_id" %in% names(df_need)) {
  unit_tbl <- table(df_need$amount_unit_concept_id, useNA = "ifany")
  cat("    amount_unit_concept_id breakdown (8576=mg, 9655=mcg, 8504=g):\n")
  unit_df <- as.data.frame(unit_tbl, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(unit_df)))
    cat(sprintf("      concept %-8s : %d records\n",
                ifelse(is.na(unit_df[i, 1]), "NA", unit_df[i, 1]),
                unit_df[i, 2]))
  cat("\n")
}

# ---------------------------------------------------------------------------
# Step 5 — M2: SIG parsing viability
# ---------------------------------------------------------------------------
cat("[5] M2 (tablets_freq) viability — SIG text → freq_per_day\n")

has_sig <- "sig" %in% names(df_need)
if (!has_sig) {
  cat("    No 'sig' column in data. M2 via auto-parse is impossible.\n\n")
  n_freq_ok <- 0L
  n_m2_ok   <- 0L
} else {
  sig_vals   <- df_need[["sig"]]
  n_sig_na   <- sum(is.na(sig_vals))
  n_sig_empty <- sum(!is.na(sig_vals) & nchar(trimws(as.character(sig_vals))) == 0L)
  n_sig_present <- nrow(df_need) - n_sig_na - n_sig_empty

  cat(sprintf("    sig column present:    %s\n", has_sig))
  cat(sprintf("    sig NA:                %d / %d  (%.1f%%)\n",
              n_sig_na, n_need, 100 * n_sig_na / max(n_need, 1)))
  cat(sprintf("    sig empty/whitespace:  %d / %d  (%.1f%%)\n",
              n_sig_empty, n_need, 100 * n_sig_empty / max(n_need, 1)))
  cat(sprintf("    sig non-empty:         %d / %d  (%.1f%%)\n",
              n_sig_present, n_need, 100 * n_sig_present / max(n_need, 1)))

  if (n_sig_present == 0L) {
    cat("\n    => sig column is all NULL/empty for records needing imputation.\n")
    cat("       m2_sig_parse = 'auto' makes no difference. M2 is dead.\n\n")
    n_freq_ok <- 0L
    n_m2_ok   <- 0L
  } else {
    # Parse only the non-empty SIG rows (cap at 2000 for speed)
    sig_present_rows <- df_need[!is.na(sig_vals) & nchar(trimws(as.character(sig_vals))) > 0, ]
    sig_cap          <- min(2000L, nrow(sig_present_rows))
    sig_sample       <- sig_present_rows[seq_len(sig_cap), ]

    parsed <- purrr::map_dfr(sig_sample[["sig"]], SteroidDoseR:::parse_sig_one)

    cat(sprintf("\n    parse_sig() on %d non-empty SIG rows (capped at 2000):\n", sig_cap))
    cat("    parsed_status breakdown:\n")
    status_tbl <- table(parsed$parsed_status, useNA = "ifany")
    for (nm in names(status_tbl))
      cat(sprintf("      %-12s : %d\n", nm, status_tbl[nm]))

    n_freq_ok_sample <- sum(!is.na(parsed$freq_per_day))
    cat(sprintf("\n    freq_per_day parsed:  %d / %d  (%.1f%% of non-empty SIG sample)\n",
                n_freq_ok_sample, sig_cap, 100 * n_freq_ok_sample / max(sig_cap, 1)))

    # Scale to full set
    freq_rate  <- n_freq_ok_sample / max(sig_cap, 1)
    n_freq_ok  <- round(freq_rate * n_sig_present)

    # Cross with strength_mg for the same sample rows
    av_s    <- if ("amount_value"           %in% names(sig_sample)) suppressWarnings(as.numeric(sig_sample$amount_value)) else rep(NA_real_,    nrow(sig_sample))
    avu_s   <- if ("amount_unit_concept_id" %in% names(sig_sample)) sig_sample$amount_unit_concept_id                     else rep(NA_integer_, nrow(sig_sample))
    av_mg_s <- dplyr::if_else(is.na(avu_s) | as.integer(avu_s) == 0L | as.integer(avu_s) == 8576L, av_s, NA_real_)
    sv_s    <- if ("drug_source_value"      %in% names(sig_sample)) sig_sample$drug_source_value                          else rep(NA_character_, nrow(sig_sample))
    str_s <- suppressWarnings(as.numeric(
      stringr::str_extract(
        stringr::str_extract(stringr::str_to_lower(as.character(sv_s)), "(\\d+(?:\\.\\d+)?)\\s*mg"),
        "\\d+(?:\\.\\d+)?"
      )
    ))
    str_s <- dplyr::coalesce(av_mg_s, str_s)

    n_m2_sample <- sum(!is.na(parsed$freq_per_day) & !is.na(str_s))
    m2_rate     <- n_m2_sample / max(sig_cap, 1)
    n_m2_ok     <- round(m2_rate * n_sig_present)

    cat(sprintf("    M2 computable (freq + strength both ok): %d / %d sample\n",
                n_m2_sample, sig_cap))
    cat(sprintf("    => Estimated M2 records (extrapolated):  ~%d / %d needing imputation\n\n",
                n_m2_ok, n_need))

    if (n_m2_sample > 0L) {
      cat("    Sample SIGs that yield a parseable freq_per_day:\n")
      freq_rows <- sig_sample[!is.na(parsed$freq_per_day), ]
      freq_parsed <- parsed[!is.na(parsed$freq_per_day), ]
      for (i in seq_len(min(6L, nrow(freq_rows)))) {
        cat(sprintf("      sig='%s'\n           => freq=%.2f, tablets=%.0f, strength_mg=%s\n",
                    freq_rows$sig[[i]],
                    freq_parsed$freq_per_day[[i]],
                    freq_parsed$tablets[[i]],
                    ifelse(is.na(str_s[which(!is.na(parsed$freq_per_day))][i]),
                           "NA (M2 blocked)", as.character(str_s[which(!is.na(parsed$freq_per_day))][i]))))
      }
      cat("\n")
    }
  }
}

# ---------------------------------------------------------------------------
# Step 6 — M3/M4: quantity + date/supply viability
# ---------------------------------------------------------------------------
cat("[6] M3/M4 viability — quantity-based imputation\n")

has_qty  <- "quantity"    %in% names(df_need)
has_sup  <- "days_supply" %in% names(df_need)
has_end  <- "drug_exposure_end_date" %in% names(df_need)
has_start <- "drug_exposure_start_date" %in% names(df_need)

qty  <- if (has_qty)  suppressWarnings(as.numeric(df_need$quantity))   else rep(NA_real_, n_need)
sup  <- if (has_sup)  suppressWarnings(as.numeric(df_need$days_supply)) else rep(NA_real_, n_need)
dur  <- if (has_end && has_start) {
          as.numeric(as.Date(df_need$drug_exposure_end_date) -
                     as.Date(df_need$drug_exposure_start_date)) + 1L
        } else rep(NA_real_, n_need)
dur  <- ifelse(!is.na(dur) & dur > 0, dur, NA_real_)

n_m3_ok <- sum(!is.na(qty) & !is.na(dur) & !is.na(strength_mg))
n_m4_ok <- sum(!is.na(qty) & !is.na(sup) & sup > 0 & !is.na(strength_mg))

cat(sprintf("    quantity non-NA:                %d / %d\n", sum(!is.na(qty)), n_need))
cat(sprintf("    days_supply > 0:                %d / %d\n",
            sum(!is.na(sup) & sup > 0, na.rm = TRUE), n_need))
cat(sprintf("    actual_duration > 0 (end-start):%d / %d\n",
            sum(!is.na(dur)), n_need))
cat(sprintf("    M3 computable (qty+duration+str):%d / %d  (%.1f%%)\n",
            n_m3_ok, n_need, 100 * n_m3_ok / max(n_need, 1)))
cat(sprintf("    M4 computable (qty+supply+str):  %d / %d  (%.1f%%)\n\n",
            n_m4_ok, n_need, 100 * n_m4_ok / max(n_need, 1)))

# ---------------------------------------------------------------------------
# Step 7 — overall imputation coverage
# ---------------------------------------------------------------------------
cat("[7] Overall imputation coverage (records needing M2/M3/M4)\n")

n_any_imputable <- sum(
  (!is.na(strength_mg)) & (
    (!is.na(qty) & !is.na(dur)) |
    (!is.na(qty) & !is.na(sup) & sup > 0)
  )
)
n_m2_imputable  <- n_m2_ok   # estimated from sample above

cat(sprintf("    Can be imputed by M3 or M4:    %d / %d  (%.1f%%)\n",
            n_any_imputable, n_need, 100 * n_any_imputable / max(n_need, 1)))
cat(sprintf("    Estimated M2 (auto only):      ~%d / %d  (%.1f%% additional)\n",
            n_m2_imputable, n_need, 100 * n_m2_imputable / max(n_need, 1)))
cat(sprintf("    Will remain 'missing':         ~%d / %d  (%.1f%%)\n\n",
            n_need - n_any_imputable, n_need,
            100 * (n_need - n_any_imputable) / max(n_need, 1)))

# ---------------------------------------------------------------------------
# Step 8 — run baseline with auto and show final method breakdown
# ---------------------------------------------------------------------------
cat("[8] Run calc_daily_dose_baseline(m2_sig_parse='auto') — method breakdown\n")

baseline_out <- suppressMessages(suppressWarnings(
  calc_daily_dose_baseline(df2, m2_sig_parse = "auto",
                           filter_oral = FALSE, max_daily_dose_mg = 2000)
))

method_tbl <- table(baseline_out$imputation_method, useNA = "ifany")
cat("    imputation_method breakdown (all oral steroid records):\n")
total_out <- nrow(baseline_out)
for (nm in names(method_tbl)) {
  cat(sprintf("      %-20s : %5d  (%5.1f%%)\n",
              nm, method_tbl[nm], 100 * method_tbl[nm] / total_out))
}

n_non_missing <- sum(baseline_out$imputation_method != "missing", na.rm = TRUE)
n_missing     <- sum(baseline_out$imputation_method == "missing", na.rm = TRUE)
cat(sprintf("\n    Successfully imputed:  %d / %d  (%.1f%%)\n",
            n_non_missing, total_out, 100 * n_non_missing / total_out))
cat(sprintf("    Remain 'missing':      %d / %d  (%.1f%%)\n\n",
            n_missing, total_out, 100 * n_missing / total_out))

# ---------------------------------------------------------------------------
# Step 9 — final analysis inclusion
# ---------------------------------------------------------------------------
cat("[9] Final analysis inclusion — dose_from_tablets_freq contribution\n")
n_m2_actual <- sum(baseline_out$imputation_method == "tablets_freq", na.rm = TRUE)
cat(sprintf("    Records credited to M2 (tablets_freq): %d / %d  (%.1f%%)\n",
            n_m2_actual, total_out, 100 * n_m2_actual / total_out))

if (n_m2_actual > 0L) {
  cat("\n    Sample M2-imputed records:\n")
  m2_rows <- baseline_out[baseline_out$imputation_method == "tablets_freq" &
                           !is.na(baseline_out$imputation_method), ]
  m2_show <- head(m2_rows, 6L)
  drug_col <- intersect(c("drug_name_std", "drug_concept_name", "drug_source_value"),
                        names(m2_show))[[1L]]
  for (i in seq_len(nrow(m2_show))) {
    cat(sprintf("      %s  =>  %.1f mg/day\n",
                m2_show[[drug_col]][[i]], m2_show$daily_dose_mg_imputed[[i]]))
  }
}

cat("\n==============================================================\n")
cat("  Funnel summary\n")
cat("==============================================================\n")
cat(sprintf("  Raw records:               %d\n", n_raw))
cat(sprintf("  Known steroids:            %d\n", n_steroid))
cat(sprintf("  Oral route:                %d\n", N))
cat(sprintf("  M1 available (original):   %d  (%.1f%%)\n",
            n_m1, 100 * n_m1 / max(N, 1)))
cat(sprintf("  Need M2/M3/M4:             %d  (%.1f%%)\n",
            n_need, 100 * n_need / max(N, 1)))
cat(sprintf("  Successfully imputed:      %d  (%.1f%% of oral)\n",
            n_non_missing, 100 * n_non_missing / max(N, 1)))
cat(sprintf("    of which M2 (sig-based): %d  (%.1f%% of oral)\n",
            n_m2_actual, 100 * n_m2_actual / max(N, 1)))
cat(sprintf("  Remain missing:            %d  (%.1f%% of oral)\n",
            n_missing, 100 * n_missing / max(N, 1)))
cat("\n")
