# extras/DebugM2SigParse.R
#
# Diagnostic script: why does m2_sig_parse = "auto" produce the same output
# as m2_sig_parse = "warn"?
#
# Run AFTER CodeToRun.R has created `drug_df` (i.e. source this in the same
# R session, or paste the blocks interactively).
#
# Each section prints a numbered checkpoint and a conclusion so you can see
# exactly where the auto/warn divergence fails to materialise.

stopifnot(exists("drug_df"), is.data.frame(drug_df))

cat("\n======================================================\n")
cat("  M2 / m2_sig_parse debug analysis\n")
cat("======================================================\n\n")

methods_default <- c("original", "tablets_freq", "actual_duration", "supply_based")

# ---------------------------------------------------------------------------
# [1] Is "tablets_freq" in the method list?
# ---------------------------------------------------------------------------
cat("--- [1] tablets_freq in methods? ---\n")
cat(sprintf(
  "  'tablets_freq' %%in%% default methods: %s\n",
  "tablets_freq" %in% methods_default
))
cat("  => If FALSE, the entire M2 guard is bypassed and auto/warn are identical.\n\n")

# ---------------------------------------------------------------------------
# [2] Tablets / freq_per_day column inventory
# ---------------------------------------------------------------------------
cat("--- [2] tablets / freq_per_day column inventory ---\n")
has_tab_col  <- "tablets"      %in% names(drug_df)
has_freq_col <- "freq_per_day" %in% names(drug_df)
has_tab_vals  <- has_tab_col  && any(!is.na(drug_df[["tablets"]]))
has_freq_vals <- has_freq_col && any(!is.na(drug_df[["freq_per_day"]]))

cat(sprintf("  'tablets'      column present: %-5s  | any non-NA values: %s\n",
            has_tab_col, has_tab_vals))
cat(sprintf("  'freq_per_day' column present: %-5s  | any non-NA values: %s\n",
            has_freq_col, has_freq_vals))

guard_outer <- "tablets_freq" %in% methods_default && !has_tab_vals && !has_freq_vals
cat(sprintf("\n  GUARD outer condition (!has_tab_vals && !has_freq_vals): %s\n", guard_outer))
if (!guard_outer) {
  cat("  => GUARD DID NOT FIRE. Both 'auto' and 'warn' skip M2 the same way.\n")
  cat("     Reason: at least one of tablets/freq_per_day already has usable values.\n")
  if (has_tab_vals)  cat("     tablets:      ", sum(!is.na(drug_df$tablets)), "non-NA values found.\n")
  if (has_freq_vals) cat("     freq_per_day: ", sum(!is.na(drug_df$freq_per_day)), "non-NA values found.\n")
} else {
  cat("  => GUARD outer condition met; checking sig_present next.\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# [3] sig column analysis  (only relevant when guard outer fired)
# ---------------------------------------------------------------------------
cat("--- [3] sig column analysis ---\n")

has_sig_col <- "sig" %in% names(drug_df)
cat(sprintf("  'sig' column present in drug_df: %s\n", has_sig_col))

if (has_sig_col) {
  sig_vals     <- drug_df[["sig"]]
  n_total      <- length(sig_vals)
  n_na         <- sum(is.na(sig_vals))
  n_empty      <- sum(!is.na(sig_vals) & nchar(trimws(as.character(sig_vals))) == 0L)
  n_nonempty   <- n_total - n_na - n_empty
  sig_present  <- n_nonempty > 0L

  cat(sprintf("  Total rows:          %d\n", n_total))
  cat(sprintf("  NA values:           %d  (%.1f%%)\n", n_na, 100 * n_na / n_total))
  cat(sprintf("  empty / whitespace:  %d  (%.1f%%)\n", n_empty, 100 * n_empty / n_total))
  cat(sprintf("  non-empty:           %d  (%.1f%%)\n", n_nonempty, 100 * n_nonempty / n_total))
  cat(sprintf("\n  sig_present (required for auto/warn divergence): %s\n", sig_present))

  if (!sig_present) {
    cat("  => sig_present = FALSE. The inner 'if (sig_present)' block is NEVER entered.\n")
    cat("     Both 'auto' and 'warn' produce identical results because there is\n")
    cat("     nothing to parse.  Switching m2_sig_parse has no effect.\n")
    cat("\n  FIX OPTIONS:\n")
    cat("     A) If your CDM stores SIG text in drug_source_value:\n")
    cat("          calc_daily_dose_baseline(drug_df, sig_source = 'drug_source_value',\n")
    cat("                                   m2_sig_parse = 'auto')\n")
    cat("     B) Check whether the sig column is populated at a different stage:\n")
    cat("          table(is.na(drug_df$sig))   # should show some FALSEs\n")
    cat("     C) Use the NLP method instead, which handles sig-less records via\n")
    cat("          the amount_value strength fallback.\n")
  } else {
    cat("  => sig_present = TRUE. Showing sample non-empty SIG values:\n")
    sample_sigs <- head(sig_vals[!is.na(sig_vals) & nchar(trimws(sig_vals)) > 0], 10)
    for (s in sample_sigs) cat(sprintf("     '%s'\n", s))
    cat("\n  => auto/warn SHOULD diverge. See [4] for parse_sig output.\n")
  }
} else {
  cat("  'sig' column is absent from drug_df entirely.\n")
  cat("  sig_present = FALSE -> inner guard block never entered.\n")
  cat("  => auto and warn are identical because there is nothing to parse.\n")
  cat("\n  FIX: use sig_source = 'drug_source_value' if SIG text lives there.\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# [4] parse_sig dry-run  (only when sig IS present)
# ---------------------------------------------------------------------------
if (has_sig_col && exists("sig_present") && sig_present) {
  cat("--- [4] parse_sig() dry-run on non-empty SIG rows ---\n")

  library(SteroidDoseR)

  sig_rows <- drug_df[!is.na(drug_df$sig) & nchar(trimws(drug_df$sig)) > 0, ]
  sig_rows <- sig_rows[seq_len(min(200L, nrow(sig_rows))), ]  # cap at 200

  parsed_sample <- purrr::map_dfr(sig_rows$sig, SteroidDoseR:::parse_sig_one)

  cat(sprintf("  Parsed %d non-empty SIG rows.\n", nrow(parsed_sample)))
  cat("\n  parsed_status breakdown:\n")
  print(table(parsed_sample$parsed_status, useNA = "ifany"))

  n_have_freq   <- sum(!is.na(parsed_sample$freq_per_day))
  n_have_tabs   <- sum(!is.na(parsed_sample$tablets))
  n_have_daily  <- sum(!is.na(parsed_sample$daily_dose_mg))

  cat(sprintf("\n  freq_per_day non-NA:   %d / %d\n", n_have_freq,  nrow(parsed_sample)))
  cat(sprintf("  tablets non-NA:        %d / %d\n",   n_have_tabs,  nrow(parsed_sample)))
  cat(sprintf("  daily_dose_mg non-NA:  %d / %d\n",   n_have_daily, nrow(parsed_sample)))

  if (n_have_freq == 0L) {
    cat("\n  => freq_per_day is all-NA even after parsing.\n")
    cat("     M2 (tablets * freq * strength) will be NA for every row.\n")
    cat("     auto and warn produce the same M2 column -> same final dose.\n")
    cat("\n  Sample SIGs that failed to parse a frequency:\n")
    no_freq <- parsed_sample[is.na(parsed_sample$freq_per_day), ]
    for (s in head(no_freq$sig_raw, 10)) cat(sprintf("     '%s'\n", s))
  } else {
    cat(sprintf("\n  freq_per_day is parseable for %d rows.\n", n_have_freq))
    cat("  Showing sample SIGs that DID yield a frequency:\n")
    with_freq <- parsed_sample[!is.na(parsed_sample$freq_per_day), ]
    sigs_with_freq <- sig_rows$sig[!is.na(parsed_sample$freq_per_day)]
    for (i in seq_len(min(8L, length(sigs_with_freq)))) {
      cat(sprintf("     sig='%s'  =>  freq=%.2f  tablets=%.0f\n",
                  sigs_with_freq[i], with_freq$freq_per_day[i], with_freq$tablets[i]))
    }
  }
  cat("\n")
}

# ---------------------------------------------------------------------------
# [4b] strength_mg cross-check — the third leg of M2
# ---------------------------------------------------------------------------
# M2 = tablets * freq_per_day * strength_mg.  Even when freq_per_day is
# parsed, a missing strength_mg silently kills M2.  strength_mg comes from
# amount_value (unit-checked) or an mg pattern in drug_source_value — NOT
# from the SIG text.  This section checks how many of the freq-parseable rows
# actually have a usable strength_mg.
cat("--- [4b] strength_mg availability for freq-parseable rows ---\n")

if (has_sig_col && exists("sig_present") && sig_present) {
  # Rebuild the same 200-row sample from [4]
  sig_rows_4b <- drug_df[!is.na(drug_df$sig) & nchar(trimws(drug_df$sig)) > 0, ]
  sig_rows_4b <- sig_rows_4b[seq_len(min(200L, nrow(sig_rows_4b))), ]

  parsed_4b <- purrr::map_dfr(sig_rows_4b$sig, SteroidDoseR:::parse_sig_one)
  freq_mask  <- !is.na(parsed_4b$freq_per_day)

  # Replicate the strength_mg logic from baseline.R
  av      <- if ("amount_value" %in% names(sig_rows_4b))
               SteroidDoseR:::safe_as_numeric(sig_rows_4b$amount_value)
             else rep(NA_real_, nrow(sig_rows_4b))
  av_unit <- if ("amount_unit_concept_id" %in% names(sig_rows_4b))
               sig_rows_4b$amount_unit_concept_id
             else rep(NA_integer_, nrow(sig_rows_4b))
  av_mg   <- dplyr::if_else(
    is.na(av_unit) | as.integer(av_unit) == 0L | as.integer(av_unit) == 8576L,
    av, NA_real_
  )
  sv      <- if ("drug_source_value" %in% names(sig_rows_4b))
               sig_rows_4b$drug_source_value
             else rep(NA_character_, nrow(sig_rows_4b))
  str_from_source <- stringr::str_extract(
    stringr::str_to_lower(as.character(sv)),
    "(\\d+(?:\\.\\d+)?)\\s*(?:mg|MG)"
  ) |> stringr::str_extract("\\d+(?:\\.\\d+)?") |> as.numeric()
  strength_mg <- dplyr::coalesce(av_mg, str_from_source)

  n_freq_rows        <- sum(freq_mask)
  n_strength_ok      <- sum(!is.na(strength_mg[freq_mask]))
  n_m2_computable    <- sum(!is.na(parsed_4b$freq_per_day[freq_mask]) &
                             !is.na(strength_mg[freq_mask]))

  cat(sprintf("  Rows with parseable freq_per_day:          %d\n", n_freq_rows))
  cat(sprintf("  Of those, strength_mg non-NA:              %d\n", n_strength_ok))
  cat(sprintf("  => M2 computable (freq AND strength both ok): %d\n\n", n_m2_computable))

  # amount_value column details
  cat("  amount_value column:\n")
  if ("amount_value" %in% names(sig_rows_4b)) {
    cat(sprintf("    non-NA:              %d / %d\n", sum(!is.na(av)), nrow(sig_rows_4b)))
    cat(sprintf("    accepted as mg:      %d / %d\n", sum(!is.na(av_mg)), nrow(sig_rows_4b)))
    if ("amount_unit_concept_id" %in% names(sig_rows_4b)) {
      unit_tbl <- table(sig_rows_4b$amount_unit_concept_id, useNA = "ifany")
      cat("    unit concept_id breakdown:  ")
      print(unit_tbl)
    } else {
      cat("    amount_unit_concept_id column absent — unit assumed unknown (accepted).\n")
    }
  } else {
    cat("    column ABSENT from drug_df.\n")
    cat("    => No tablet strength from OMOP drug_strength. Falling back to drug_source_value.\n")
  }

  cat("\n  drug_source_value mg-extraction:\n")
  cat(sprintf("    non-NA mg extracted: %d / %d\n",
              sum(!is.na(str_from_source)), nrow(sig_rows_4b)))
  if (sum(!is.na(str_from_source)) > 0) {
    cat("    sample drug_source_values with mg:\n")
    ex <- head(sv[!is.na(str_from_source)], 5)
    for (e in ex) cat(sprintf("      '%s'\n", e))
  } else {
    cat("    => drug_source_value has no '\\d+ mg' patterns either.\n")
  }

  if (n_m2_computable == 0L) {
    cat("\n  CONCLUSION: M2 cannot fire on any row because strength_mg is NA\n")
    cat("  for every row where freq_per_day was parsed.\n")
    cat("  auto and warn will produce identical output.\n")
    cat("\n  WHY strength_mg is NA — most common causes:\n")
    cat("    1. amount_unit_concept_id is a non-mg concept (mcg=9655, g=8504).\n")
    cat("       baseline.R rejects these to avoid unit mismatches.\n")
    cat("       Check: table(drug_df$amount_unit_concept_id)\n")
    cat("    2. drug_strength JOIN returned no rows (drug_concept_id not in vocab).\n")
    cat("       Check: sum(is.na(drug_df$amount_value))\n")
    cat("    3. drug_source_value has no '\\d+ mg' pattern (e.g. 'PREDNISONE TAB').\n")
    cat("       Check: head(drug_df$drug_source_value[is.na(str_from_source)], 10)\n")
  } else {
    cat(sprintf("\n  CONCLUSION: %d rows should produce a non-NA M2.\n", n_m2_computable))
    cat("  If results are still identical, the installed package is stale.\n")
    cat("  Re-install:\n")
    cat("    devtools::install_local(getwd())\n")
    cat("    detach('package:SteroidDoseR', unload = TRUE)\n")
    cat("    library(SteroidDoseR)\n")
    cat("  Then re-run the comparison.\n")
  }
} else {
  cat("  [4b] skipped — no non-empty SIG rows (sig_present = FALSE).\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# [5] daily_dose / daily_dose_mg column check
# ---------------------------------------------------------------------------
cat("--- [5] daily_dose / daily_dose_mg column check ---\n")
dd_cols_present <- intersect(c("daily_dose", "daily_dose_mg"), names(drug_df))
cat(sprintf("  Columns found: %s\n",
            if (length(dd_cols_present) == 0) "(none)" else paste(dd_cols_present, collapse = ", ")))
if (length(dd_cols_present) > 0) {
  for (col in dd_cols_present) {
    vals <- drug_df[[col]]
    cat(sprintf("  %s: %d non-NA, %d > 0\n", col,
                sum(!is.na(vals)), sum(!is.na(vals) & vals > 0, na.rm = TRUE)))
  }
  cat("  => M1 ('original') may fire for records where the above column is > 0.\n")
} else {
  cat("  => No pre-existing daily dose column. M1 will always be NA.\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# [6] Quick side-by-side comparison on a 50-row sample
# ---------------------------------------------------------------------------
cat("--- [6] Side-by-side comparison: auto vs warn (first 50 oral steroid rows) ---\n")

library(SteroidDoseR)

# Oral-filter to match what baseline does internally
rc <- if ("route_concept_name" %in% names(drug_df)) drug_df$route_concept_name else NULL
rs <- if ("route_source_value"  %in% names(drug_df)) drug_df$route_source_value  else NULL
ds <- if ("drug_source_value"   %in% names(drug_df)) drug_df$drug_source_value   else NULL

if (!is.null(rc) || !is.null(rs) || !is.null(ds)) {
  route_class <- SteroidDoseR:::classify_route(rc, rs, ds)
  oral_rows   <- drug_df[route_class == "oral" | is.na(route_class), ]
} else {
  oral_rows <- drug_df
}

sample50 <- head(oral_rows, 50L)

out_auto <- suppressMessages(suppressWarnings(
  calc_daily_dose_baseline(sample50, m2_sig_parse = "auto",
                           filter_oral = FALSE, max_daily_dose_mg = NULL)
))
out_warn <- suppressWarnings(
  calc_daily_dose_baseline(sample50, m2_sig_parse = "warn",
                           filter_oral = FALSE, max_daily_dose_mg = NULL)
)

are_identical_dose   <- identical(out_auto$daily_dose_mg_imputed, out_warn$daily_dose_mg_imputed)
are_identical_method <- identical(out_auto$imputation_method,     out_warn$imputation_method)
are_identical_m2     <- identical(out_auto$dose_from_tablets_freq, out_warn$dose_from_tablets_freq)

cat(sprintf("  daily_dose_mg_imputed identical: %s\n", are_identical_dose))
cat(sprintf("  imputation_method     identical: %s\n", are_identical_method))
cat(sprintf("  dose_from_tablets_freq identical: %s\n", are_identical_m2))

if (are_identical_dose && are_identical_method) {
  cat("\n  => Columns are 100%% identical on this sample.\n")
  cat("     Root cause is one of [1]-[4] above.\n")
} else {
  cat("\n  => Some rows DO differ. Showing first 10 differences:\n")
  diff_rows <- which(
    out_auto$daily_dose_mg_imputed != out_warn$daily_dose_mg_imputed |
    out_auto$imputation_method     != out_warn$imputation_method
  )
  for (i in head(diff_rows, 10L)) {
    cat(sprintf(
      "  row %d: auto dose=%.1f (%s) | warn dose=%.1f (%s)\n",
      i,
      out_auto$daily_dose_mg_imputed[i], out_auto$imputation_method[i],
      out_warn$daily_dose_mg_imputed[i], out_warn$imputation_method[i]
    ))
  }
}
cat("\n")

cat("======================================================\n")
cat("  Debug complete. See checkpoints [1]-[6] above.\n")
cat("======================================================\n\n")
