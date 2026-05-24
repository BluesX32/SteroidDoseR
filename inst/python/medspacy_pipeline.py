# medspacy_pipeline.py
# Steroid dose extractor for clinical note text.
# Called from R via reticulate in nlp_notes.R.
#
# Dependencies:
#   pip install medspacy scispacy
#   python -m spacy download en_core_sci_sm
#
# The module-level _pipeline cache means load_pipeline() is only called once
# per Python session, regardless of how many R calls are made.

import re

_pipeline = None  # module-level pipeline cache

# ---------------------------------------------------------------------------
# Steroid vocabulary
# ---------------------------------------------------------------------------

STEROID_NAMES = {
    "prednisone", "prednisolone", "methylprednisolone", "medrol",
    "dexamethasone", "decadron", "hydrocortisone", "cortisol",
    "triamcinolone", "kenalog", "budesonide", "betamethasone",
    "fludrocortisone", "deflazacort", "cortisone",
}

# Frequency keyword → doses per day
FREQ_MAP = {
    "qid": 4, "tid": 3, "bid": 2, "qd": 1,
    "four times daily": 4, "four times a day": 4,
    "three times daily": 3, "three times a day": 3,
    "twice daily": 2, "twice a day": 2, "twice per day": 2,
    "once daily": 1, "once a day": 1, "daily": 1,
    "every day": 1, "every morning": 1, "every evening": 1, "nightly": 1,
    "every other day": 0.5, "alternate day": 0.5, "qod": 0.5,
    "weekly": 1 / 7, "once a week": 1 / 7, "once weekly": 1 / 7,
    "monthly": 1 / 30, "once a month": 1 / 30,
}

# Context triggers
NEGATION_TRIGGERS = {
    "not", "no", "denies", "denied", "never", "without", "negative for",
    "not on", "not taking", "not prescribed", "not started",
}
UNCERTAINTY_TRIGGERS = {
    "may", "might", "possibly", "consider", "perhaps", "unclear if",
    "likely on", "suspected", "possible",
}
HISTORICAL_TRIGGERS = {
    "was on", "was taking", "previously on", "formerly on", "used to",
    "history of", "h/o", "prior", "past", "stopped", "discontinued",
    "weaned off", "tapered off",
}
TAPER_TRIGGERS = {
    "taper", "tapering", "tapered", "wean", "weaning", "weaned",
    "reduce", "reducing", "reduced", "decrease", "decreasing", "decreased",
    "drop", "step down",
}

# ---------------------------------------------------------------------------
# Pipeline loader
# ---------------------------------------------------------------------------

def load_pipeline():
    """Load and cache the medspaCy NLP pipeline (called once per session)."""
    global _pipeline
    if _pipeline is not None:
        return _pipeline

    try:
        import medspacy
        try:
            nlp = medspacy.load("en_core_sci_sm")
        except (OSError, IOError):
            # scispaCy model not installed — fall back to medspaCy default
            nlp = medspacy.load()

        # Context detection: negation, uncertainty, historical
        if "medspacy_context" not in nlp.pipe_names:
            nlp.add_pipe("medspacy_context")

        # Section detection: assessment, plan, history, etc.
        if "medspacy_sectionizer" not in nlp.pipe_names:
            try:
                nlp.add_pipe("medspacy_sectionizer")
            except Exception:
                pass  # sectionizer is optional; proceed without it

        _pipeline = nlp
        return _pipeline

    except ImportError:
        raise ImportError(
            "medspaCy is not installed. Install it with:\n"
            "  pip install medspacy scispacy\n"
            "  python -m spacy download en_core_sci_sm"
        )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _is_steroid(text):
    """Return True if text contains a known steroid name."""
    t = text.lower()
    return any(s in t for s in STEROID_NAMES)


def _extract_dose_mg(span_text):
    """
    Extract a numeric mg value from a span of text.
    Handles: '40 mg', '40mg', '40 milligrams', '20-40 mg' (returns first).
    Returns float or None.
    """
    m = re.search(r"(\d+(?:\.\d+)?)\s*(?:mg|milligrams?)", span_text, re.IGNORECASE)
    if m:
        return float(m.group(1))
    return None


def _extract_freq(span_text):
    """
    Extract doses-per-day from a span of text using FREQ_MAP and every-N-hours.
    Returns float or None.
    """
    t = span_text.lower()

    # every N hours → 24/N
    m = re.search(r"every\s*(\d+(?:\.\d+)?)\s*hours?", t)
    if m:
        n = float(m.group(1))
        if n > 0:
            return round(24 / n, 4)

    # N times a week
    m = re.search(r"(\d+)\s*times?\s*(?:a\s*|per\s*)?week", t)
    if m:
        return round(int(m.group(1)) / 7, 4)

    # every N days
    m = re.search(r"every\s*(\d+)\s*days?", t)
    if m:
        n = int(m.group(1))
        if n > 0:
            return round(1 / n, 4)

    # keyword lookup (longest match first to avoid "daily" swallowing "twice daily")
    for phrase in sorted(FREQ_MAP, key=len, reverse=True):
        if phrase in t:
            return FREQ_MAP[phrase]

    return None


def _extract_duration_days(span_text):
    """
    Extract duration in days from text like 'for 2 weeks', 'x 10 days'.
    Returns float or None.
    """
    t = span_text.lower()
    m = re.search(
        r"(?:for|x)\s*(\d+(?:\.\d+)?)\s*(day|days|wk|wks|week|weeks|mo|mos|month|months)",
        t,
    )
    if not m:
        return None
    n, unit = float(m.group(1)), m.group(2)
    if unit.startswith("day"):
        return n
    if unit.startswith("wk") or unit.startswith("week"):
        return n * 7
    if unit.startswith("mo"):
        return n * 30
    return None


def _detect_context(window_text):
    """
    Scan the window of text around the drug entity for context signals.
    Returns dict: negated (bool), uncertain (bool), historical (bool).
    """
    t = window_text.lower()
    negated   = any(trigger in t for trigger in NEGATION_TRIGGERS)
    uncertain = any(trigger in t for trigger in UNCERTAINTY_TRIGGERS)
    historical = any(trigger in t for trigger in HISTORICAL_TRIGGERS)
    return {"negated": negated, "uncertain": uncertain, "historical": historical}


def _detect_taper(window_text):
    t = window_text.lower()
    return any(trigger in t for trigger in TAPER_TRIGGERS)


def _detect_section(doc, token_idx):
    """
    Return the section title for the token at token_idx, or None.
    Works whether or not medspacy_sectionizer is in the pipeline.
    """
    token = doc[token_idx]
    # medspaCy >= 1.x stores section on the span via the sectionizer extension
    if token.has_extension("section_category"):
        return token._.section_category
    # Fallback: scan backwards for common section headers
    sent_text = token.sent.text.lower() if token.is_sent_start is not None else ""
    for header in ("assessment", "plan", "impression", "history", "medications",
                   "current medications", "past medical"):
        if header in sent_text:
            return header
    return None


# ---------------------------------------------------------------------------
# Main extraction function (called from R)
# ---------------------------------------------------------------------------

def extract_steroid_doses(text):
    """
    Extract steroid dose information from a clinical note string.

    Parameters
    ----------
    text : str
        Raw clinical note text.

    Returns
    -------
    list of dict, one entry per steroid entity found.  Each dict contains:
        drug          str   — normalised steroid name (e.g. 'prednisone')
        dose_mg       float — mg per dose/day extracted near the entity (or None)
        freq_per_day  float — doses per day (or None)
        duration_days float — duration in days (or None)
        taper_flag    bool  — taper/wean language detected in context
        negated       bool  — drug is negated ('patient is NOT on prednisone')
        uncertain     bool  — drug is uncertain ('may be taking prednisone')
        historical    bool  — drug is past/historical context
        section       str   — detected section name (or None)
        raw_span      str   — raw matched text span
    """
    if not text or not isinstance(text, str) or not text.strip():
        return []

    nlp = load_pipeline()
    doc = nlp(text)

    results = []

    # Collect token-level steroid matches (NER may catch DRUG/CHEMICAL;
    # also run a keyword scan as safety net for models that miss steroids)
    matched_spans = []

    # 1. NER entities labelled DRUG, CHEMICAL, MEDICATION
    for ent in doc.ents:
        if ent.label_ in ("DRUG", "CHEMICAL", "MEDICATION", "MED"):
            if _is_steroid(ent.text):
                matched_spans.append(ent)

    # 2. Regex keyword fallback (catches steroids the NER model may miss)
    steroid_pattern = re.compile(
        r"\b(" + "|".join(re.escape(s) for s in sorted(STEROID_NAMES, key=len, reverse=True)) + r")\b",
        re.IGNORECASE,
    )
    ner_char_spans = {(s.start_char, s.end_char) for s in matched_spans}
    for m in steroid_pattern.finditer(text):
        # Skip if already captured by NER
        if (m.start(), m.end()) in ner_char_spans:
            continue
        span = doc.char_span(m.start(), m.end(), alignment_mode="expand")
        if span is not None:
            matched_spans.append(span)

    if not matched_spans:
        return []

    # De-duplicate spans by start char
    seen = set()
    unique_spans = []
    for sp in matched_spans:
        if sp.start_char not in seen:
            seen.add(sp.start_char)
            unique_spans.append(sp)

    for span in unique_spans:
        # Context window: up to 60 chars before and 120 chars after the entity
        win_start = max(0, span.start_char - 60)
        win_end   = min(len(text), span.end_char + 120)
        window    = text[win_start:win_end]

        ctx          = _detect_context(window)
        taper_flag   = _detect_taper(window)
        dose_mg      = _extract_dose_mg(window)
        freq_per_day = _extract_freq(window)
        dur_days     = _extract_duration_days(window)
        section      = _detect_section(doc, span.start)

        # Attempt medspaCy context extension (negation/uncertainty) if available
        try:
            if hasattr(span, "_") and span._.is_negated:
                ctx["negated"] = True
            if hasattr(span, "_") and span._.is_uncertain:
                ctx["uncertain"] = True
            if hasattr(span, "_") and span._.is_historical:
                ctx["historical"] = True
        except AttributeError:
            pass

        results.append({
            "drug":          span.text.lower(),
            "dose_mg":       dose_mg,
            "freq_per_day":  freq_per_day,
            "duration_days": dur_days,
            "taper_flag":    taper_flag,
            "negated":       ctx["negated"],
            "uncertain":     ctx["uncertain"],
            "historical":    ctx["historical"],
            "section":       section,
            "raw_span":      span.text,
        })

    return results
