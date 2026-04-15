# connection.R
# Internal utility for loading .env credential files.
#
# All database connections are created by the user with
# DatabaseConnector::createConnectionDetails() and then wrapped via
# create_omop_connector(connectionDetails, cdm_schema = ...).
# See the package README and ?create_omop_connector for examples.

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
