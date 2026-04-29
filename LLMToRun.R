# =============================================================================
# LLMToRun.R — LLM Pipeline Entry Point (R with reticulate)
# SteroidDoseR / Myositis project
#
# Sets up the "myositis" conda environment, installs Python dependencies,
# and runs the Databricks connection test and sample queries.
#
# Prerequisites:
#   - Anaconda or Miniconda installed (conda on PATH)
#   - LLM/R.env filled in: DATABRICKS_SERVER_HOSTNAME, DATABRICKS_HTTP_PATH,
#     DATABRICKS_TOKEN
#
# Run from the SteroidDoseR project root:
#   source("LLMToRun.R")
# =============================================================================

if (!requireNamespace("reticulate", quietly = TRUE)) {
  install.packages("reticulate")
}
library(reticulate)

ENV_NAME   <- "myositis"
PYTHON_VER <- "3.12"

# Resolve the LLM/ directory relative to this script file
SCRIPT_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(0)$ofile, winslash = "/")),
  error = function(e) {
    # fallback when sourced interactively or from rstudioapi context
    tryCatch(
      dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path, winslash = "/")),
      error = function(e2) normalizePath(getwd(), winslash = "/")
    )
  }
)
LLM_DIR <- file.path(SCRIPT_DIR, "LLM")

if (!dir.exists(LLM_DIR)) {
  stop("LLM/ directory not found at: ", LLM_DIR,
       "\nSet your working directory to the SteroidDoseR project root.")
}

# =============================================================================
# 1. Verify Python / conda
# =============================================================================

conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) "")
if (!nzchar(conda_bin)) {
  stop(
    "conda not found. Install Anaconda or Miniconda, then restart R.\n",
    "Download: https://docs.conda.io/en/latest/miniconda.html"
  )
}
message("conda: ", conda_bin)

# =============================================================================
# 2. Create the "myositis" conda environment if it does not exist
# =============================================================================

# Redirect the conda package cache to a local drive.  In JHU's environment
# C:\Users\<name> is a redirected network folder; conda's file-locking fails
# on SMB shares with EINVAL (errno 22).  A path under C:\Temp is always local.
local_pkgs <- "C:/Temp/conda_pkgs"
dir.create(local_pkgs, showWarnings = FALSE, recursive = TRUE)
Sys.setenv(CONDA_PKGS_DIRS = local_pkgs)

envs <- reticulate::conda_list()

if (!ENV_NAME %in% envs$name) {
  message("Creating conda environment '", ENV_NAME, "' (Python ", PYTHON_VER, ") ...")
  reticulate::conda_create(ENV_NAME, python_version = PYTHON_VER)
  message("Environment '", ENV_NAME, "' created.")
} else {
  message("Conda environment '", ENV_NAME, "' already exists.")
}

# =============================================================================
# 3. Activate the environment
# NOTE: use_condaenv() must be called before Python is initialised
# =============================================================================

reticulate::use_condaenv(ENV_NAME, required = TRUE)
message("Python executable: ", reticulate::py_config()$python)

# =============================================================================
# 4. Install required Python packages (no-op if already installed)
# =============================================================================

required_pkgs <- c("databricks-sql-connector", "python-dotenv", "pandas")
message("Ensuring packages installed: ", paste(required_pkgs, collapse = ", "))
reticulate::conda_install(ENV_NAME, packages = required_pkgs, pip = TRUE)

# =============================================================================
# 5. Test the Databricks connection via LLM/connection.py
# =============================================================================

# Add LLM/ to Python's sys.path so `import connection` resolves
reticulate::py_run_string(sprintf(
  "import sys\nif r'%s' not in sys.path:\n    sys.path.insert(0, r'%s')",
  LLM_DIR, LLM_DIR
))

connection <- reticulate::import_from_path("connection", path = LLM_DIR)

cat("\n--- Connection test ---\n")
ok <- connection$test_connection()
if (!isTRUE(ok)) stop("Connection test failed. Check credentials in LLM/R.env.")

# =============================================================================
# 6. Sample query
# =============================================================================

cat("\n--- Sample query ---\n")
conn <- connection$get_connection()
on.exit(conn$close(), add = TRUE)

pd  <- reticulate::import("pandas")
cur <- conn$cursor()
cur$execute("SELECT current_catalog(), current_database(), current_timestamp()")
cols <- sapply(cur$description, `[[`, 1)
rows <- cur$fetchall()
cur$close()

df <- reticulate::py_to_r(pd$DataFrame(rows, columns = cols))
print(df)

# =============================================================================
# 7. LLM analysis — add inference calls below
# =============================================================================
# conn is still open; close it when done with on.exit() above.
#
# Example — list tables in deid.derived:
#   cur <- conn$cursor()
#   cur$execute("SHOW TABLES IN deid.derived")
#   rows <- cur$fetchall()
#   cur$close()
#   print(data.frame(table = sapply(rows, `[[`, 1)))
