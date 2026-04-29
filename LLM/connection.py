"""
Databricks connection helper — Python equivalent of test_connection.R.

Credentials are loaded from LLM/R.env (or the DATABRICKS_TOKEN / SAFE_DATABRICKS_TOKEN
environment variable if the file is absent).

Required packages:
    pip install databricks-sql-connector python-dotenv
"""

import os
from pathlib import Path

from dotenv import load_dotenv
from databricks import sql


def _load_env() -> None:
    """Load credentials from LLM/R.env if present."""
    env_file = Path(__file__).parent / "R.env"
    if env_file.exists():
        load_dotenv(env_file, override=False)


def get_connection() -> sql.client.Connection:
    """
    Return an open Databricks SQL connection.

    Equivalent to the DatabaseConnector::connect() call in test_connection.R.
    The caller is responsible for closing the connection.
    """
    _load_env()

    host      = os.getenv("DATABRICKS_SERVER_HOSTNAME")
    http_path = os.getenv("DATABRICKS_HTTP_PATH")
    # honour SAFE_DATABRICKS_TOKEN (R convention) then fall back to DATABRICKS_TOKEN
    token     = os.getenv("SAFE_DATABRICKS_TOKEN") or os.getenv("DATABRICKS_TOKEN")

    if not all([host, http_path, token]):
        missing = [k for k, v in {
            "DATABRICKS_SERVER_HOSTNAME": host,
            "DATABRICKS_HTTP_PATH":       http_path,
            "DATABRICKS_TOKEN / SAFE_DATABRICKS_TOKEN": token,
        }.items() if not v]
        raise EnvironmentError(f"Missing credentials: {', '.join(missing)}")

    conn = sql.connect(
        server_hostname=host,
        http_path=http_path,
        access_token=token,
    )
    return conn


def test_connection() -> bool:
    """
    Open a connection, run SELECT 1, print the result, then close.
    Returns True on success.
    """
    print("Connecting to Databricks …")
    conn = get_connection()
    try:
        with conn.cursor() as cursor:
            cursor.execute("SELECT 1 AS test_value")
            row = cursor.fetchone()
        print(f"Connection successful — test query returned: {row}")
        return True
    finally:
        conn.close()
