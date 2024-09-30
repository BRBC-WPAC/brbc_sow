source("constants.R")
library(tidyhydat)
library(tidyverse)
library(here)
library(janitor)
library(openxlsx)
library(data.table)
library(zoo)
library(readxl)
library(duckdb)


# unlink DUCKDB_FILE if it exists
if (file.exists(DUCKDB_FILE)) {
  file.remove(DUCKDB_FILE)
}
con <- dbConnect(duckdb(), DUCKDB_FILE, read_only = FALSE)

# read CSVs into duckdb
csv_file <- here("output", CONSOLIDATED_DATA_FILE) # nolint: object_usage_linter
dbWriteTable(con, "consolidated_data", read_csv(csv_file), overwrite = TRUE)
csv_file <- here("output", DAILY_FLOW_FILE) # nolint: object_usage_linter
dbWriteTable(con, "daily_flow", read_csv(csv_file), overwrite = TRUE)
csv_file <- here("data", GUIDELINE_FILE) # nolint: object_usage_linter
dbWriteTable(con, "guideline", read_csv(csv_file), overwrite = TRUE)

# TODO: Address unit issue in source data.
# ESCHERICHIA COLI has 2 units: MPN/100mL, No/100 mL
# Possibly these should be different variables entirely.
query <- "
  update consolidated_data
  set unit_code = 'No/100 mL'
  where
    variable_name = 'ESCHERICHIA COLI'
    and unit_code = 'MPN/100mL'
"
dbExecute(con, query)

# TODO: Address unit issue in source data.
# SODIUM ADSORPTION RATIO (CALCD.) has 2 units: mg/L, rel units
# Ditching mg/L because that is wrong.
query <- "
  update consolidated_data
  set unit_code = 'rel units'
  where
    variable_name = 'SODIUM ADSORPTION RATIO (CALCD.)'
    and unit_code = 'mg/L'
"
dbExecute(con, query)
dbDisconnect(con)
