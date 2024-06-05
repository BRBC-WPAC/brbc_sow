# Shared constants used in other scripts ####
## File names ####
# nolint start: object_name_linter.
CONSOLIDATED_DATA_FILE <- "consolidated_data-maxDL-flow.csv"
DAILY_FLOW_FILE <- "daily flows.csv"
DUCKDB_FILE <- "db.duckdb"

## Data processing constants ####
CUTOFF_YEAR <- 2022
PERC_BINS <- c(0, 0.1, 0.25, 0.75, 0.90, 1)

# The names of the variables that we do not want to show log-transformed.
# When rendering these, they will use non-transformed data and the plit
# will include a caption explaining why they're not log-transformed.
NO_LOG_TRANSFORM <- c("PH", "TURBIDITY", "ESCHERICHIA COLI")

## Image generation constants ####
# Some defaults for image generation. At 12x8@300dpi, each image is usually
# somewhere around 700KB, meaning that the generated images should take up
# around a gigabyte in total.
IMAGE_WIDTH <- 12
IMAGE_HEIGHT <- 8
IMAGE_DPI <- 300
IMAGE_DEVICE <- "jpeg"
# nolint end: object_name_linter.
