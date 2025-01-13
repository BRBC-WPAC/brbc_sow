# Shared constants used in other scripts ####
## File names ####
# nolint start: object_name_linter.
CONSOLIDATED_DATA_FILE <- "consolidated_data-maxDL-flow.csv"
DAILY_FLOW_FILE <- "daily flows.csv"
DUCKDB_FILE <- "db.duckdb"
GUIDELINE_FILE <- "Guidelines.csv"

## Data processing ####
CUTOFF_YEAR <- 2022
PERC_BINS <- c(0, 0.1, 0.25, 0.75, 0.90, 1)

# The names of the variables that we do not want to show on log10 scale
# When rendering these, they will use non-transformed data and the plit
# will include a caption explaining why they're not on log10 scale.
NO_LOG_SCALE <- c("PH")

## Plot appearance ####
NA_COLOUR <- "grey75"
FLOW_COLOUR <- "blue"
PERC_COLOURS <- c(
  "#00A6C8",
  "#ACE2E5",
  "#95C087",
  "#D1B361",
  "#BA3C3C",
  NA
)
PERC_LABELS <- c(
  "<10th",
  "10th - 24th",
  "25th - 75th",
  "76th - 90th",
  ">90th",
  "Not ranked"
)
GUIDELINE_BOUND_COLOURS <- c("red", "red", "blue")
GUIDELINE_BOUND_LABELS <- c("Upper acute", "Upper chronic", "Lower chronic")
GUIDELINE_EXPOSURE_LINETYPES <- c("solid", "dotted", "dotted")

SEASON_LEVELS <- c("high flow", "open water", "under ice")

WATER_QUALITY_NOT_AVAILABLE <- "Unavailable"
STANDARD_FOOTNOTE <- "This figure was created for the BRBC State of the Watershed Report, Water Quality Chapter. For more information visit: brbc.ab.ca/state-of-the-watershed"
CONCENTRATION_FOOTNOTES <- c(
  STANDARD_FOOTNOTE,
  "The guideline for ammonia could not be calculated with the available data.",
  "The guideline for sulphate assumes total hardness between 76 and 180 mg/L.",
  "Guidelines for total phosphorus, total nitrogen, total suspended solids and turbidity are narratives and cannot be shown on a graph."
)
INDIVIDUAL_CONCENTRATION_FOOTNOTES <- c(
  "The guideline for ammonia could not be calculated with the available data.",
  "The guideline for sulphate assumes total hardness between 76 and 180 mg/L.",
  "Guideline for total phosphorus is narrative and cannot be shown on a graph.",
  "Guideline for total nitrogen is narrative and cannot be shown on a graph.",
  "Guideline for total suspended solids is narrative and cannot be shown on a graph.",
  "Guideline for turbidity is narrative and cannot be shown on a graph."
)
names(INDIVIDUAL_CONCENTRATION_FOOTNOTES) <- c(
  "AMMONIA TOTAL",
  "SULPHATE DISSOLVED",
  "PHOSPHORUS TOTAL (P)",
  "NITROGEN TOTAL (CALCD.)",
  "TOTAL SUSPENDED SOLIDS (TSS)",
  "TURBIDITY"
)



## Image generation ####
# Some defaults for image generation. At 12x8@300dpi, each image is usually
# somewhere around 700KB, meaning that the generated images should take up
# around a gigabyte in total.
IMAGE_WIDTH <- 12
IMAGE_HEIGHT <- 8
IMAGE_DPI <- 300
IMAGE_DEVICE <- "JPEG"
