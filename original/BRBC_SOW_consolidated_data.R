# Author: Eric Camm - City of Calgary
# Date: 2024-02-23
# Script to consolidate WQ data for BRBC SOW

library(tidyverse)
library(here)
library(janitor)

# vector of SOW parameters
wq_parameters <- c(
  "TOTAL DISSOLVED SOLIDS (CALCD.)",
  "PH (LAB)",
  "SPECIFIC CONDUCTANCE (FIELD)",
  "TURBIDITY",
  "NITROGEN, NITRATE",
  "NITROGEN TOTAL (CALCD.)",
  "AMMONIA TOTAL",
  "PHOSPHORUS TOTAL (P)",
  "PHOSPHORUS TOTAL DISSOLVED",
  "CHLORIDE DISSOLVED",
  "SULPHATE DISSOLVED",
  "SODIUM ADSORPTION RATIO (CALCD.)",
  "CARBON TOTAL ORGANIC (TOC)",
  "ESCHERICHIA COLI",
  "OXYGEN DISSOLVED (FIELD METER)",
  "TEMPERATURE WATER"
)

#### CITY OF CALGARY ####
coc_df <- read_csv(here("data", "Watershed_Surface_Water_Quality_Data_CityofCalgary.csv")) %>%
  clean_names()

# convert to standard date, strip time for flow join on later
coc_df$sample_date <- mdy_hm(coc_df$sample_date) %>%
  as_date()

coc_df <- coc_df %>%
  add_column(agency = "City of Calgary")

# create range column. All City of Calgary sites are in Lower Foothills except Big Hill Creek
coc_df <- coc_df %>%
  mutate(range = case_when(
    sample_site == "Big Hill Creek Mouth" ~ "Upper Foothills",
    TRUE ~ "Lower Foothills"
  ))

# align variable names to AEPA template
coc_df <- coc_df %>%
  rename("station" = "sample_site") %>%
  rename("variable_name" = "parameter") %>%
  rename("measurement_value" = "numeric_result") %>%
  rename("measurement_flag" = "result_qualifier") %>%
  rename("unit_code" = "result_units")

coc_df <- coc_df %>%
  select(agency, range, station, sample_date, variable_name, measurement_value, measurement_flag, unit_code)

coc_df <- coc_df %>%
  add_column(detection_limit = NA) %>%
  add_column(flow_cms = NA)

# change parameter names to AEPA format
coc_df$variable_name[coc_df$parameter == "Total Dissolved Solids (TDS)(Calculated)"] <- "TOTAL DISSOLVED SOLIDS (CALCD.)"
coc_df$variable_name[coc_df$variable_name == "pH"] <- "PH (LAB)"
coc_df$variable_name[coc_df$variable_name == "Specific Conductance (Field)"] <- "SPECIFIC CONDUCTANCE (FIELD)"
coc_df$variable_name[coc_df$variable_name == "Turbidity"] <- "TURBIDITY"
coc_df$variable_name[coc_df$variable_name == "Nitrate (NO3-N)"] <- "NITROGEN, NITRATE"
coc_df$variable_name[coc_df$variable_name == "Total Nitrogen"] <- "NITROGEN TOTAL (CALCD.)"
coc_df$variable_name[coc_df$variable_name == "Ammonia (NH3-N)"] <- "AMMONIA TOTAL"
coc_df$variable_name[coc_df$variable_name == "Total Phosphorus (TP)"] <- "PHOSPHORUS TOTAL (P)"
coc_df$variable_name[coc_df$variable_name == "Total Dissolved Phosphorus (TDP)"] <- "PHOSPHORUS TOTAL DISSOLVED"
coc_df$variable_name[coc_df$variable_name == "Chloride (Cl)"] <- "CHLORIDE DISSOLVED"
coc_df$variable_name[coc_df$variable_name == "Sulphate (SO4)"] <- "SULPHATE DISSOLVED"
coc_df$variable_name[coc_df$variable_name == "Sodium Adsorption Ratio (SAR)(Calculated)"] <- "SODIUM ADSORPTION RATIO (CALCD.)"
coc_df$variable_name[coc_df$variable_name == "Total Organic Carbon (TOC)"] <- "CARBON TOTAL ORGANIC (TOC)"
coc_df$variable_name[coc_df$variable_name == "E.coli"] <- "ESCHERICHIA COLI"
coc_df$variable_name[coc_df$variable_name == "Dissolved Oxygen (Field)"] <- "OXYGEN DISSOLVED (FIELD METER)"
coc_df$variable_name[coc_df$variable_name == "Water Temperature (Field)"] <- "TEMPERATURE WATER"

coc_stations <- c(
  "Bow River Upstream of Highwood River",
  "Bow River Cushing Bridge",
  "Big Hill Creek Mouth",
  "Fish Creek Mouth",
  "Bow River Below Bearspaw Dam"
)

coc_df <- coc_df %>%
  filter(station %in% coc_stations) %>%
  filter(variable_name %in% wq_parameters)



#### AEPA ####
aepa_df <- read_csv(here("data", "Water Quality-2023-06-12_GovofAlberta.csv")) %>%
  clean_names()

aepa_df$sample_date_time <- mdy_hm(aepa_df$sample_date_time) %>%
  as_date()

aepa_df <- aepa_df %>%
  rename("sample_date" = "sample_date_time")

aepa_stations <- c(
  "BOW RIVER, AT CLUNY",
  "BOW RIVER, AT COCHRANE",
  "BOW RIVER, BELOW CARSELAND DAM",
  "BOW RIVER, NEAR RONALANE BRIDGE",
  "WEST ARROWWOOD CREEK, D/S OF SYPHON",
  "EAST ARROWWOOD CREEK, NEAR THE MOUTH",
  "CROWFOOT CREEK, ON HWY 1",
  "SHEEP RIVER, 1.6 KM D/S OF HWY 2",
  "HIGHWOOD RIVER, AT HWY 552",
  "PINE CREEK, NEAR THE MOUTH",
  "ELBOW RIVER, AT 9TH AVE BRIDGE",
  "BOW RIVER, AT COCHRANE",
  "JUMPINGPOUND CREEK, NEAR MOUTH",
  "COAL CREEK, 1/2 MILE WEST OF BOW CITY",
  "NEW WEST COULEE, AT HWY 36 CROSSING",
  "TWELVE MILE CREEK, AT GAUGING STATION (RANGE ROAD 525)",
  "NOSE CREEK, NEAR THE MOUTH-MEMORIAL DRIVE",
  "GHOST RIVER, ABOVE CONFLUENCE WITH WAIPAROUS CREEK"
)

aepa_df <- aepa_df %>%
  filter(station %in% aepa_stations) %>%
  filter(variable_name %in% wq_parameters)

aepa_df <- aepa_df %>%
  add_column(agency = "Alberta Environment and Protected Areas")

# create range column
aepa_df <- aepa_df %>%
  mutate(range = case_when(
    station == "GHOST RIVER, ABOVE CONFLUENCE WITH WAIPAROUS CREEK" ~ "Upper Foothills",
    station == "JUMPINGPOUND CREEK, NEAR MOUTH" ~ "Upper Foothills",
    station == "BOW RIVER, AT COCHRANE" ~ "Lower Foothills",
    station == "ELBOW RIVER, AT 9TH AVE BRIDGE" ~ "Lower Foothills",
    station == "NOSE CREEK, NEAR THE MOUTH-MEMORIAL DRIVE" ~ "Lower Foothills",
    station == "PINE CREEK, NEAR THE MOUTH" ~ "Lower Foothills",
    station == "HIGHWOOD RIVER, AT HWY 552" ~ "Lower Foothills",
    station == "SHEEP RIVER, 1.6 KM D/S OF HWY 2" ~ "Lower Foothills",
    station == "BOW RIVER, BELOW CARSELAND DAM" ~ "Middle Bow",
    station == "WEST ARROWWOOD CREEK, D/S OF SYPHON" ~ "Middle Bow",
    station == "EAST ARROWWOOD CREEK, NEAR THE MOUTH" ~ "Middle Bow",
    station == "BOW RIVER, AT CLUNY" ~ "Middle Bow",
    station == "CROWFOOT CREEK, ON HWY 1" ~ "Middle Bow",
    TRUE ~ "Lower Bow"
  ))

aepa_df <- aepa_df %>%
  select(agency, range, station, sample_date, variable_name, measurement_value, measurement_flag, unit_code)

aepa_df <- aepa_df %>%
  add_column(detection_limit = NA) %>%
  add_column(flow_cms = NA)

#### ENVIRONMENT CANADA ####
eccc_df <- read_csv(here("data", "Water-Qual-Eau-S-Saskatchewan-2000-present_ECCC.csv")) %>%
  clean_names()

eccc_df$date_time <- ymd_hm(eccc_df$date_time) %>%
  as_date()

# convert to standard date, strip time for flow join later
eccc_df <- eccc_df %>%
  add_column(agency = "Environment Canada")

eccc_stations <- c(
  "Bow River at Highway 1 Above Lake Louise",
  "Bow River About 4.5km Above Canmore"
)

# create range column
eccc_df <- eccc_df %>%
  mutate(range = case_when(
    site_no == "AL05BA0011" ~ "Upper Bow",
    site_no == "AL05BE0013" ~ "Upper Foothills",
    TRUE ~ NA
  ))

# align variable names to AEPA
eccc_df <- eccc_df %>%
  rename("station" = "site_no") %>%
  rename("sample_date" = "date_time") %>%
  rename("variable_name" = "variable") %>%
  rename("measurement_value" = "value") %>%
  rename("measurement_flag" = "flag") %>%
  rename("unit_code" = "unit")

eccc_df <- eccc_df %>%
  mutate(station = case_when(
    station == "AL05BA0011" ~ "Bow River at Highway 1 Above Lake Louise",
    station == "AL05BE0013" ~ "Bow River About 4.5km Above Canmore",
    TRUE ~ NA
  ))

eccc_df <- eccc_df %>%
  select(agency, range, station, sample_date, variable_name, measurement_value, measurement_flag, unit_code)

# change parameter names to AEPA format
eccc_df$variable_name[eccc_df$variable_name == "TOTAL DISSOLVED SOLIDS (FILTERABLE RESIDUE) (CALCD.)"] <- "TOTAL DISSOLVED SOLIDS (CALCD.)"
eccc_df$variable_name[eccc_df$variable_name == "PH"] <- "PH (LAB)" # ECCC pH might be field
eccc_df$variable_name[eccc_df$variable_name == "SPECIFIC CONDUCTANCE"] <- "SPECIFIC CONDUCTANCE (FIELD)"
eccc_df$variable_name[eccc_df$variable_name == "DISSOLVED NITRITE/NITRATE"] <- "NITROGEN, NITRATE"
eccc_df$variable_name[eccc_df$variable_name == "NITROGEN TOTAL (CALCD.)"] <- "NITROGEN TOTAL (CALCD.)"
# could be problem with comparing ECCC dissolved ammonia to AEPA/COC total ammonia
eccc_df$variable_name[eccc_df$variable_name == "PHOSPHORUS TOTAL"] <- "PHOSPHORUS TOTAL (P)"
eccc_df$variable_name[eccc_df$variable_name == "CARBON TOTAL ORGANIC (CALCD.)"] <- "CARBON TOTAL ORGANIC (TOC)"
eccc_df$variable_name[eccc_df$variable_name == "OXYGEN DISSOLVED"] <- "OXYGEN DISSOLVED (FIELD METER)"
eccc_df$variable_name[eccc_df$variable_name == "TEMPERATURE WATER (FIELD)"] <- "TEMPERATURE WATER"

eccc_df <- eccc_df %>%
  filter(station %in% eccc_stations) %>%
  filter(variable_name %in% wq_parameters)

eccc_df <- eccc_df %>%
  add_column(detection_limit = NA) %>%
  add_column(flow_cms = NA)

#### CONSOLIDATED DATA ####
# bind three dataframes
consolidated_df <- do.call("rbind", list(coc_df, aepa_df, eccc_df))

# standardize unit_code formatting
consolidated_df$unit_code[consolidated_df$unit_code == "°C"] <- "deg C"
consolidated_df$unit_code[consolidated_df$unit_code == "DEG C"] <- "deg C"
consolidated_df$unit_code[consolidated_df$unit_code == "åµS/CM"] <- "µS/cm"
consolidated_df$unit_code[consolidated_df$unit_code == "uS/cm"] <- "µS/cm"
consolidated_df$unit_code[consolidated_df$unit_code == "PH"] <- "pH units"
consolidated_df$unit_code[consolidated_df$unit_code == "ratio"] <- "rel units"
consolidated_df$unit_code[consolidated_df$unit_code == "REL UNIT"] <- "rel units"
consolidated_df$unit_code[consolidated_df$unit_code == "NO/100ML"] <- "No/100 mL"
consolidated_df$unit_code[consolidated_df$unit_code == "MG/L"] <- "mg/L"

# standardize name formats
consolidated_df$station <- toupper(consolidated_df$station)
consolidated_df$agency <- toupper(consolidated_df$agency)
consolidated_df$range <- toupper(consolidated_df$range)

# Four values from ECCC recorded as 'NR' replaced by NA when converting column to numeric
consolidated_df$measurement_value <- as.numeric(consolidated_df$measurement_value)

# align all data to year range of 2002-2022
consolidated_df <- consolidated_df %>%
  mutate(year = year(sample_date)) %>%
  filter(year > 2001 & year < 2023) %>%
  select(-year)

write_excel_csv(consolidated_df, here("output", "consolidated_sow_wq.csv"))
