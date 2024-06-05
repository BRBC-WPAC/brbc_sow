# Author: Eric Camm - City of Calgary
# Date: 2024-04-05
# Script to consolidate WQ data for BRBC SOW

#### SETUP ####
library(tidyhydat)
library(tidyverse)
library(here)
library(janitor)
library(openxlsx)
library(data.table)
library(zoo)
library(readxl)
library(duckdb)
source("constants.R")

dir.create("output", showWarnings = FALSE)

# for Nadine
# wd ="C:/Users/nadine.taube/OneDrive - Government of Alberta/Documents/BRBC/State of the watershed/Analysis"  
# setwd(wd)

#FUNCTIONS
count_num <- function(x) {sum(!is.na(x))}
count_NA <- function(x) {sum(is.na(x))}

# vector of SOW parameters
wq_parameters <- c(
  "TOTAL DISSOLVED SOLIDS (CALCD.)",
  "PH",
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
  "TOTAL SUSPENDED SOLIDS (TSS)"
)

#### CITY OF CALGARY ####
  coc_df <- read_csv(here("data", "Watershed_Surface_Water_Quality_Data_CityofCalgary.csv")) %>%
    clean_names()
  
  # convert to standard date, strip time for flow join on later
  coc_df$sample_date <- mdy_hm(coc_df$sample_date) 
  
  #Total Nitrogen calculation  
  #need to add in TN after 2018
  #will not have ID number for this calculated value
  coc_tn <- coc_df %>%
    mutate(year = year(sample_date)) %>% 
    filter(year > 2018) %>% 
    filter(parameter %in% c("NOx (Calculated)", "Total Kjeldahl Nitrogen (TKN)")) %>% 
    select(-id, -numeric_result, -result_qualifier) %>% #needed for pivoting
    pivot_wider(names_from = parameter, values_from = formatted_result) %>% 
    mutate(`NOx (Calculated)_num` = as.numeric(`NOx (Calculated)`)) %>% #create new numeric column to retain censored values
    mutate(`Total Kjedahl Nitrogen (TKN)_num` = as.numeric(`Total Kjeldahl Nitrogen (TKN)`)) %>% #create new numeric column to retain censored values
    replace_na(.,list(`NOx (Calculated)_num` = 0, `Total Kjedahl Nitrogen (TKN)_num` = 0)) %>%
    mutate(`Total Nitrogen` = `NOx (Calculated)_num` + `Total Kjedahl Nitrogen (TKN)_num`) %>% #TN calculation
    select(-c(`NOx (Calculated)_num`,`Total Kjedahl Nitrogen (TKN)_num`))  #remove numeric columns
  
    
  coc_tn$`Total Nitrogen`[coc_tn$`Total Nitrogen` == 0] <- NA #when both NOx and TKN are censored substitute NA
  
coc_tn <- coc_tn %>%  pivot_longer(cols = -sample_site:-year, 
               names_to = "parameter", 
               values_to = "formatted_result",
               values_transform = list(formatted_result = "as.character")) %>% 
  relocate(formatted_result, .before = result_units) 

coc_tn <- coc_tn %>% 
  mutate(result_qualifier = case_when(
    grepl("<", formatted_result) ~ "<",
    TRUE ~ NA
  ))

coc_tn$numeric_result <- coc_tn$formatted_result
coc_tn$numeric_result <- gsub("<", "", coc_tn$numeric_result) 
coc_tn <- coc_tn %>% 
  relocate("result_qualifier", .before = "formatted_result") %>% 
  relocate("numeric_result", .before = result_qualifier) %>% 
  relocate("parameter", .after = "sample_date")

coc_tn$numeric_result <- as.numeric(coc_tn$numeric_result)

coc_df <- full_join(coc_df, coc_tn) %>%
  select(-year)
 
#add agency 
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
coc_df$variable_name[coc_df$variable_name == "Total Dissolved Solids (TDS)(Calculated)"] <- "TOTAL DISSOLVED SOLIDS (CALCD.)"
coc_df$variable_name[coc_df$variable_name == "pH"] <- "PH"
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
coc_df$variable_name[coc_df$variable_name == "Total Suspended Solids (TSS)"] <- "TOTAL SUSPENDED SOLIDS (TSS)"
# coc_df$variable_name[coc_df$variable_name == "Dissolved Oxygen (Field)"] <- "OXYGEN DISSOLVED (FIELD METER)"
# coc_df$variable_name[coc_df$variable_name == "Water Temperature (Field)"] <- "TEMPERATURE WATER"

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

aepa_df2 <- read_csv(here("data", "Water Quality-2023_GovofAB_TSS_TDS_variables.csv")) %>% 
  clean_names()

aepa_df2$sample_date_time <- mdy_hms(aepa_df2$sample_date_time) %>% 
  as_date()

aepa_df <- rbind(aepa_df, aepa_df2)


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

aepa_df$variable_name[aepa_df$variable_name == 	"PH (FIELD)"] <- "PH"
aepa_df$variable_name[aepa_df$variable_name == 	"RESIDUE NONFILTERABLE"] <- "TOTAL SUSPENDED SOLIDS (TSS)"

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

eccc_df <- eccc_df %>% 
  filter(variable_name != "PH")


# change parameter names to AEPA format
eccc_df$variable_name[eccc_df$variable_name == "TOTAL DISSOLVED SOLIDS (FILTERABLE RESIDUE) (CALCD.)"] <- "TOTAL DISSOLVED SOLIDS (CALCD.)"
eccc_df$variable_name[eccc_df$variable_name == "PH (FIELD)"] <- "PH" # ECCC pH might be field
eccc_df$variable_name[eccc_df$variable_name == "SPECIFIC CONDUCTANCE"] <- "SPECIFIC CONDUCTANCE (FIELD)"
eccc_df$variable_name[eccc_df$variable_name == "DISSOLVED NITRITE/NITRATE"] <- "NITROGEN, NITRATE"
eccc_df$variable_name[eccc_df$variable_name == "NITROGEN TOTAL (CALCD.)"] <- "NITROGEN TOTAL (CALCD.)"
# could be problem with comparing ECCC dissolved ammonia to AEPA/COC total ammonia
eccc_df$variable_name[eccc_df$variable_name == "PHOSPHORUS TOTAL"] <- "PHOSPHORUS TOTAL (P)"
eccc_df$variable_name[eccc_df$variable_name == "CARBON TOTAL ORGANIC (CALCD.)"] <- "CARBON TOTAL ORGANIC (TOC)"
eccc_df$variable_name[eccc_df$variable_name == "OXYGEN DISSOLVED"] <- "OXYGEN DISSOLVED (FIELD METER)"
eccc_df$variable_name[eccc_df$variable_name == "TEMPERATURE WATER (FIELD)"] <- "TEMPERATURE WATER"
eccc_df$variable_name[eccc_df$variable_name == "RESIDUE NONFILTERABLE"] <- "TOTAL SUSPENDED SOLIDS (TSS)"

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


#check for missing data for all stations and parameters
# dat.miss.p <- consolidated_df %>%
#   group_by(station) %>%
#   count(variable_name) %>% 
#   ungroup() %>% 
#   complete(station, variable_name) %>% 
#   View()

write_excel_csv(consolidated_df, here("output", "consolidated_sow_wq.csv"))

  
#Nadine Taube - Alberta Environment and Protected Areas  
#### ADD AEPA SAR back in ####

##### ISSUES -----
#Waiparous seems to be missing from both WQual datasets
#AEPA data is missing TN
#ECCC doesn't have NH3

# Load consolidated df
#consolidated_df <- read_excel("./consolidated_sow_wq.xlsx", sheet="consolidated_sow_wq", guess_max = 1048576)
consolidated_df.read <- fread(here("output", "consolidated_sow_wq.csv"))
str(consolidated_df.read)

# formatting dates and taking out SAR
consolidated_df2 <- consolidated_df.read %>% 
  mutate(sample_datetime = as.POSIXct(sample_date, format="%Y/%m/%d %H:%M:%S", tz = "UTC")) %>%
  mutate(sample_date = as.Date(sample_datetime, format = "%Y-%m-%d", origin = "1899-12-30")) %>%
  filter(! (variable_name == "SODIUM ADSORPTION RATIO (CALCD.)" & agency == "ALBERTA ENVIRONMENT AND PROTECTED AREAS"))

str(consolidated_df2)

# load AEPA SAR
df.AEPA <- fread(here("data", "Water Quality-2024-04-04_AEPA_wSARcalc.csv"))
stn_lookup <- read_excel(here("data", "Flow Data Availability.xlsx"), sheet="master", guess_max = 1048576)

intersect(unique(df.AEPA$Station),unique(stn_lookup$StationName_WQdataset)) 

# get AEPA SAR to match consolidated df
df.SAR.TN <- df.AEPA %>% filter(VariableName %in% c("SODIUM ADSORPTION RATIO (CALCD.)", "NITROGEN TOTAL (CALCD.)")) %>%
  mutate(sample_datetime = as.POSIXct(SampleDateTime, format="%Y/%m/%d %H:%M:%S", tz = "UTC")) %>%
  mutate(sample_date = as.Date(SampleDate, format = "%Y-%m-%d", origin = "1899-12-30")) %>%
  #left_join(stn_lookup %>% select(StationName_WQdataset), 
  #          by = c("Station" = "StationName_WQdataset")) %>%
  select(Station, VariableName, sample_datetime, sample_date, MeasurementValue, MeasurementFlag, UnitCode)

df.SAR.TN <- df.SAR.TN %>%
  add_column(agency = "ALBERTA ENVIRONMENT AND PROTECTED AREAS") %>%
  add_column(detection_limit = NA) %>%
  add_column(flow_cms = NA) %>%
  rename(station = Station, 
         variable_name = VariableName, 
         measurement_value = MeasurementValue,
         measurement_flag = MeasurementFlag,
         unit_code = UnitCode)
  
# create range column
df.SAR.TN <- df.SAR.TN %>%
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

# bind to consolidated_df2
str(df.SAR.TN)
str(consolidated_df2)
consolidated_df.new <- rbind(consolidated_df2, df.SAR.TN)

fwrite(consolidated_df.new, file=here("output", "./consolidated_sow_wq_wSARandTN.csv"), row.names=FALSE)

#### ADD DL ####
consolidated_df <- consolidated_df.new

consolidated_df$measurement_value <- as.numeric(consolidated_df$measurement_value)
consolidated_df$measurement_flag <- as.factor(consolidated_df$measurement_flag)
consolidated_df <- consolidated_df %>% 
  mutate(cen_logic = ifelse(consolidated_df$measurement_flag %in% c("L", "<"), T, F))


# summarise
overview1 <- consolidated_df %>% group_by(agency, variable_name) %>%
  summarise(count.samples = length(measurement_value),
            min.date = min(sample_date),
            max.date = max(sample_date),
            count.values = sum(!is.na(measurement_value)),
            count.LFLAG = sum(measurement_flag %in% c("L", "<")),
            perc.BDL = count.LFLAG/count.values*100,
            list.DLs = list(unique(measurement_value[measurement_flag %in% c("L", "<")])),
            count.years = length(unique(year(sample_date))),
            count.months = length(month(sample_date)),
            count.stns = length(unique(station)),
            Min  = quantile(measurement_value, probs = 0, na.rm = TRUE),
            p10 = quantile(measurement_value, probs = 0.10, na.rm = TRUE),
            p25 = quantile(measurement_value, probs = 0.25, na.rm = TRUE),
            Mean = mean(measurement_value, na.rm = TRUE),
            Median = median(measurement_value, na.rm = TRUE),
            p75 = quantile(measurement_value, probs = 0.75, na.rm = TRUE),
            p90 = quantile(measurement_value, probs = 0.90, na.rm = TRUE),
            Max  = quantile(measurement_value, probs = 1, na.rm = TRUE),
            StDev  = sd(measurement_value, na.rm = TRUE))


# Summarize Percent BDLs Before MAX DL Update
mdl.summary = consolidated_df %>% 
  group_by(agency, variable_name) %>% 
  summarise(n_censored = sum(measurement_flag %in% c("<", "L"), na.rm = TRUE),
            n_total = sum(!is.na(measurement_value)), 
            percent_censored = round((n_censored / n_total)*100, 2), .groups = 'drop',
            min.date = min(sample_date), max.date = max(sample_date)) 

# Summarize Actual BDLs
mdl.scan = consolidated_df %>% filter(measurement_flag %in% c("<", "L"))
mdl.scan.details = mdl.scan %>% 
  group_by(agency, variable_name, "measurement_value" = as.numeric(measurement_value)) %>% 
  summarise(n = sum(measurement_flag %in% c("<", "L"), na.rm = TRUE), .groups = 'drop',
            min.date = min(sample_date), max.date = max(sample_date))

df.max.mdl <- mdl.scan.details %>% group_by(agency, variable_name) %>%
  summarise(maxDL = max(measurement_value), .groups = 'drop')  

write.csv(mdl.scan.details, file=here("output", "./MDL Scan Details.csv"), row.names=FALSE)
write.csv(df.max.mdl, file=here("output", "./MAX MDL Scan Details.csv"), row.names=FALSE)

# check that the highest main DL makes sense e.g. compare df.max.mdl and mdl.scan.details
# changed TOC, Cl, NO3, TDP, TSS for AEPA and NH3 for CoC

# read final maxDL back in
max.mdl = read.csv(here("output", "./MAX MDL Scan Details.csv"))

# Substitute to Max DL
df.mdl = NULL

for (a in levels(factor(max.mdl$agency))){
  max.mdl.temp = max.mdl %>% filter(agency == a) #max.mdl only has the variables that have values BDL
  for (i in levels(factor(max.mdl.temp$variable_name))){
    max.mdl.f = max.mdl.temp %>% filter(variable_name == i) 
    fdf = consolidated_df %>% filter(agency == a, variable_name == i) %>%
      mutate(measurement_flag2 = ifelse(measurement_value <= max.mdl.f$maxDL, T, F)) %>%
      mutate(measurement_maxDLsub = ifelse(measurement_flag2 == T, max.mdl.f$maxDL, measurement_value)) %>%
      mutate(measurement_flag2 = ifelse(measurement_value <= max.mdl.f$maxDL, T, cen_logic)) 
    df.mdl = rbind(df.mdl, fdf)
  }
}

# put the data back together 
anti.join <- anti_join(consolidated_df, df.mdl) %>% #return all rows from original without a match in maxDL one
  mutate(measurement_flag2 = cen_logic,
         measurement_maxDLsub = measurement_value)
consolidated_df_maxDL <- rbind(anti.join, df.mdl)

# summarise again to see how perc.BDL changes
overview2 <- consolidated_df_maxDL %>% group_by(agency, variable_name) %>%
  summarise(min.date = min(sample_date),
            max.date = max(sample_date),
            count.values = sum(!is.na(measurement_maxDLsub)),
            count.LFLAG = sum(cen_logic),
            count.LFLAG2 = sum(measurement_flag2), #after substituting maxDL
            perc.BDL = count.LFLAG/count.values*100,
            perc.BDL2 = count.LFLAG2/count.values*100, #after substituting maxDL
            list.DLs = list(unique(measurement_value[cen_logic == TRUE])),
            Min  = quantile(measurement_maxDLsub, probs = 0, na.rm = TRUE),
            p10 = quantile(measurement_maxDLsub, probs = 0.10, na.rm = TRUE),
            p25 = quantile(measurement_maxDLsub, probs = 0.25, na.rm = TRUE),
            Mean = mean(measurement_maxDLsub, na.rm = TRUE),
            Median = median(measurement_maxDLsub, na.rm = TRUE),
            p75 = quantile(measurement_maxDLsub, probs = 0.75, na.rm = TRUE),
            p90 = quantile(measurement_maxDLsub, probs = 0.90, na.rm = TRUE),
            Max  = quantile(measurement_maxDLsub, probs = 1, na.rm = TRUE),
            StDev  = sd(measurement_maxDLsub, na.rm = TRUE))

#### ADD FLOW ####

# stn_lookup <- read_excel("./Flow Data Availability.xlsx", sheet="master", guess_max = 1048576)
dfw.AEPA_flow <- read_excel(here("data", "AEPA_SWQMF_Flow_BOW_1999-2022.xlsx"), sheet="flow", guess_max = 1048576) #Cochrane, Carseland, Cluny
# PC and NC
df.PCNC.raw <- fread(here("data", "DATA_COC_PINE_NOSE_FLOW.csv"))
# Big Hill Creek (BHC)
df.BHC.raw <- read_excel(here("data", "DATA_Masaki, Big_Hill_Creek_Q_2011-2020.xlsx"), sheet="data", range = cell_cols("A:B"), guess_max = 1048576)

# identify date-related inputs 
startAnalysis   <- '2002/01/01'  ## start of common period
endAnalysis     <- '2022/12/31'  ## end of common period
startYear_WY    <- 11            ## input water year start month
startSeasonal   <- 4             ## input seasonal flow grouping start month
endSeasonal     <- 11            ## input seasonal end month; keep higher than start month
decSeasonal     <- 'OpenWater'   ## declare seasonal flow group name
invSeasonal     <- 'Winter'      ## declare inverse flow group name
SeasonalFilter  <- c('OpenWater', 'Winter')  

##### IMPORT: WSC DATA ####

# from Flow Data Availability.xlsx:
# need 05BE004,05BA001,05BH004,05BN012,05BM008,05BJ001,05BK001,05BG010,05BL024,05BH015,05BN006,05BL012,05BN002,05BM014,05BG006
# Bow u/s Highwood (BRHR) -> WQ station where flow will be calculated from other flow stations
# Pine Creek and Nose Creek from City, Big Hill Creek from Masaki

# select analysis gauges from HYDAT
gauges_WSC <- c("05BE004","05BA001","05BH004","05BN012","05BM008","05BJ001","05BK001","05BG010","05BL024",
                "05BH015","05BN006","05BL012","05BN002","05BM014","05BG006")

gauges_WSC <- as.matrix(gauges_WSC)

# import HYDAT data LONG, refine, map to study date range
dfl.HYDAT <- hy_daily_flows(station_number = gauges_WSC, start_date = "2002-01-01", end_date = "2022-12-31") #tidyhydat package
dfl.HYDAT <- dfl.HYDAT %>% 
  filter(Parameter == "Flow") %>% group_by(STATION_NUMBER) %>% 
  #complete(Date = seq.Date(min(as.Date(startAnalysis)), max(as.Date(endAnalysis)), by ="day")) %>%
  select(STATION_NUMBER, Date, Value)

dfl.HYDAT <- dfl.HYDAT %>%
  left_join(
    hy_stations(station_number = gauges_WSC) %>%
      select(STATION_NUMBER, STATION_NAME, DRAINAGE_AREA_GROSS), by = "STATION_NUMBER") %>%
  ## conversion to mm/d
  mutate(runoff_mm.d = Value / DRAINAGE_AREA_GROSS * 86400 / 1e6 * 1e3)

dfl.HYDAT <- dfl.HYDAT %>% 
  mutate(day       = as.numeric(format(Date, format = '%d')),
         jday       = yday(Date),
         Year_Cal  = as.numeric(format(Date, format = '%Y')),
         Month_Cal = as.numeric(format(Date, format = '%m')),
         Year_WY   = ifelse(Month_Cal >= startYear_WY, Year_Cal + 1, Year_Cal),
         SeasonalFilter = ifelse(Month_Cal >= startSeasonal & Month_Cal <= endSeasonal, 
                                 decSeasonal, invSeasonal),
         Month_WY = ifelse(Month_Cal == 10 & !is.na(Value),  1, # & !is.na(Value)
                           ifelse(Month_Cal == 11 & !is.na(Value),  2, 
                                  ifelse(Month_Cal == 12 & !is.na(Value),  3, 
                                         ifelse(Month_Cal == 1 & !is.na(Value),  4,
                                                ifelse(Month_Cal == 2 & !is.na(Value),  5,
                                                       ifelse(Month_Cal == 3 & !is.na(Value),  6,
                                                              ifelse(Month_Cal == 4 & !is.na(Value),  7,
                                                                     ifelse(Month_Cal == 5 & !is.na(Value),  8,
                                                                            ifelse(Month_Cal == 6 & !is.na(Value),  9,
                                                                                   ifelse(Month_Cal == 7 & !is.na(Value),  10,
                                                                                          ifelse(Month_Cal == 8 & !is.na(Value),  11,
                                                                                                 ifelse(Month_Cal == 9 & !is.na(Value),  12, NA))))))))))))
  ) 

##### Summaries ####
OverallStats_dailyQ_hydat <- dfl.HYDAT %>%
  group_by(STATION_NUMBER) %>% 
  dplyr::summarise(
    min_yr = min(year(Date)[!is.na(Value)]), max_yr = max(year(Date)[!is.na(Value)]),
    min_date = min(Date[!is.na(Value)]), max_date = max(Date[!is.na(Value)]),
    total.days = as.numeric(difftime(max_date, min_date, units = "days"))+1,
    count.flow      = count_num(Value),
    countNA.flow   = count_NA(Value),
    Perc.missing = round((total.days-count.flow)/total.days*100,1),
    Min  = quantile(Value, probs=0, na.rm = TRUE),
    Q10 = quantile(Value, probs=0.1, na.rm = TRUE),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q90 = quantile(Value, probs=0.9, na.rm = TRUE),
    Max  = quantile(Value, probs=1, na.rm = TRUE),
    CV = sd(Value, na.rm = TRUE)/mean(Value, na.rm = TRUE))

AnnualStats_dailyQ_hydat <- dfl.HYDAT %>%
  group_by(STATION_NUMBER, year(Date)) %>% 
  dplyr::summarise(
    min_date = min(Date[!is.na(Value)]), max_date = max(Date[!is.na(Value)]),
    total.days = as.numeric(difftime(max_date, min_date, units = "days"))+1,
    count.flow      = count_num(Value),
    countNA.flow   = count_NA(Value),
    Perc.missing = round((total.days-count.flow)/total.days *100,1),
    Min  = quantile(Value, probs=0, na.rm = TRUE),
    Q10 = quantile(Value, probs=0.1, na.rm = TRUE),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q90 = quantile(Value, probs=0.9, na.rm = TRUE),
    Max  = quantile(Value, probs=1, na.rm = TRUE),
    CV = sd(Value, na.rm = TRUE)/mean(Value, na.rm = TRUE),
    Mo_from = min(month(Date)[!is.na(Value)]),
    Mo_to = max(month(Date)[!is.na(Value)])) %>%
  mutate(SeasonalFlag = ifelse(Mo_from == 1 & Mo_to == 12, "Continuous", "Seasonal"))

fwrite(OverallStats_dailyQ_hydat, file = here("output", "./overall_summary-hydat.csv"))
fwrite(AnnualStats_dailyQ_hydat, file = here("output", "./annual_summary-hydat.csv"))

##### CALCULATE missing Q stations ----- 
# turn hydat wide
dfw.dailyQ <- dfl.HYDAT %>% 
  select(c(STATION_NUMBER, Date, Value, jday, SeasonalFilter)) %>%
  pivot_wider(names_from = STATION_NUMBER, values_from = c(Value))

# add Cochrane, Carseland, Cluny
str(dfw.AEPA_flow)
dfw.dailyQ.complete <- dfw.dailyQ %>% left_join(dfw.AEPA_flow %>% select(-c("units", "05BN012")), by = c("Date" = "date"))
# left join dfw.dailyQ to dfw.AEPA_flow

# calculate missing stations according to Flow Data Availability.xlsx i.e. Bow u/s of Highwood
dfw.dailyQ.complete <- dfw.dailyQ.complete %>% 
  mutate(BRHR = ifelse(is.na(`05BM002`) | is.na(`05BL024`), NA, `05BM002` - `05BL024`))

# get Nose Creek and Pine Creek and Big Hill Creek ready
df.PCNC <- df.PCNC.raw %>%
  mutate(SAMPLE_DATETIME = as.POSIXct(date, format = "%Y/%m/%d %H:%M:%S")) %>%
  mutate(SAMPLE_DATE = as.Date(date, format = "%Y/%m/%d")) #, origin = "1899-12-30"

df.PCNC.dailyQ <- df.PCNC %>%
  group_by(site, SAMPLE_DATE) %>%
  summarise(Value = mean(flow_cms), .groups = 'drop') %>%
  mutate(Flow_StationNumber = ifelse(site == "PC2A", "PC", "NC")) %>%
  rename('Date' = 'SAMPLE_DATE') %>%
  filter(!is.na(Value)) %>%
  select(Date, Value, Flow_StationNumber)

df.BHC.dailyQ <- df.BHC.raw %>% 
  rename("Value" = `Q (m3/s)`) %>%
  filter(!is.na(Value)) %>%
  mutate(Flow_StationNumber = "BHC")

#checkna <- df.BHC.raw %>% filter(is.na(Date))

##### consolidate flow ####
# turn dfw.dailyQ.complete long (station number col needs to be called Flow_StationNumber to match with lookup table)
str(dfw.dailyQ.complete)
colnames(dfw.dailyQ.complete)
df.dailyQ.complete <- dfw.dailyQ.complete %>%
  select(-c('jday', 'SeasonalFilter')) %>%
  pivot_longer(cols = c(2:20), names_to = "Flow_StationNumber", values_to = "Value") %>%
  select(Date, Value, Flow_StationNumber) %>%
  arrange(Flow_StationNumber, Date)

# rbind PCNC and BHC to it
colnames(df.dailyQ.complete)
colnames(df.BHC.dailyQ)
colnames(df.PCNC.dailyQ)
df.dailyQ.join <- rbind(df.dailyQ.complete, df.BHC.dailyQ, df.PCNC.dailyQ)

##### MERGE WQ and FLOW DATAFRAMES --------------

# merge station lookup table to water quality and flow df
str(stn_lookup)
str(df.dailyQ.join)
dfl.flow <- df.dailyQ.join %>% 
  left_join(stn_lookup %>%
              select(StationName_WQdataset, WQTC_StationName, Flow_StationNumber, Flow_StationName),
            by = "Flow_StationNumber")

df.missingWQstn <- dfl.flow %>% filter(is.na(StationName_WQdataset)) #checking if all flow values have a WQ name associated with them

str(consolidated_df_maxDL)
merge.df1 <- consolidated_df_maxDL %>% 
  left_join(stn_lookup %>%
              select(StationName_WQdataset, Flow_StationNumber, Flow_StationName, WQTC_StationName),
            by = c("station" = "StationName_WQdataset"))

df.missingflowstn <- merge.df1 %>% filter(is.na(Flow_StationNumber)) 
unique(df.missingflowstn$station)
unique(df.missingflowstn$WQTC_StationName)

# merging daily flows to WQ
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

merge.df2 <- merge.df1 %>% left_join(dfl.flow, by = c("Flow_StationNumber", "Flow_StationName" , "sample_date" = "Date", "WQTC_StationName")) %>%
  rename(daily.flow.cms = Value) %>%
  mutate('YEAR' = year(sample_date)) %>% 
  mutate("MONTH_YEAR" = as.yearmon(sample_date, "%m-%Y")) %>%
  mutate(MONTH_YEAR = as.Date(MONTH_YEAR, format = "%m-%Y")) %>%
  mutate(Season = ifelse(month(sample_date) %in% 4:10, "Open", "Ice"),
         month_abb = factor(month.abb[month(sample_date)], month_labels))

consolidated_df_maxDL_flow <- merge.df2
fwrite(consolidated_df_maxDL_flow, file = here("output", CONSOLIDATED_DATA_FILE))
fwrite(dfl.flow, file = here("output", DAILY_FLOW_FILE))

# FINAL DF CHECKS AND SUMMARIES -----------------
#find WQ sites that have missing flows
missing.dailyQ <- merge.df2 %>% filter(is.na(daily.flow.cms))

#collapse columns so that we have the unique dates, variable, years, etc
missing.dailyQ.short <- missing.dailyQ %>% 
  group_by_at(vars(MONTH_YEAR, station)) %>%
  summarize_all(paste, collapse=",") %>% 
  select(c(1:2,5:6))

missing.dailyQ.stns <- missing.dailyQ %>% 
  group_by_at(vars(station, YEAR)) %>%
  summarize_all(paste, collapse=",") %>% 
  select(c(1:2,6, 23))

write.xlsx(missing.dailyQ.stns, here("output", "./missing dailyQs.xlsx"))
# only seasonal stations in the winter season should be missing, late 2021 and 2022 are often missing flow data
# Cushing Bridge, Coal Creek, East Arrowwood don't have any flow data
# Continuous flow: Canmore, Bearspaw, Carseland, Cluny, Cochrane, Ronalane, Elbow, Ghost, Highwood, Bow u/s Highwood, Jumpingpound, Sheep, Waiparous
# Seasonal flow: Lake Louise, Big Hill Creek (until 2020), Crowfoot Creek, Fish Creek, New West Coulee, Nose Creek, Pine Creek, TMCreek, West Arrowwood
# Big Hill Creek only has 2022 WQual data and flow data only goes to 2020

overview3 <- consolidated_df_maxDL_flow %>% group_by(station, WQTC_StationName, variable_name) %>%
  summarise(count.samples = length(measurement_value),
            min.date = min(sample_date),
            max.date = max(sample_date),
            count.values = sum(!is.na(measurement_maxDLsub)),
            count.LFLAG = sum(cen_logic),
            count.LFLAG2 = sum(measurement_flag2), #after substituting maxDL
            perc.BDL = count.LFLAG/sum(!is.na(measurement_value))*100,
            perc.BDL2 = count.LFLAG2/count.values*100,
            list.DLs = list(unique(measurement_value[measurement_flag %in% c("L", "<")])),
            count.years = length(unique(year(sample_date))),
            count.months = length(month(sample_date)),
            count.LFLAG2 = sum(measurement_flag2),
            Min  = quantile(measurement_maxDLsub, probs = 0, na.rm = TRUE),
            p10 = quantile(measurement_maxDLsub, probs = 0.10, na.rm = TRUE),
            p25 = quantile(measurement_maxDLsub, probs = 0.25, na.rm = TRUE),
            Mean = mean(measurement_maxDLsub, na.rm = TRUE),
            Mean.flow = mean(daily.flow.cms, na.rm = T),
            Median = median(measurement_maxDLsub, na.rm = TRUE),
            Median.flow = median(daily.flow.cms, na.rm = TRUE),
            p75 = quantile(measurement_maxDLsub, probs = 0.75, na.rm = TRUE),
            p90 = quantile(measurement_maxDLsub, probs = 0.90, na.rm = TRUE),
            Max  = quantile(measurement_maxDLsub, probs = 1, na.rm = TRUE),
            StDev  = sd(measurement_maxDLsub, na.rm = TRUE))
write.xlsx(overview3, here("output", "./summary overall.xlsx"))

overview4 <- consolidated_df_maxDL_flow %>% group_by(station, WQTC_StationName, variable_name, YEAR) %>%
  summarise(min.date = min(sample_date),
            max.date = max(sample_date),
            count.values = sum(!is.na(measurement_maxDLsub)),
            count.LFLAG = sum(cen_logic),
            count.LFLAG2 = sum(measurement_flag2), #after substituting maxDL
            perc.BDL = count.LFLAG/sum(!is.na(measurement_value))*100,
            perc.BDL2 = count.LFLAG2/count.values*100, #after substituting maxDL
            list.DLs = list(unique(measurement_value[measurement_flag %in% c("L", "<")])),
            count.years = length(unique(year(sample_date))),
            count.months = length(month(sample_date)),
            Min  = quantile(measurement_maxDLsub, probs = 0, na.rm = TRUE),
            p10 = quantile(measurement_maxDLsub, probs = 0.10, na.rm = TRUE),
            p25 = quantile(measurement_maxDLsub, probs = 0.25, na.rm = TRUE),
            Mean = mean(measurement_maxDLsub, na.rm = TRUE),
            Mean.flow = mean(daily.flow.cms, na.rm = T),
            Median = median(measurement_maxDLsub, na.rm = TRUE),
            Median.flow = median(daily.flow.cms, na.rm = TRUE),
            p75 = quantile(measurement_maxDLsub, probs = 0.75, na.rm = TRUE),
            p90 = quantile(measurement_maxDLsub, probs = 0.90, na.rm = TRUE),
            Max  = quantile(measurement_maxDLsub, probs = 1, na.rm = TRUE),
            StDev  = sd(measurement_maxDLsub, na.rm = TRUE))
write.xlsx(overview4, here("output", "./summary annual.xlsx"))
