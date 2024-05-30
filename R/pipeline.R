# Title: BRBC State of Watershed Reporting - Water Quality Percentile Analysis
# Author: Eric Camm, City of Calgary
# Date Created: Aug, 2023
# Description: Code to generate percentile bins of historical data that are compared to the median value of each year.
# Generates plots for each parameter and sample site

library(tidyverse)
library(here) #avoid local directory issues
library(janitor) #cleaning 
library(santoku) #percentile generation

# vector of relevant parameters
wq_parameters <- c(
  "TOTAL DISSOLVED SOLIDS (CALCD.)",
  "PH (FIELD)",
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

## guidelines
guidelines <- tribble(
  ~variable_name_unit, ~units, ~chronic, ~acute,
  "CHLORIDE DISSOLVED (mg/L)", "mg/L", 120, 640,
  "NITROGEN, NITRATE (mg/L)", "mg/", 3, 124,
  "OXYGEN DISSOLVED (FIELD METER) (mg/L)", "mg/L", 6.5, 5
)

# sulphate guidelines based on water hardness values.
# On average mainstem sites are in moderately hard (76-180) to very hard (181-250) range
# guideline would be 309 mg/L, or 429 mg/L for those ranges.

## import data - add file to 'data' folder within project folder
all_data <- read_csv(here("data", "AEPA_WQ.csv")) %>%
  clean_names()
all_data$sample_date_time <- mdy_hms(all_data$sample_date_time)
glimpse(all_data)

# extract units for parameters
units <- all_data %>%
  filter(variable_name %in% wq_parameters) %>%
  select(variable_name, unit_code) %>%
  distinct(variable_name, unit_code) %>%
  filter(unit_code != "JTU")

# vector for relevant sample stations
ltrn_stations <- c(
  "BOW RIVER, BELOW CARSELAND DAM",
  "BOW RIVER, AT CLUNY",
  "BOW RIVER, NEAR RONALANE BRIDGE",
  "BOW RIVER, AT COCHRANE"
)

# create filtered dataframe for analysis
ltrn_data_stations <- all_data %>%
  filter(variable_name %in% wq_parameters) %>%
  filter(station %in% ltrn_stations) %>%
  mutate(year = year(sample_date_time))

# stations function - generates percentiles of historical data
percentile_station <- function(data, max, parameter, site) {
  # site <- "BOW RIVER, AT CLUNY"
  # parameter <- "TURBIDITY"
  # data <- ltrn_data_stations
  # max <- 2022

  historical_range <- data %>%
    filter(variable_name == parameter) %>%
    filter(station == site) %>%
    filter(year != max) %>%
    filter(is.na(measurement_value) != TRUE)

  perc_distribution <- ecdf(historical_range$measurement_value)

  all_year <- data %>%
    filter(variable_name == parameter) %>%
    filter(station == site) %>%
    # filter(year == max) %>% #remove filter by last year to apply to all years
    group_by(station, variable_name, year) %>% # group by year to generate percentiles for each year
    summarize(avg = median(measurement_value)) %>% # comparing median against percentiles, could also use mean here
    mutate(perc_bin = perc_distribution(avg))

  return(all_year)
}

# testing function
percentile_station(ltrn_data_stations, 2022, "TEMPERATURE WATER", "BOW RIVER, AT CLUNY") %>%
  View()

# loop percentiles function over all parameters and stations
perc_results_stations <- c()
for (i in 1:length(ltrn_stations)) {
  perc_results_parameter <- c()

  for (j in 1:length(wq_parameters)) {
    perc_results_parameter[j] <- list(percentile_station(ltrn_data_stations, 2022, wq_parameters[j], ltrn_stations[i]))
  }
  perc_results_parameter <- bind_rows(perc_results_parameter)
  perc_results_stations[i] <- list(perc_results_parameter)
}
perc_results_stations <- bind_rows(perc_results_stations)

df_all <- full_join(perc_results_stations, ltrn_data_stations, by = c("station", "variable_name", "year"))

# generate percentile bins for plotting
# either base::cut or santoku::chop work here
df_all$colourcut <- cut(df_all$perc_bin, breaks = c(0, 0.1, 0.25, 0.75, 0.90, 1))

# plot function to to graph percentile outputs
plot_site <- function(site) {
  df_all %>%
    filter(station == site) %>%
    ggplot(aes(
      x = year, y = measurement_value, group = year,
      fill = colourcut
    )) +
    geom_boxplot() +
    # stat_summary(fun = "mean",col='red',geom='point') + #to add mean data point to plots
    scale_y_log10() +
    facet_wrap(~variable_name, scales = "free") +
    labs(
      title = site,
      subtitle = "Water Quality Percentile Comparisons",
      y = "log10(measurement_value)", x = ""
    ) +
    scale_fill_manual(
      name = "Percentile",
      values = c("#1f26f5", "#55d7da", "#1df129", "#e9ad29", "#b34044", NA),
      drop = FALSE, # need this argument to keep unused categories
      labels = c("<10th", "10th - 24th", "25th - 75th", "76th - 90th", ">90th", "Not ranked")
    ) +
    theme_bw()
}
plot_site("BOW RIVER, AT CLUNY")


# nested dataframe to use Purrr for iteration across all sites
df_all <- df_all %>%
  filter(unit_code != "JTU") %>%
  mutate(variable_name_unit = paste(variable_name, " (", unit_code, ")", sep = "")) # creating variable for parameter names

purrr <- df_all %>%
  group_by(station) %>%
  nest()

plots_purrr <- purrr %>%
  mutate(plot = map2(
    .x = station,
    .y = data,
    ~ ggplot(data = .y, aes(
      x = year, y = measurement_value, group = year,
      fill = colourcut
    )) +
      geom_boxplot() +
      geom_hline(data = guidelines, aes(yintercept = chronic, linetype = "Chronic"), color = "red") +
      geom_hline(data = guidelines, aes(yintercept = acute, linetype = "Acute"), color = "red") +
      scale_y_log10() +
      labs(
        title = .x,
        subtitle = "Water Quality Percentile Comparisons",
        y = "log10", x = ""
      ) +
      facet_wrap(~variable_name_unit, scales = "free") +
      scale_fill_manual(
        name = "Percentile",
        values = c("#1f26f5", "#55d7da", "#1df129", "#e9ad29", "#b34044", NA),
        drop = FALSE, # need this argument to keep unused categories if only colouring latest year
        labels = c("<10th", "10th - 24th", "25th - 75th", "76th - 90th", ">90th", "Not ranked")
      ) +
      scale_linetype_manual(name = "Guideline (AEPA)", values = c("solid", "dashed")) +
      theme_bw()
  ))
walk(plots_purrr$plot, print)

# generate filenames
plot_names <- ltrn_stations %>%
  map(paste0, "_median_logscale_allpercentiles.png")

# save function
walk2(
  plots_purrr$plot, plot_names,
  ~ ggsave(
    plot = .x,
    filename = .y,
    path = here("output"),
    height = 12,
    width = 18,
    dpi = 300
  )
)
