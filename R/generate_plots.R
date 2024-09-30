source("constants.R")
source("functions.R")
library(future)
library(iterators)
library(parallel)
library(foreach)
library(doParallel)
library(furrr)

# Use as many cores as we have in order to generate the images in parallel
plan(multisession, workers = detectCores())
doFuture::registerDoFuture()

# Generate plots ####
## Percentiles bucketed by year ####
stations <- get_stations()
variables <- get_variables()
the_list <- map(stations, function(station) {
  map(variables, function(variable) {
    list(station = station, variable = variable)
  })
}) %>% unlist(recursive = FALSE)
log_scale_filenames <- future_map(the_list, function(the_pair) {
  concentration_img( # nolint: object_usage_linter.
    the_pair$station,
    the_pair$variable
  )
})
conccentration_filenames <- future_map(the_list, function(the_pair) {
  concentration_img( # nolint: object_usage_linter.
    the_pair$station,
    the_pair$variable,
    log10 = FALSE
  )
})

## Scatter plot over time ####
scatter_filenames <- future_map(the_list, function(the_pair) {
  scatter_img( # nolint: object_usage_linter.
    the_pair$station,
    the_pair$variable
  )
})

## Flow over time ####
stations_with_flow <- get_stations(with_flow = TRUE)
flow_filenames <- future_map(stations_with_flow, function(station) {
  flow_img(
    station
  )
})

## Flux bucketed by year ####
variables_concentration <- get_variables(only_concentrations = TRUE)
the_list <- future_map(stations_with_flow, function(station) {
  map(variables_concentration, function(variable) {
    list(station = station, variable = variable)
  })
}) %>% unlist(recursive = FALSE)
flux_filenames <- future_map(the_list, function(the_pair) {
  flux_img( # nolint: object_usage_linter.
    the_pair$station,
    the_pair$variable
  )
})
