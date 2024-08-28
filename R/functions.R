
library(foreach)
library(DBI)
library(duckdb)
library(magrittr)
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(purrr)
library(tidyr)

source(here("constants.R"))

# Utility manipulation functions ####

#' Get a connection to the duckdb database
#' @return A connection to the duckdb database
get_con <- function() {
  con <- dbConnect(
    duckdb(),
    DUCKDB_FILE, # nolint: object_usage_linter
    read_only = TRUE
  )
  # Note: consolidated_data is loaded into the duckdb database
  # at the end of the pipeline.R script
  return(con)
}

#' Get a dataframe from the database
#' @param query The query to run
#' @param params The parameters to pass to the query
#' @return The dataframe from the query
get_df <- function(query, params = list()) {
  con <- get_con()
  the_df <- dbGetQuery(con, query, params)
  dbDisconnect(con)
  return(the_df)
}

#' Replace characters unsuitable for a filename
#' @param words A list of words to replace characters in
#' @return A list of words with characters replaced
get_safe_filename <- function(words) {
  replacements <- list(
    "u/s" = "upstream"
  )
  safe_words <- map(words, function(word) {
    safe_word <- word
    for (key in names(replacements)) {
      safe_word <- gsub(key, replacements[[key]], safe_word)
    }
    return(safe_word)
  })
  return(safe_words)
}

# Data functions ####

#' Set percentile bin
#' @description
#' Set the percentile bin for a dataframe.
#' Originally from Eric Camm.
#' @param the_df The dataframe to add the percentile bin to
#' @param cutoff_year The year to cut off the data
#' @return The dataframe with the percentile bin added
#' @seealso
#' * stats::ecdf()
#' * [percentile_station() in original file](./figure_generation.R)
set_percentile_bin <- function(the_df, cutoff_year) {
  historical_range <- the_df %>%
    dplyr::filter(.data$year != cutoff_year) %>%
    dplyr::filter(is.na(.data$measurement_value) != TRUE)
  if (nrow(historical_range) == 0) {
    # No historical data found
    # Set perc_bin to nan so that it shows up as "Not ranked" when plotted
    the_df <- the_df %>% mutate(perc_bin = NaN)
  } else {
    perc_distribution <- ecdf(historical_range$measurement_value)
    perc_df <- the_df %>%
      group_by(.data$year) %>%
      # comparing median against percentiles, could also use mean here
      summarize(avg = median(.data$measurement_value)) %>%
      mutate(perc_bin = perc_distribution(.data$avg))
    the_df <- left_join(the_df, perc_df, by = "year")
  }
  return(the_df)
}

#' Get flux dataframe
#' @description
#' Calculates the flux for a dataframe.
#' Does not modify the original dataframe.
#' Originally from Eric Camm.
#' ```pseudo
#' concentration (mg/L) x discharge (cms)
#' e.g. for variables other than E.coli,
#' (mg/L * 0.001 L/m3) * (m3/s * 86400 s/d) =
#' (mg/m3) * (m3/d) =
#' (mg/d)
#' ````
#' @param the_df The dataframe to get the flux for
#' @return The dataframe with the flux added
#' @examples
#'
get_flux_df <- function(the_df) {
  # copy and drop NA values for daily flow
  flow_df <- data.frame(the_df) %>% filter(!is.na(.data$daily_flow_cms))
  # warn if NAs were dropped
  if (nrow(the_df) != nrow(flow_df)) {
    warning("NA values were dropped for flux calculation")
  }
  if (nrow(the_df) > 0) {
    # The unit_code must be "mg/L".
    unit_code <- flow_df$unit_code[1]
    if (unit_code != "mg/L") {
      stop("Unit code must be 'mg/L' for flux calculation")
    }
    flow_df <- flow_df %>%
      mutate(
        measurement_value = (.data$measurement_value * 0.001) * (.data$daily_flow_cms * 86400), # nolint: line_length_linter.
        unit_code = "mg/d"
      )
    return(flow_df)
  }
  # leave this as undefined if no data
}

# Data retrieval functions ####

#' Get a list of all of the stations
#' @param with_flow Whether to only get stations with flow data
#' @return A list of all of the stations
get_stations <- function(with_flow = FALSE) {
  query <- "
    select distinct
      WQTC_StationName as station
    from consolidated_data
  "
  if (with_flow) {
    query <- paste(query, "where \"daily.flow.cms\" is not null")
  }
  result_df <- get_df(query)
  return(result_df$station)
}

#' Get variables
#' @description
#' Get a list of all of the variables in the entire dataset.
#' @param only_concentrations:boolean only variables in [m,u,n]g/L
#' @return A list of all of the variables
get_variables <- function(only_concentrations = FALSE) {
  if (only_concentrations) {
    query <- "
      select distinct
        variable_name
      from consolidated_data
      where
        variable_name != 'ESCHERICHIA COLI'
        and unit_code like '%g/L'
      "
  } else {
    query <- "
      select distinct
        variable_name
      from consolidated_data"
  }

  result_df <- get_df(query)
  return(result_df$variable_name)
}

#' Get observation dataframe
#' @description
#' Get the observation dataframe for a station and variable.
#' - Detection condition
#'   - measurement flags '<' and 'L' are considered 'below detection limit'
#'   - measurement flags '>' and 'G' are considered 'above detection limit'
#'   - any other value (or no value) is considered 'detected'
#' - Water season
#'   - 3-season approach (in use)
#'     - high flow: May-July
#'     - open water: from August-October
#'     - under ice: is  November-April
#'   - 2-season approach
#'     - open water: is May-October
#'     - winter: is December-April
#' @param station The station to get the observation dataframe for
#' @param variable The variable to get the observation dataframe for
#' @return The observation dataframe
get_observation_df <- function(station, variable) {
  query <- "
    select
      sample_date::date as sample_date,
      measurement_value,
      case
        when measurement_flag is null then 'detected'
        when measurement_flag = 'L' or measurement_flag = '<'
          then 'below detection limit'
        when measurement_flag = 'G' or measurement_flag = '>'
          then 'above detection limit'
        else 'unknown'
      end as detection_condition,
      date_part('year', sample_date) as year,
      case
        when date_part('month', sample_date) in (5, 6, 7) then 'high flow'
        when date_part('month', sample_date) in (8, 9, 10) then 'open water'
        else 'under ice'
      end as season,
      \"daily.flow.cms\" as daily_flow_cms,
      unit_code
    from consolidated_data
    where
      WQTC_StationName = ?
      and variable_name = ?
    "
  the_df <- get_df(query, list(station, variable))
  return(the_df)
}

#' Get flow dataframe
#' @description
#' Get the flow dataframe for a station.
#' @param station The station to get the flow dataframe for
#' @return The flow dataframe
get_flow_df <- function(station) {
  query <- "
    select distinct
      \"Date\"::date as sample_date,
      \"Value\" as daily_flow_cms
    from daily_flow
    where
      WQTC_StationName = ?
  "
  return(get_df(query, list(station)))
}

#' Get unit
#' @param variable The variable to get the unit for
#' @return The unit for the variable
get_unit <- function(variable) {
  query <- "
    select distinct
      unit_code
    from consolidated_data
    where variable_name = ?
  "
  result_df <- get_df(query, list(variable))
  the_count <- nrow(result_df)
  if (the_count == 0) {
    stop(paste("No unit found for variable", variable))
  } else if (the_count > 1) {
    query <- "
      select
        string_agg(distinct unit_code, ', ') as unit_code
      from consolidated_data
      where variable_name = ?
    "
    units <- get_df(query, list(variable))
    error_message <- paste(
      "Multiple units found for variable ",
      variable,
      ":",
      units$unit_code[1],
      sep = ""
    )
    stop(error_message)
  } else {
    return(result_df$unit_code[1])
  }
}

#' Get min and max date and year for the entire dataset
#' @return A vector with the min and max date and year
get_min_max <- function() {
  con <- get_con()
  query <- "
    select
      date_part('year', min(sample_date)) as min_year,
      date_part('year', max(sample_date)) as max_year,
      min(sample_date)::date as min_date,
      max(sample_date)::date as max_date
    from consolidated_data
  "
  the_df <- dbGetQuery(con, query)
  dbDisconnect(con)
  min_year <- the_df$min_year[1]
  max_year <- the_df$max_year[1]
  return(c(min_year, max_year))
}

# Plotting functions ####

#' Generate an image with boxplots grouped by year
#' @description
#' Generate an image with boxplots grouped by year.
#' Image is saved to the output/figures directory.
#' @param station The station to plot
#' @param variable The variable to plot
#' @param log10 Whether to use log10 scale
#' @return The filename of the generated image
concentration_img <- function(station, variable, log10 = TRUE) {
  captions <- list()
  if (log10) {
    ending <- "log_conc.jpeg"
  } else {
    ending <- "conc.jpeg"
  }
  the_filename <- paste(
    get_safe_filename(c(station, variable, ending)),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    the_filename
  )
  dir.create(dirname(the_filename), recursive = TRUE, showWarnings = FALSE)
  unit <- get_unit(variable)
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  the_df <- get_observation_df(station, variable)

  # if the dataframe is empty, plot should be blank
  if (nrow(the_df) == 0) {
    caption <- "Note: No data found for this station and variable."
    captions <- c(captions, caption)
    the_plot <- the_df %>%
      ggplot() +
      labs(
        y = variable
      )
  } else {
    the_df <- set_percentile_bin(the_df, CUTOFF_YEAR) # nolint: object_usage_linter
    the_df$colour_cut <- cut(the_df$perc_bin, breaks = PERC_BINS) # nolint: object_usage_linter
    the_plot <- the_df %>%
      ggplot(
        aes(
          x = .data$year,
          y = .data$measurement_value,
          group = .data$year,
          fill = .data$colour_cut
        )
      ) +
      geom_boxplot(show.legend = TRUE) +
      scale_fill_manual(
        name = "Percentile",
        values = PERC_COLOURS, # nolint: object_usage_linter
        drop = FALSE, # need this argument to keep unused categories
        na.value = NA_COLOUR, # nolint: object_usage_linter
        labels = PERC_LABELS # nolint: object_usage_linter
      )
    if (log10) {
      # If variable is in NO_LOG_SCALE, plot raw data
      plot_raw <- variable %in% NO_LOG_SCALE # nolint: object_usage_linter
      if (plot_raw) {
        log10 <- FALSE
        caption <- paste(
          "Note: ",
          variable,
          " is intentionally not shown on log10 scale",
          sep = ""
        )
        captions <- c(captions, caption)
      }
    }

    if (log10) {
      the_plot <- the_plot +
        scale_y_log10(
          oob = scales::squish_infinite
        ) +
        labs(
          subtitle = "Water Quality Percentile Comparisons : log10 scale",
          y = paste("log10(", variable, ")", sep = "")
        )
    } else {
      the_plot <- the_plot +
        labs(
          subtitle = "Water Quality Percentile Comparisons : as measured",
          y = paste(variable, " (", unit, ")", sep = "")
        )
    }
  }

  the_plot <- the_plot +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        face = "bold",
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.text.y = element_text(face = "bold")
    ) +
    labs(
      title = station,
      x = NULL # remove x-axis label since years are self-explanatory
    ) +
    scale_x_continuous(
      limits = c(min_year, max_year)
    )

  if (length(captions) > 0) {
    the_plot <- the_plot + labs(caption = paste(captions, collapse = "\n"))
  }

  ggsave(
    filename = the_filename,
    plot = the_plot,
    width = IMAGE_WIDTH, # nolint: object_usage_linter
    height = IMAGE_HEIGHT, # nolint: object_usage_linter
    dpi = IMAGE_DPI # nolint: object_usage_linter
  )

  return(the_filename)
}

#' Generate flow plot
#' @description
#' Generate an image with flow data for a station.
#' Image is saved to the output/figures directory.
#' @param station The station to plot
#' @return The filename of the generated image
flow_img <- function(station) {
  the_filename <- paste(
    get_safe_filename(c(
      station,
      paste0("flow.", IMAGE_DEVICE) # nolint: object_usage_linter.
    )),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    the_filename
  )
  dir.create(dirname(the_filename), recursive = TRUE, showWarnings = FALSE)
  unit <- "cms"
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  the_df <- get_flow_df(station)
  the_plot <- the_df %>%
    ggplot(
      aes(
        x = .data$sample_date,
        y = .data$daily_flow_cms
      )
    ) +
    geom_line(
      color = "blue",
      linewidth = 0.25
    ) +
    scale_x_date(
      date_labels = "%Y",
      limits = c(
        as.Date(paste(min_year, "-01-01", sep = ""), "%Y-%m-%d"),
        as.Date(paste(max_year, "-01-01", sep = ""), "%Y-%m-%d")
      ),
    ) +
    labs(
      title = station,
      subtitle = "Flow Data",
      y = paste("Daily Averaged Flow", " (", unit, ")", sep = ""),
      x = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        face = "bold",
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.text.y = element_text(face = "bold")
    )
  ggsave(
    filename = the_filename,
    plot = the_plot,
    width = IMAGE_WIDTH, # nolint: object_usage_linter
    height = IMAGE_HEIGHT, # nolint: object_usage_linter
    dpi = IMAGE_DPI # nolint: object_usage_linter
  )
  return(the_filename)
}

#' Generate flux plot
#' @description
#' Generate an image with flux data for a station and variable.
#' Image is saved to the output/figures directory.
#' @param station The station to plot
#' @param variable The variable to plot
#' @return The filename of the generated image
flux_img <- function(station, variable) {
  the_filename <- paste(
    get_safe_filename(c(station, variable, "log_flux.jpeg")),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    the_filename
  )
  dir.create(dirname(the_filename), recursive = TRUE, showWarnings = FALSE)
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  the_df <- get_observation_df(station, variable)
  captions <- list()
  if (nrow(the_df) == 0) {
    caption <- "Note: No data found for this station and variable."
    captions <- c(captions, caption)
    the_plot <- the_df %>%
      ggplot() +
      labs(
        y = variable
      )
  } else {
    the_df <- get_flux_df(the_df)
    the_df <- set_percentile_bin(the_df, CUTOFF_YEAR) # nolint: object_usage_linter
    the_df$colour_cut <- cut(the_df$perc_bin, breaks = PERC_BINS) # nolint: object_usage_linter
    the_plot <- the_df %>%
      ggplot(
        aes(
          x = .data$year,
          y = .data$measurement_value,
          group = .data$year,
          fill = .data$colour_cut
        )
      ) +
      geom_boxplot(show.legend = TRUE) +
      scale_fill_manual(
        name = "Percentile",
        values = PERC_COLOURS, # nolint: object_usage_linter
        drop = FALSE, # need this argument to keep unused categories
        na.value = NA_COLOUR, # nolint: object_usage_linter
        labels = PERC_LABELS # nolint: object_usage_linter
      ) +
      scale_y_log10(
        oob = scales::squish_infinite
      ) +
      labs(y = paste("log10(", variable, ")", sep = ""))

    the_plot <- the_plot +
      theme_bw() +
      theme(
        axis.text.x = element_text(
          face = "bold",
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        axis.text.y = element_text(face = "bold")
      ) +
      labs(
        title = station,
        subtitle = "Water Quality Percentile Comparisons : Flux (cms)",
        x = NULL # remove x-axis label since years are self-explanatory
      ) +
      scale_x_continuous(
        limits = c(min_year, max_year)
      )
  }

  if (length(captions) > 0) {
    the_plot <- the_plot + labs(caption = paste(captions, collapse = "\n"))
  }

  ggsave(
    filename = the_filename,
    plot = the_plot,
    width = IMAGE_WIDTH, # nolint: object_usage_linter
    height = IMAGE_HEIGHT, # nolint: object_usage_linter
    dpi = IMAGE_DPI # nolint: object_usage_linter
  )
  return(the_filename)
}

#' Generate scatter plot
#' @description
#' Generate an image with scatter plot data for a station and variable.
#' Image is saved to the output/figures directory.
#' @param station The station to plot
#' @param variable The variable to plot
#' @return The filename of the generated image
scatter_img <- function(station, variable) {
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  the_df <- get_observation_df(station, variable)
  # if any detection_condition is "unknown", stop
  if (any(the_df$season == "unknown")) {
    stop("Unknown detection_condition found in data")
  }
  the_filename <- paste(
    get_safe_filename(c(station, variable, "scatter.jpeg")),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    the_filename
  )
  dir.create(dirname(the_filename), recursive = TRUE, showWarnings = FALSE)
  unit <- get_unit(variable)
  the_plot <- the_df %>%
    ggplot(
      aes(
        x = .data$sample_date,
        y = .data$measurement_value,
        shape = .data$detection_condition,
        colour = as.factor(.data$season),
        fill = .data$season
      )
    ) +
    geom_point(
      size = 2.5,
      alpha = 0.85
    ) +
    scale_color_manual(
      name = "Water Season",
      values = c(
        "high flow" = PERC_COLOURS[3], # nolint: object_usage_linter
        "open water" = PERC_COLOURS[1], # nolint: object_usage_linter
        "under ice" = PERC_COLOURS[5] # nolint: object_usage_linter
      ),
      labels = c("high flow", "open water", "under ice")
    ) +
    scale_fill_manual(
      name = "Water Season",
      values = c(
        "high flow" = PERC_COLOURS[3], # nolint: object_usage_linter
        "open water" = PERC_COLOURS[1], # nolint: object_usage_linter
        "under ice" = PERC_COLOURS[5] # nolint: object_usage_linter
      ),
      labels = c("high flow", "open water", "under ice")
    ) +
    scale_shape_manual(
      name = "Detection Condition",
      values = c(
        "detected" = 16, # solid circle
        "below detection limit" = 6, # triangle down, not filled
        "above detection limit" = 2, # triangle up, not filled
        "unknown" = 4 # cross, not filled
      ),
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        face = "bold",
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.text.y = element_text(face = "bold")
    ) +
    labs(
      title = station,
      subtitle = "Water Quality Percentile Comparisons : Scatter",
      x = NULL, # remove x-axis label since years are self-explanatory
      y = paste(variable, " (", unit, ")", sep = "")
    ) +
    scale_x_date(
      date_labels = "%Y",
      limits = c(
        as.Date(paste(min_year, "-01-01", sep = ""), "%Y-%m-%d"),
        as.Date(paste(max_year, "-01-01", sep = ""), "%Y-%m-%d")
      ),
    )

  ggsave(
    filename = the_filename,
    plot = the_plot,
    width = IMAGE_WIDTH, # nolint: object_usage_linter
    height = IMAGE_HEIGHT, # nolint: object_usage_linter
    dpi = IMAGE_DPI # nolint: object_usage_linter
  )
  return(the_filename)
}
