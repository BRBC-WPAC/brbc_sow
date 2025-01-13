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
show_warnings <- FALSE

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

facet_title <- function(variable_names, unit_codes) {
  only_variable_name <- c("PH", "SODIUM ADSORPTION RATIO (CALCD.)")
  facet_titles <- sapply(
    seq_along(variable_names),
    function(i) {
      if (variable_names[i] %in% only_variable_name) {
        return(variable_names[i])
      } else {
        return(paste(variable_names[i], " (", unit_codes[i], ")", sep = ""))
      }
    },
    simplify = TRUE,
    USE.NAMES = FALSE
  )
  return(facet_titles)
}

apply_scale_y_log10 <- function(...) {
  scale_y_continuous(
    ...,
    transform = scales::log10_trans()
  )
}

# Data functions ####

#' Percentile binning function
#' @description
#' Bins the measurement values into percentiles.
#' @param measurement_values The measurement values to bin
#' @param years The years to bin
#' @param cutoff_year The year to cut off the data
#' @return The percentile bin
perc_bin_f <- function(
  measurement_values,
  years,
  cutoff_year = CUTOFF_YEAR # nolint: object_usage_linter.
) {
  historical_range <- measurement_values[
    years < cutoff_year &
      !is.na(measurement_values)
  ]
  if (sum(historical_range) == 0) {
    return(NaN)
  }
  perc_distribution <- ecdf(historical_range)
  a_df <- data.frame(
    measurement_value = measurement_values,
    year = years
  ) %>%
    group_by(.data$year) %>%
    mutate(
      perc_bin = perc_distribution(
        median(.data$measurement_value, na.rm = TRUE)
      )
    ) %>%
    # make sure that any perc_bin values that are 0 are set to 0.01
    mutate(perc_bin = ifelse(.data$perc_bin == 0, 0.01, .data$perc_bin)) %>%
    ungroup()
  return(a_df$perc_bin)
}

get_flux_unit <- function(unit_code) {
  flux_unit <- ifelse(
    unit_code == "No/100 mL",
    "No/s",
    "mg/s"
  )
  return(flux_unit)
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
get_flux_df <- function(the_df) {
  flux_compatible_units <- c("mg/L", "No/100 mL")
  known_incompatible_units <- c("pH units", "µS/cm", "rel units")
  # copy and drop NA values for daily flow
  flow_df <- data.frame(the_df) %>%
    # check for flow data
    filter(!is.na(.data$daily_flow_cms))
  # warn if NAs were dropped
  if (show_warnings && nrow(the_df) != nrow(flow_df)) {
    warning("Values were dropped for flux calculation because of NA flow data")
  }
  last_count <- nrow(flow_df)
  flow_df <- flow_df %>%
    # check unit_code
    filter(.data$unit_code %in% flux_compatible_units)
  if (last_count != nrow(flow_df)) {
    # are all of the unit_code values in the known incompatible units?
    bad_unit <- unique(
      flow_df$unit_code[!flow_df$unit_code %in% flux_compatible_units]
    )
    if (length(bad_unit) > 0  && all(bad_unit %in% known_incompatible_units)) {
      if (show_warnings) {
        warning(
          paste(
            "Unit code",
            bad_unit,
            "is not compatible with flux calculation"
          )
        )
      }
      return(NULL)
    }
  }

  if (nrow(flow_df) > 0) {
    flow_df <- flow_df %>%
      mutate(
        conc = .data$measurement_value,
        conc_unit = .data$unit_code
      ) %>%
      mutate(
        volume_conversion_factor = ifelse(
          .data$conc_unit == "No/100 mL",
          10,
          1
        ),
        unit_code = get_flux_unit(.data$unit_code)
      ) %>%
      mutate(
        measurement_value = (
          # convert to x/L
          (.data$measurement_value * .data$volume_conversion_factor)

          # convert from x/L to x/m3
          * 1000

          # multiply by daily flow (m3/s) to get flux in (x/s)
          * .data$daily_flow_cms
        ),
        conc = .data$measurement_value
      )
    return(flow_df)
  }
  if (show_warnings) {
    warning("No data found for flux calculation")
  }
  return(NULL)
}

# Data retrieval functions ####

#' Get a list of all of the stations
#' @param with_flow Whether to only get stations with flow data
#' @return A list of all of the stations
get_stations <- function(with_flow = FALSE) {
  if (with_flow) {
    query <- "
      select distinct
        f.WQTC_StationName as station
      from daily_flow f
      join consolidated_data c on f.WQTC_StationName = c.WQTC_StationName
    "
  } else {
    query <- "
      select distinct
        WQTC_StationName as station
      from consolidated_data
      where WQTC_StationName is not null
    "
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
        variable_name = 'ESCHERICHIA COLI'
        or unit_code like '%g/L'
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
get_observation_df <- function(station, variable = NULL) {
  if (!is.null(variable)) {
    variable_criteria <- "and variable_name = ?"
    params <- list(station, variable)
  } else {
    variable_criteria <- ""
    params <- list(station)
  }
  query <- paste(
    "
    select
      sample_date::date as sample_date,
      case
        when
          measurement_flag2 is null or measurement_flag2 = FALSE
          then measurement_value
        else measurement_maxDLsub
      end as measurement_value,

      case
        when
          measurement_flag2 is null or measurement_flag2 = FALSE
          then
            case
              when measurement_flag is null then 'detected'
              when measurement_flag = 'L' or measurement_flag = '<'
                then 'below detection limit'
              when measurement_flag = 'G' or measurement_flag = '>'
                then 'above detection limit'
              else 'unknown'
            end
        else 'below detection limit'
      end as detection_condition,
      date_part('year', sample_date) as year,
      case
        when date_part('month', sample_date) in (5, 6, 7) then 'high flow'
        when date_part('month', sample_date) in (8, 9, 10) then 'open water'
        else 'under ice'
      end as season,
      \"daily.flow.cms\" as daily_flow_cms,
      unit_code,
      variable_name
    from consolidated_data
    where
      WQTC_StationName = ?",
    variable_criteria,
    "
      and
      case
        when
          measurement_flag2 is null or measurement_flag2 = FALSE
          then measurement_value
        else measurement_maxDLsub
      end is not null
    order by
      variable_name,
      sample_date
    "
  )
  the_df <- get_df(query, params)
  # year as int
  the_df$year <- as.integer(the_df$year)
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

#' Get guidelines
#' @param variable The variable to get the guidelines for
#' @return The guidelines for the variable
get_guidelines <- function(variable, unit_code) {
  query <- "
    select
      source,
      variable_name,
      lower_value,
      upper_value,
      unit_code,
      exposure_duration
    from guideline
    where variable_name = ?
    and unit_code = ?
  "
  result_df <- get_df(query, list(variable, unit_code))
  the_count <- nrow(result_df)
  if (the_count == 0) {
    query <- "select count(*) from guideline where variable_name = ?"
    if (get_df(query, list(variable))$count[1] != 0) {
      stop(
        paste0(
          "Incorrect unit_code supplied for guideline variable ",
          variable,
          ": ",
          unit_code,
          "instead of ",
          result_df$unit_code[1]
        )
      )
    } # It's OK if there are no guidelines for this variable
  } else {
    return(result_df)
  }
}

# Plotting functions ####

add_guidelines <- function(
    captions,
    the_plot,
    the_df,
    variable,
    unit,
    display_unit) {
  guideline_df <- get_guidelines(variable, unit)
  if (!is.null(guideline_df)) {
    for (i in 1:nrow(guideline_df)) {
      guideline <- guideline_df[i, ]
      # If both upper and lower, plot as area
      if (!is.na(guideline$lower_value) && !is.na(guideline$upper_value)) {
        guideline_text <- paste0(
          guideline$lower_value,
          " - ",
          guideline$upper_value,
          " ",
          display_unit
        )
        the_plot <- the_plot +
          geom_rect(
            aes(
              xmin = -Inf,
              xmax = Inf,
              ymin = guideline$upper_value,
              ymax = Inf
            ),
            fill = "red",
            alpha = 0.2,
            color = "red",
            linetype = "dashed",
            data = data.frame(),
            inherit.aes = FALSE
          )
        the_plot <- the_plot +
          geom_rect(
            aes(
              xmin = -Inf,
              xmax = Inf,
              ymin = -Inf,
              ymax = guideline$lower_value
            ),
            fill = "red",
            alpha = 0.2,
            color = "red",
            linetype = "dashed",
            data = data.frame(),
            inherit.aes = FALSE
          )
      } else {
        if (!is.na(guideline$lower_value)) {
          guideline_text <- paste0(
            ">",
            guideline$lower_value,
            " ",
            display_unit
          )
          the_plot <- the_plot +
            geom_hline(
              yintercept = guideline$lower_value,
              linetype = "dashed",
              color = "red"
            )
        } else {
          guideline_text <- paste0(
            "<",
            guideline$upper_value,
            " ",
            display_unit
          )
          the_plot <- the_plot +
            geom_hline(
              yintercept = guideline$upper_value,
              linetype = "dashed",
              color = "red"
            )
        }
      }

      source <- guideline$source
      exposure_duration <- guideline$exposure_duration
      if (!is.na(exposure_duration)) {
        exposure_duration <- paste0(" ", exposure_duration, " guideline: ")
      } else {
        exposure_duration <- " guideline: "
      }
      captions <- c(
        captions,
        paste0(
          source,
          exposure_duration,
          guideline_text
        )
      )
    }
  }

  results <- list()
  results$the_plot <- the_plot
  results$captions <- captions
  return(results)
}

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
    ending <- paste0("log_conc.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter
  } else {
    ending <- paste0("conc.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter
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
  if (unit == "pH units") {
    display_unit <- ""
  } else {
    display_unit <- unit
  }
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
    # the_df <- set_percentile_bin(the_df, CUTOFF_YEAR) # nolint: object_usage_linter
    the_df <- the_df %>%
      mutate(
        perc_bin = perc_bin_f(
          .data$measurement_value,
          .data$year
        )
      )
    the_df$colour_cut <- cut(the_df$perc_bin, breaks = PERC_BINS) # nolint: object_usage_linter
    the_plot <- the_df %>%
      ggplot(
        aes(
          x = .data$year,
          y = .data$measurement_value,
          group = .data$year,
          fill = .data$colour_cut
        )
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

      results <- add_guidelines(
        captions,
        the_plot,
        the_df,
        variable,
        unit,
        display_unit
      )
      the_plot <- results$the_plot
      captions <- results$captions
    }

    the_plot <- the_plot +
      geom_boxplot(show.legend = TRUE) +
      scale_fill_manual(
        name = "Percentile",
        values = PERC_COLOURS, # nolint: object_usage_linter
        drop = FALSE, # need this argument to keep unused categories
        na.value = NA_COLOUR, # nolint: object_usage_linter
        labels = PERC_LABELS # nolint: object_usage_linter
      )


    subtitle <- "Water Quality Percentile Comparisons : Concentration"
    if (display_unit == "") {
      unit_label <- ""
    } else {
      unit_label <- paste0(" (", display_unit, ")")
    }

    y_axis_label <- paste0(variable, unit_label)
    if (log10) {
      the_plot <- the_plot +
        apply_scale_y_log10(
          oob = scales::oob_squish_infinite,
          labels = scales::comma
        )
      captions <- c(captions, "Note: y-axis is on log10 scale")
    }
    the_plot <- the_plot +
      labs(
        subtitle = subtitle,
        y = y_axis_label
      )
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
      paste0("flow.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter.
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

  # for any missing days in the_df, put in a row with NA for measurement_value
  the_df <- the_df %>%
    tidyr::complete(
      sample_date = seq.Date(
        min(the_df$sample_date),
        max(the_df$sample_date),
        by = "day"
      )
    )
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
#' @param log10 Whether to use log10 scale
#' @return The filename of the generated image
flux_img <- function(station, variable, log10 = TRUE) {
  captions <- list(STANDARD_FOOTNOTE) # nolint: object_usage_linter
  label <- NULL
  unit <- get_unit(variable)
  flux_unit <- get_flux_unit(unit)
  y_axis_label <- paste(variable, " (", flux_unit, ")", sep = "")
  if (log10) {
    y_axis_label <- paste(y_axis_label, "log10 scale", sep = "\n")
    ending <- paste0("log_flux.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter
  } else {
    ending <- paste0("flux.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter
  }
  the_filename <- paste(
    get_safe_filename(c(station, variable, ending)),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    "individual",
    the_filename
  )
  dir.create(
    dirname(the_filename),
    recursive = TRUE,
    showWarnings = FALSE
  )
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  the_df <- get_observation_df(station, variable)
  if (nrow(the_df) == 0) {
    caption <- "No data found for this station and variable."
    captions <- c(captions, caption)
    the_plot <- the_df %>%
      ggplot() +
      labs(
        y = variable
      )
    label <- WATER_QUALITY_NOT_AVAILABLE # nolint: object_usage_linter
  } else {
    the_df <- get_flux_df(the_df)
    if (is.null(the_df)) {
      caption <- "No flow data for this station matches this variable's sample dates." # nolint: line_length_linter.
      captions <- c(captions, caption)
      the_plot <- the_df %>%
        ggplot() +
        labs(
          y = variable
        )
      label <- WATER_QUALITY_NOT_AVAILABLE # nolint: object_usage_linter
    } else {
      the_df <- the_df %>%
        mutate(
          perc_bin = perc_bin_f(
            .data$measurement_value,
            .data$year
          )
        )
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
        geom_boxplot(show.legend = FALSE) +
        geom_point(
          aes(
            fill = .data$colour_cut
          ),
          shape = 22,
          alpha = 0,
          show.legend = TRUE
        ) +
        scale_fill_manual(
          name = "Percentile",
          values = PERC_COLOURS, # nolint: object_usage_linter
          drop = FALSE, # need this argument to keep unused categories
          na.value = NA_COLOUR, # nolint: object_usage_linter
          labels = PERC_LABELS # nolint: object_usage_linter
        ) +
        guides(
          fill = guide_legend(
            title = "Percentile",
            override.aes = list(
              alpha = 1,
              size = 6,
              linewidth = 1
            ),
            order = 1
          )
        )
      if (log10) {
        the_plot <- the_plot +
          apply_scale_y_log10(
            oob = scales::oob_squish_infinite,
            labels = scales::comma
          )
      }
    }
  }

  subtitle <- "Water Quality Percentile Comparisons : Flux"

  the_plot <- the_plot +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        face = "bold",
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.text.y = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0)
    ) +
    labs(
      title = station,
      subtitle = subtitle,
      x = NULL, # remove x-axis label since years are self-explanatory
      y = y_axis_label
    ) +
    scale_x_continuous(
      limits = c(min_year, max_year)
    )

  if (length(captions) == 1) {
    captions <- c(captions, " ")
  }
  the_plot <- the_plot + labs(caption = paste(captions, collapse = "\n"))

  if (!is.null(label)) {
    the_plot <- the_plot +
      geom_label(
        aes(
          x = .data$year,
          y = .data$measurement_value,
          label = .data$text
        ),
        hjust = "middle",
        data = data.frame(
          measurement_value = 1,
          year = 2011,
          text = label
        )
      )
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
    get_safe_filename(c(station, variable, paste0("scatter.", tolower(IMAGE_DEVICE)))), # nolint: object_usage_linter
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

#' Generate mega plot
#' @description
#' Generate an image with scatter plot data for a station and variable.
#' Image is saved to the output/figures directory.
#' @param station The station to plot
#' @return The filename of the generated image
faceted_concentration_img <- function(station) {
  captions <- list()
  ending <- paste0("log_conc.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter
  the_filename <- paste(
    get_safe_filename(c(station, ending)),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    the_filename
  )
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  min_date <- as.Date(paste(min_year, "-01-01", sep = ""), "%Y-%m-%d")
  max_date <- as.Date(paste(max_year, "-01-01", sep = ""), "%Y-%m-%d")
  dir.create(dirname(the_filename), recursive = TRUE, showWarnings = FALSE)

  the_df <- get_observation_df(station)
  # if the dataframe is empty, plot should be blank
  if (nrow(the_df) == 0) {
    caption <- "Note: No data found for this station and variable."
    captions <- c(captions, caption)
    the_plot <- the_df %>%
      ggplot() +
      facet_grid(
        . ~ variable_name
      )
  } else {
    the_df <- the_df %>%
      group_by(.data$variable_name) %>%
      mutate(
        facet_title = facet_title(.data$variable_name, .data$unit_code),
        perc_bin = perc_bin_f(.data$measurement_value, .data$year)
      ) %>%
      ungroup()

    the_df$colour_cut <- cut(the_df$perc_bin, breaks = PERC_BINS) # nolint: object_usage_linter

    query <- "
      select
        source,
        variable_name,
        lower_value,
        upper_value,
        unit_code,
        exposure_duration
      from guideline
    "
    guideline_df <- get_df(query) %>%
      group_by(.data$variable_name) %>%
      mutate(
        facet_title = facet_title(.data$variable_name, .data$unit_code)
      ) %>%
      ungroup()

    line_df <- data.frame()
    for (i in 1:nrow(guideline_df)) {
      guideline <- guideline_df[i, ]
      if (!is.na(guideline$upper_value)) {
        line_df <- rbind(
          line_df,
          data.frame(
            yintercept = c(guideline$upper_value),
            facet_title = c(
              facet_title(
                guideline$variable_name,
                guideline$unit_code
              )
            ),
            bound = c("lower"),
            exposure_duration = c(guideline$exposure_duration)
          )
        )
      }
      if (!is.na(guideline$lower_value)) {
        line_df <- rbind(
          line_df,
          data.frame(
            yintercept = c(guideline$lower_value),
            facet_title = c(
              facet_title(
                guideline$variable_name,
                guideline$unit_code
              )
            ),
            bound = c("upper"),
            exposure_duration = c(guideline$exposure_duration)
          )
        )
      }
    }


    flow_df <- get_flow_df(station) %>%
      # for any missing days in the_df, put in a row with NA for measurement_value
      tidyr::complete(
        sample_date = seq.Date(
          min(the_df$sample_date),
          max(the_df$sample_date),
          by = "day"
        )
      ) %>%
      mutate(
        facet_title = "Flow (cms)",
        year = as.integer(format(as.Date(.data$sample_date), "%Y"))
      )

    the_df <- the_df %>%
      mutate(
        sample_date = as.Date(paste0(.data$year, "-01-01"), "%Y-%m-%d")
      )



    the_plot <- the_df %>%
      ggplot(
        aes(
          x = .data$sample_date,
          y = .data$measurement_value
        )
      ) +
      geom_boxplot(
        aes(
          fill = .data$colour_cut,
          group = .data$year
        ),
        show.legend = FALSE,
        # width = 0.5 / max_year - min_year,
        position = position_nudge(x = -0.5),
        outlier.size = 1 / 5,
        linewidth = 1 / 5
      ) +
      geom_point(
        aes(
          fill = .data$colour_cut
        ),
        shape = 22,
        alpha = 0,
        show.legend = TRUE
      ) +
      scale_fill_manual(
        values = PERC_COLOURS, # nolint: object_usage_linter
        na.value = NA_COLOUR, # nolint: object_usage_linter
        drop = FALSE, # need this argument to keep unused categories
        labels = PERC_LABELS # nolint: object_usage_linter
      ) +
      scale_linetype_manual(
        values = c("solid", "dashed"), # nolint: object_usage_linter
        labels = c("acute", "chronic") # nolint: object_usage_linter
      ) +
      scale_color_manual(
        values = c("red", "blue"), # nolint: object_usage_linter
        labels = c("upper", "lower") # nolint: object_usage_linter
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
      ) +
      scale_y_log10(
        oob = scales::squish_infinite,
        labels = scales::comma
      ) +
      geom_hline(
        aes(
          yintercept = .data$yintercept,
          linetype = .data$exposure_duration,
          color = .data$bound
        ),
        data = line_df
      ) +
      geom_line(
        aes(
          x = .data$sample_date,
          y = .data$daily_flow_cms
        ),
        # dark grey
        color = "#333333",
        linewidth = 1 / 12,
        data = flow_df
      ) +
      facet_wrap(
        ~facet_title,
        scales = "free_y"
      ) +
      labs(
        title = station,
        subtitle = "Water Quality Percentile Comparisons",
        y = "log10 scale",
        x = ""
      ) +
      scale_x_date(
        limits = c(min_date, max_date),
        breaks = seq.Date(as.Date("2000-01-01"), max_date, by = "5 year"),
        date_labels = "%Y"
      ) +
      guides(
        fill = guide_legend(
          title = "Percentile",
          override.aes = list(
            alpha = 1,
            size = 6,
            linewidth = 1
          ),
          order = 1
        ),
        linetype = guide_legend(
          title = "Guideline",
          order = 2
        ),
        color = guide_legend(
          title = "Bound",
          order = 3
        )
      ),
      color = "none"
    ) +
    apply_scale_y_log10(
      oob = scales::oob_squish_infinite,
      labels = scales::comma
    ) +
  }

  ggsave(
    filename = the_filename,
    plot = the_plot,
    width = IMAGE_WIDTH * 1.3, # nolint: object_usage_linter
    height = IMAGE_HEIGHT * 1.2, # nolint: object_usage_linter
    dpi = IMAGE_DPI # nolint: object_usage_linter
  )
  return(the_filename)
}

faceted_flux_img <- function(station) {
  captions <- list()
  ending <- paste0("log_flux.", tolower(IMAGE_DEVICE)) # nolint: object_usage_linter
  the_filename <- paste(
    get_safe_filename(c(station, ending)),
    collapse = "_"
  )
  the_filename <- here(
    "output",
    "figures",
    the_filename
  )
  min_max <- get_min_max()
  min_year <- min_max[1] - 1
  max_year <- min_max[2] + 1
  min_date <- as.Date(paste(min_year, "-01-01", sep = ""), "%Y-%m-%d")
  max_date <- as.Date(paste(max_year, "-01-01", sep = ""), "%Y-%m-%d")
  dir.create(dirname(the_filename), recursive = TRUE, showWarnings = FALSE)

  the_df <- get_observation_df(station)
  the_df <- get_flux_df(the_df)
  # is the_df null?
  if (!is.null(the_df)) {
    the_df <- the_df %>%
      filter(!is.na(.data$measurement_value))
  }

  # if the dataframe is empty, plot should be blank
  if (is.null(the_df) || nrow(the_df) == 0) {
    print(paste("No data (paired with daily flow) found for ", station))
    return(NULL)
  }
  the_df <- the_df %>%
    group_by(.data$variable_name) %>%
    mutate(
      facet_title = facet_title(.data$variable_name, .data$unit_code),
      perc_bin = perc_bin_f(.data$measurement_value, .data$year)
    ) %>%
    ungroup()

  the_df$colour_cut <- cut(the_df$perc_bin, breaks = PERC_BINS) # nolint: object_usage_linter

  flow_df <- get_flow_df(station) %>%
    # for any missing days in the_df, put in a row with NA for measurement_value
    tidyr::complete(
      sample_date = seq.Date(
        min(the_df$sample_date),
        max(the_df$sample_date),
        by = "day"
      )
    ) %>%
    mutate(
      facet_title = "Flow (cms)",
      year = as.integer(format(as.Date(.data$sample_date), "%Y"))
    )

  the_df <- the_df %>%
    mutate(
      sample_date = as.Date(paste0(.data$year, "-01-01"), "%Y-%m-%d")
    )

  the_plot <- the_df %>%
    ggplot(
      aes(
        x = .data$sample_date,
        y = .data$measurement_value
      )
    ) +
    geom_boxplot(
      aes(
        fill = .data$colour_cut,
        group = .data$year
      ),
      show.legend = FALSE,
      # width = 0.5 / max_year - min_year,
      position = position_nudge(x = -0.5),
      outlier.size = 1 / 5,
      linewidth = 1 / 5
    ) +
    geom_point(
      aes(
        fill = .data$colour_cut
      ),
      shape = 22,
      alpha = 0,
      show.legend = TRUE
    ) +
    scale_fill_manual(
      values = PERC_COLOURS, # nolint: object_usage_linter
      na.value = NA_COLOUR, # nolint: object_usage_linter
      drop = FALSE, # need this argument to keep unused categories
      labels = PERC_LABELS # nolint: object_usage_linter
    ) +
    scale_linetype_manual(
      values = c("solid", "dashed"), # nolint: object_usage_linter
      labels = c("acute", "chronic") # nolint: object_usage_linter
    ) +
    scale_color_manual(
      values = c("red", "blue"), # nolint: object_usage_linter
      labels = c("upper", "lower") # nolint: object_usage_linter
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
    ) +
    scale_y_log10(
      oob = scales::squish_infinite,
      labels = scales::comma
    ) +
    geom_line(
      aes(
        x = .data$sample_date,
        y = .data$daily_flow_cms
      ),
      # dark grey
      color = "#333333",
      linewidth = 1 / 12,
      data = flow_df
    ) +
    facet_wrap(
      ~facet_title,
      scales = "free_y"
    ) +
    labs(
      title = station,
      subtitle = "Water Quality Percentile Comparisons : Flux",
      y = "log10 scale",
      x = ""
    ) +
    scale_x_date(
      limits = c(min_date, max_date),
      breaks = seq.Date(as.Date("2000-01-01"), max_date, by = "5 year"),
      date_labels = "%Y"
    ) +
    guides(
      fill = guide_legend(
        title = "Percentile",
        override.aes = list(
          alpha = 1,
          size = 6,
          linewidth = 1
        ),
        order = 1
      )
    ) +
    apply_scale_y_log10(
      oob = scales::oob_squish_infinite,
      labels = scales::comma
    )

  ggsave(
    filename = the_filename,
    plot = the_plot,
    width = IMAGE_WIDTH * 1.3, # nolint: object_usage_linter
    height = IMAGE_HEIGHT * 1.2, # nolint: object_usage_linter
    dpi = IMAGE_DPI # nolint: object_usage_linter
  )
  return(the_filename)
}