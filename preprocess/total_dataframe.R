box::use(
  load_data = data/`__init__`,
  preprocess/calendar,
  preprocess/centroids,
  preprocess/era5_weather,
  preprocess/elia_solar_power,
  preprocess/solar_position,
  preprocess/weather_locations,
  util/datetime,
  util/check_dataframe,
  cpprfast,
  dplyr[...],
  lubridate,
  purrr,
  tidyr
)

most_occuring_element <- function(row) {
  if (all(is.na(row))) {
    return(NA)
  }
  
  freq_table <- table(row)  
  most_common <- names(which.max(freq_table))
  
  return(most_common[1])
}

average_weather_variables_over_included_locations <- function(df, config) {
  weather_variables <- config$weather_variables
  
  col_names <- colnames(df)
  weather_col_names <- col_names[grepl(paste(weather_variables, collapse = "|"), col_names)]
  df_weather_variables <- tibble(
    weather_variable = weather_col_names,
    col_type = purrr$map(df[, weather_col_names], class) %>% purrr$list_c()
  ) %>%
    mutate(weather_variable = gsub("_-?\\d+(\\.\\d+)?_-?\\d+(\\.\\d+)?$", "", weather_variable)) %>%
    distinct()
  
  # add mean of numeric weather variables
  numeric_weather_variables <- (df_weather_variables %>% filter(col_type == "numeric"))$weather_variable
  df[numeric_weather_variables] <- numeric_weather_variables %>%
    purrr$map( ~ {
      mean_selected_weather <- df %>%
        select(starts_with(.x)) %>% 
        rowMeans(., na.rm = TRUE)
      
      return(mean_selected_weather)
    })
  
  # add most occurring string for character weather variables
  character_weather_variables <- (df_weather_variables %>% filter(col_type == "character"))$weather_variable
  df[character_weather_variables] <- character_weather_variables %>%
    purrr$map( ~ {
      weather_var <- .x
      
      df_selected <- df %>%
        select(starts_with(weather_var))

      most_occurring_character_selected_weather <- c(1:NROW(df_selected)) %>%
        purrr::map_chr(~ {
          row_number <- .x
          
          return(most_occuring_element(t(df_selected[row_number, ])))
        })

      return(most_occurring_character_selected_weather)
    })
  
  # remove original weather variables at different locations
  df <- df %>%
    select(-all_of(weather_col_names))

  return(df)
}

#' @export
preprocess <- function(start_date, end_date, config) {
  first_timestamp <- datetime$local_date_to_utc_start_of_day(start_date)
  last_timestamp <- datetime$local_date_to_utc_end_of_day(end_date) - lubridate$hours(1)
  
  df_timestamp <- tibble(
    timestamp = seq(first_timestamp, last_timestamp, by = "hour")
  )
  

  df_load_factor <- elia_solar_power$preprocess(
    start_date = start_date,
    end_date = end_date,
    config = config
  )
    
  df_included_coords_belgium <- load_data$read_rds("weather/df_weather_locations.rds") %>%
    select(longitude, latitude)
  n_included_coords_belgium <- NROW(df_included_coords_belgium)

  if (is.null(config$n_cluster) | (n_included_coords_belgium == config$n_cluster)) {
    df_selected_coord <- df_included_coords_belgium
  } else if (n_included_coords_belgium < config$n_cluster) {
    stop("More cluster selected than included coordinates")
  } else {
    weighted_kmeans <- cpprfast$weighted_kmeans(
      df = df_included_coords_belgium,
      n_cluster = config$n_cluster,
      n_start = 5,
      max_iter = 1000,
      weight_col_name = NULL, #  add name of column with weights in case of weighted kmeans
      seed = 10
    )
    
    df_selected_coord <- centroids$determine_centroids(
      weighted_kmeans = weighted_kmeans,
      grid_resolution = config$grid_resolution
    )
  }
    
  df_weather <- era5_weather$preprocess(
    start_date = start_date, 
    end_date = end_date,
    df_coords = df_selected_coord, 
    config = config
  )
  
  if (config$use_average_over_clusters) {
    df_weather <- average_weather_variables_over_included_locations(df_weather, config)
  }
    
  df <- df_timestamp %>%
    left_join(df_load_factor, by = "timestamp") %>%
    left_join(df_weather, by = "timestamp") %>%
    calendar$add_calendar_data(config = config) %>%
    solar_position$add_solar_position(
      df_coord = df_included_coords_belgium %>%
        summarise(across(everything(), mean)),
      include = config$include_solar_position
    )
  
  if (anyNA(df)) {
    check_dataframe$check_na(df)
  }
  
  return(df)
}


