box::use(
  load_data = data/`__init__`,
  util/datetime,
  dplyr[...],
  lubridate,
  tidyr,
  zoo
)

interpolate_monitored_capacity <- function(df) {
  df <- df %>%
    mutate(
      elia_monitored_capacity_lag1 = lag(elia_monitored_capacity, 1),
      is_no_change = (elia_monitored_capacity == elia_monitored_capacity_lag1) & !is.na(elia_monitored_capacity_lag1),
      elia_interpolated_monitored_capacity = if_else(is_no_change, NA, elia_monitored_capacity),
      elia_interpolated_monitored_capacity = zoo$na.approx(elia_interpolated_monitored_capacity, na.rm = FALSE),
    ) %>%
    tidyr$fill(elia_interpolated_monitored_capacity) %>%
    select(-elia_monitored_capacity_lag1, is_no_change)
  
  return(df)
}

aggregate_by_hour <- function(df) {
  df <- df %>%
    mutate(timestamp = lubridate$floor_date(timestamp, unit = "hours")) %>%
    group_by(timestamp) %>%
    summarise(
      elia_act_generation_solar_belgium = mean(elia_act_generation_solar_belgium, na.rm = TRUE),
      elia_monitored_capacity = mean(elia_monitored_capacity, na.rm = TRUE),
      elia_interpolated_monitored_capacity = mean(elia_interpolated_monitored_capacity, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df)
}

add_load_factor <- function(df) {
  df <- df %>%
    mutate(
      load_factor = elia_act_generation_solar_belgium / elia_monitored_capacity,
      load_factor_interpolated = elia_act_generation_solar_belgium / elia_interpolated_monitored_capacity
    )
}

#' @export
preprocess <- function(start_date, end_date, df_coords, config) {
  file <- "solar_power/elia_solar_power.rds"
  
  start_timestamp <- datetime$local_date_to_utc_start_of_day(start_date)
  end_timestamp <- datetime$local_date_to_utc_end_of_day(end_date)
  
  df <- load_data$read_rds(file) %>%
    arrange(timestamp) %>%
    interpolate_monitored_capacity %>%
    filter(
      timestamp >= !!start_timestamp,
      timestamp < !!end_timestamp
    ) %>%
    aggregate_by_hour %>%
    add_load_factor %>%
    arrange(timestamp)
  
  return(df)
}