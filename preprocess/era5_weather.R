box::use(
  load_data = data/`__init__`,
  util/datetime,
  dplyr[...],
  lubridate,
  tidyr
)

pivot <- function(df) {
  return (
    df %>%
      mutate(key = paste(as.character(longitude), as.character(latitude), sep = "_")) %>%
      select(-longitude, -latitude) %>%
      tidyr$pivot_wider(
        id_cols = timestamp,
        names_from = key,
        names_glue = "{.value}_{key}",
        values_from = -all_of(c("timestamp", "key")),
      )
  )
}

#' @export
preprocess <- function(start_date, end_date, df_coords, config) {
  file <- "weather/era5_weather.rds"
  
  start_timestamp <- datetime$local_date_to_utc_start_of_day(start_date)
  end_timestamp <- datetime$local_date_to_utc_end_of_day(end_date)
  
  included_longitude_latitude <- df_coords$longitude * 1000 + df_coords$latitude
  
  df <- load_data$read_rds(file) %>%
    select(timestamp, longitude, latitude, all_of(!!config$weather_variables)) %>%
    filter((longitude * 1000 + latitude) %in% !!included_longitude_latitude) %>%
    filter(timestamp >= !!start_timestamp & timestamp < !!end_timestamp) %>%
    pivot() %>%
    arrange(timestamp)
  
  return(df)
}