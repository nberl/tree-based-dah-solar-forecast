box::use(
  util/datetime,
  dplyr[...],
  glue,
  lubridate,
  purrr,
  solarPos
)

timestamps_to_julian_days <- function(timestamps) {
  n <- length(timestamps)
  years <- lubridate$year(timestamps)
  months <- lubridate$month(timestamps)
  days <- lubridate$day(timestamps)
  hours <- lubridate$hour(timestamps)

  julian_days <- vector(mode = "numeric", length = n)

  for (i in seq_len(n)) {
    julian_days[i] <- solarPos$julianDay(years[i], months[i], days[i], hours[i], 0, 0, 0)
  }

  return(julian_days)
}

get_df_solar_position <- function(timestamps, df_coord) {
  julian_days <- timestamps_to_julian_days(timestamps)

  df_solar_position <- NROW(df_coord) %>%
    purrr$map_dfc(
      ~ {
        longitude <- round(df_coord[.x, ]$longitude, 2)
        latitude <- round(df_coord[.x, ]$latitude, 2)
        df <- solarPos$solarPosition(
          jd = julian_days,
          lon = longitude,
          lat = latitude,
          delta_t = 0,
          elev = 0,
          temp = 16,
          pres = 1013.25
        ) %>%
          as_tibble() %>%
          rename_with(~ glue$glue("{.}_{longitude}_{latitude}"), everything())

        return(df)
      }
    ) %>%
    mutate(timestamp = !!timestamps)

  return(df_solar_position)
}

#' @export
add_solar_position <- function(df, df_coord, include = TRUE) {
  if (is.null(include) || !include) {
    return(df)
  }

  df_solar_position <- get_df_solar_position(
    timestamps = df$timestamp %>% unique,
    df_coord = df_coord
  )

  return(
    df %>%
      left_join(df_solar_position, by = "timestamp")
  )
}
