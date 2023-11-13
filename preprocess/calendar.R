box::use(
  dplyr[...],
  lubridate
)

relative_day_during_year <- function(date) {
  day_during_year <- lubridate$yday(date)
  years <- lubridate$year(date)
  n_days_in_year <- if_else(lubridate$leap_year(years), 366, 365)
  
  return(day_during_year / n_days_in_year)
}

#' @export
add_calendar_data <- function(df, config) {
  calendar_vars <- config$calendar_vars
  if (is.null(calendar_vars)) {
    return(df)
  }
  
  included_vars <- union(colnames(df), calendar_vars) %>% unique()
  
  df <- df %>%
    mutate(
      local_timestamp = lubridate$with_tz(as.POSIXct(timestamp, tz = "UTC"), tzone = "Europe/Brussels"),
      date = lubridate$date(local_timestamp),
      hour = lubridate$hour(local_timestamp),
      weekday = lubridate$wday(local_timestamp, week_start = getOption("lubridate.week.start", 1)),
      relative_day_during_year = relative_day_during_year(date),
      month = lubridate$month(local_timestamp), 
      week = lubridate$week(local_timestamp),
      year = lubridate$year(local_timestamp)
    ) %>%
    select(all_of(!!included_vars))
  
  return(df)
}