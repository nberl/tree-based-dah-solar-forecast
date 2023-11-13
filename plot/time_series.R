box::use(
  dplyr[...],
  ggplot2[...],
  lubridate,
  tidyr
)

#' @export
plot_solar_power_time_series <- function(df_solar_power, start_timestamp, end_timestamp) {
  if (!("local_timestamp" %in% colnames(df_solar_power))) {
    df_solar_power <- df_solar_power %>%
      mutate(
        local_timestamp = lubridate$with_tz(as.POSIXct(timestamp, tz = "UTC"), tzone = "Europe/Brussels")
      )
  }
  
  p <- df_solar_power %>% 
    filter(between(local_timestamp, !!start_timestamp, !!end_timestamp)) %>%
    ggplot() +
    geom_line(
      aes(
        x = local_timestamp, 
        y = elia_act_generation_solar_belgium
      ), 
      color = "#F8766D"
    ) +
    ylab("MW") +
    theme_bw() +
    xlab("Local timestamp") +
    theme(
      plot.title = element_text(size = 14.3, hjust = 0.5),
      axis.title.x = element_text(size = 13.3),
      axis.title.y = element_text(size = 13.3),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11)
    )

  return(p)
}

#' @export
plot_solar_power_time_series_over_selected_periods <- function(df_solar_power, save = FALSE) {
  folder <- "plot/figures/"
  plot_names <- c(
    "solar_power_time_series_01.pdf", 
    "solar_power_time_series_02.pdf", 
    "solar_power_time_series_03.pdf", 
    "solar_power_time_series_04.pdf"
  )
  start_timestamps <- c(
    as.POSIXct("2019-01-01 00:00", tz = "Europe/Brussels"),
    as.POSIXct("2021-01-01 00:00", tz = "Europe/Brussels"),
    as.POSIXct("2021-05-03 00:00", tz = "Europe/Brussels"),
    as.POSIXct("2021-05-07 00:00", tz = "Europe/Brussels")
  )
  end_timestamps <- c(
    as.POSIXct("2023-06-30 23:00", tz = "Europe/Brussels"),
    as.POSIXct("2021-12-31 23:00", tz = "Europe/Brussels"),
    as.POSIXct("2021-05-09 23:00", tz = "Europe/Brussels"),
    as.POSIXct("2021-05-07 23:00", tz = "Europe/Brussels")
  )
  
  for (i in c(1:length(plot_names))) {
    p <- plot_solar_power_time_series(df_solar_power, start_timestamps[i], end_timestamps[i])
    print(p)
    Sys.sleep(2)
    if (save) {
      ggsave(
        file.path(folder, plot_names[i]), 
        width = 12, 
        height = 6,
        units = "cm"
      )
    }
  }
  
  return(invisible())
}

#' @export
plot_target_engineering <- function(df_solar_power, save = FALSE) {
  p <- df_solar_power %>%
    select(timestamp, elia_act_generation_solar_belgium, elia_monitored_capacity, elia_interpolated_monitored_capacity) %>%
    rename(
      "ASG" = elia_act_generation_solar_belgium,
      "IC"= elia_monitored_capacity,
      "Interpolated IC"= elia_interpolated_monitored_capacity,
    ) %>% 
    tidyr$pivot_longer(
      -timestamp,
      names_to = "type",
      values_to = "MW"
    ) %>%
    ggplot() +
    geom_line(aes(x = timestamp, y = MW, color = type))
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, "target_engineering.pdf"), 
      width = 12, 
      height = 7,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_load_factor <- function(df_solar_power, save = FALSE) {
  p <- df_solar_power %>%
    select(timestamp, load_factor) %>%
    rename("Load Factor" = load_factor) %>%
    ggplot() +
    geom_line(aes(x = timestamp, y = `Load Factor`), color = "#F8766D")
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, "load_factor.pdf"),
      width = 12, 
      height = 7,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_weather_variables_time_series <- function(df_averaged_weather, save = FALSE) {
  folder <- "plot/figures/"
  weather_vars <- c(
    "surface_net_solar_radiation",
    "surface_solar_radiation_downwards",
    "relative_humidity",
    "total_cloud_cover",                   
    "temperature_2m",
    "wind_chill_index" 
  )
  plot_titles <- gsub("_", " ", weather_vars)
  units <- c(expression(~J/m^2), expression(~J/m^2), "%", "[0, 1]", "K", "°C")
  
  for (i in c(1:length(weather_vars))) {
    p <- df_averaged_weather %>%
      mutate(selected_var = !!sym(weather_vars[i])) %>%
      ggplot() +
      geom_line(aes(x = local_timestamp, y = selected_var)) + 
      ylab(units[i]) +
      theme_bw() +
      xlab("timestamp") +
      theme(
        plot.title = element_text(size = 14.3, hjust = 0.5),
        axis.title.x = element_text(size = 13.3),
        axis.title.y = element_text(size = 13.3),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11)
      ) + 
      ggtitle(plot_titles[i])
      
    print(p)
    Sys.sleep(2)
    if (save) {
      ggsave(
        file.path(folder, paste0("weather_time_series_", weather_vars[i], ".pdf")), 
        width = 12, 
        height = 6,
        units = "cm"
      )
    }
  }

  return(invisible())
}



