box::use(
  model_tune = model/tune,
  dplyr[...],
  forcats,
  ggplot2[...],
  lubridate,
  purrr,
  rsample,
  tidyr
)

#' @export
plot_tune_validation_windows <- function(df_init, config, save = FALSE){
  config_model <- config$model
  config_tune <- config_model$tune
  
  tune_date <- as.Date("2021-12-30") # date when model is tune
  
  total_validation_days <- config_tune$n_validation_sets * config_tune$validation_window
  total_days_in_tune_dataset <- config_tune$train_window_in_days + total_validation_days
  total_tune_dataset_period <- lubridate$days(total_days_in_tune_dataset)
  
  last_observation_date <- tune_date - lubridate$days(1)
  first_observation_date <- last_observation_date - total_tune_dataset_period
  
  target_var <- config_model$target_var
  
  df_tune <- df_init %>%
    filter(
      date >= !!first_observation_date,
      date <= !!last_observation_date,
      !(date %in% !!config_model$dates_outlier)
    ) %>%
    filter(hour %in% c(5:21))
  
  folds <- model_tune$create_folds(df_tune, config_tune)
  
  df_folds <- folds %>% 
    mutate(
      dates = purrr$map(splits, model_tune$get_start_end_date_from_analysis_assessment_split)
    ) %>% 
    tidyr::unnest(dates)
  
  p <- df_folds %>% 
    tidyr$pivot_longer(
      cols = where(lubridate$is.Date),
      names_to = "type",
      values_to = "date"
    ) %>% 
    tidyr$separate(type, c("start_or_end", "dataset_type"), "_date_") %>%
    mutate(
      slice = forcats$fct_rev(id),
      dataset_type = forcats$fct_rev(dataset_type)
    ) %>%
    ggplot(
      aes(
        y = slice, 
        x = date, 
        colour = dataset_type
      )
    ) +
    geom_line(linewidth = 2) +
    theme_minimal() +
    xlab("Date") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_text(size=13.3),
      panel.background = element_rect(
        fill = "white", colour = "white", linewidth = 2, linetype = "solid"
      ),
      panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "gray95"), 
      panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "gray95"),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(size = 14.3, hjust = 0.5),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11)
    )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, paste0("tune_validation_windows.pdf")),
      width = 13,
      height = 10,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_train_forecast_windows <- function(df_init, config, save = FALSE) {
  config_model <- config$model
  
  first_train_date <- as.Date("2021-12-31")
  # the training dataset always contains data until train_date - 1
  last_observation_date <- first_train_date - lubridate$days(1)
  first_observation_date <- last_observation_date - lubridate$days(config_model$train_window_in_days)
  
  df_train_forecast <- df_init %>%
    filter(between(date, first_observation_date, max(df_init$date))) %>%
    tidyr$drop_na()
 
  folds <- rsample$sliding_period(
    data = df_train_forecast, 
    index = date,
    period = "day", 
    lookback =  config_model$train_window_in_days,
    assess_start = 2,
    assess_stop = 4, # 3 days because 1 day is not visible on plot
    step = 1
  )
  
  df_folds <- folds %>% 
    mutate(
     dates = purrr$map(splits, model_tune$get_start_end_date_from_analysis_assessment_split)
    ) %>% 
    tidyr::unnest(dates)

  p <- df_folds %>% 
    tidyr$pivot_longer(
      cols = where(lubridate$is.Date),
      names_to = "type",
      values_to = "date"
    ) %>% 
    tidyr$separate(type, c("start_or_end", "dataset_type"), "_date_") %>%
    mutate(
     slice = forcats$fct_rev(id),
     dataset_type = forcats$fct_rev(dataset_type)
    ) %>%
    ggplot(aes(y = slice, x = date, colour = dataset_type)) +
    geom_path(aes(size = dataset_type), show.legend = FALSE) +
    scale_size_manual(values = c(1, 0.1)) +
    theme_minimal() +
    xlab("Date") +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 13.3),
      axis.title.y = element_blank(),
      panel.background = element_rect(
        fill = "white", colour = "white", linewidth = 2, linetype = "solid"
      ),
      panel.grid.major = element_line(
        size = 0.5, linetype = 'solid', colour = "gray95"
      ), 
      panel.grid.minor = element_line(
        size = 0.25, linetype = 'solid', colour = "gray95"
      ),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(size = 14.3, hjust = 0.5),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11)
    ) +
    scale_y_discrete(breaks = c("Slice001", "Slice200", "Slice400", "Slice545" ))+
    scale_x_date(expand = c(0.04, 0.0))

  folder <- "plot/figures/"
  if (save) {
   ggsave(
     file.path(folder, paste0("train_forecast_windows.pdf")),
     width = 13,
     height = 10,
     units = "cm"
    )
  }
}

#' @export
plot_train_forecast_windows_noah <- function(config_tune){
  
  sliding_train_size <- config_tune$preprocess$window_in_days 
  resamples <- rsample::sliding_period(df_test_window, 
                                       index = date, 
                                       period = "day", 
                                       lookback = sliding_train_size, 
                                       assess_stop = 2,
                                       step = 1)
  
  p <- rsamples %>% 
    extract_dates_rset() %>% 
    print() %>% 
    select(-one_of("splits")) %>%
    tidyr::pivot_longer(cols = where(lubridate::is.Date),
                        names_to = "type",
                        values_to = "date") %>% 
    tidyr::separate(type, c("data_type", "range_type"), "_") %>% 
    ggplot(aes(y = forcats::fct_rev(id), x = date, colour = forcats::fct_rev(data_type)))+
    geom_path(aes(size = forcats::fct_rev(data_type) ), show.legend = FALSE) +
    #geom_line(size = c(0.01))+
    #scale_colour_manual(values = c("red", "gray"))+
    #scale_size_manual(values = c( "assessment" = 100, "analysis" = 1))+
    scale_size_manual( values = c(1,0.1) ) +
    theme_minimal()+
    xlab("Year")+
    theme(legend.position = "none",
          axis.title.x = element_text(size=13.3),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray95"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray95"),
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(size = 14.3,hjust = 0.5),
          axis.text.y = element_text(size=11),
          axis.text.x = element_text(size=11))+
    scale_y_discrete(breaks = c("Slice001", "Slice050", "Slice100", "Slice150", "Slice200", "Slice250", "Slice300" ))+
    scale_x_date(expand = c(0.04,0.0))
  
  return(p)
}


