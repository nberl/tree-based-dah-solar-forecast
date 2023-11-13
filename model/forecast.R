box::use(
  model/common,
  dplyr[...],
  glue,
  logger,
  lubridate,
  parsnip,
  purrr,
  recipes,
  rsample,
  stats,
  stringr,
  tidyr,
  xgboost
)

#' @export
read_train_object <- function(date, model_name) {
  path <- common$path_model_train_object(date = date, model_name = model_name)
  object <- readRDS(file = path)
  
  return(object)
}

check_completeness <- function(df, prediction_vars) {
  df_prediction_vars <- df[, prediction_vars]
  complete_rows <- (rowSums(is.na(df_prediction_vars)) == 0)
  if (any(!complete_rows)) {
    missing_hours <- df_prediction_vars$local_timestamp[complete_rows]
    logger$log_info("Forecast dataframe contains rows with missing values during:
                    {paste(missing_hours, collapse = ' ')}")
  }
  
  return(complete_rows)
  }

add_postprocessed_forecast <- function(df, target_var) {
  df <- df %>%
    mutate(
      final_forecast = 
        switch(
          !!target_var,
          "load_factor_interpolated" = if_else(forecast < 0, 0, forecast) * elia_interpolated_monitored_capacity,
          "load_factor" = if_else(forecast < 0, 0, forecast) * elia_monitored_capacity,
          if_else(forecast < 0, 0, forecast)
      ),
      final_forecast = if_else(hour %in% c(5:21), final_forecast, 0)
    )
    
  
  return(df)
}

#' @export
create_forecast <- function(
    date, 
    model_name, 
    df_init, 
    train_object = NULL,
    forecast_horizon_in_days = 1
) {
  set.seed(123)  # set seed for reproducibility
  
  if (is.null(train_object)) {
    # date when forecast is created is also the same date when the model is trained
    # the training dataset always contains data until train_date - 1
    train_date <- date
    train_object <- read_train_object(train_date, model_name)
  }
  
  # We always start predicting from the next date
  first_forecast_date <- as.Date(date) + lubridate$days(1)
  last_forecast_date <- as.Date(date) + lubridate$days(forecast_horizon_in_days)
  
  df_selected <- df_init %>%
    filter(between(date, !!first_forecast_date, !!last_forecast_date))
  
  print(glue$glue("First forecast timestamp: {min(df_selected$local_timestamp, na.rm = TRUE)}"))
  print(glue$glue("Last forecast timestamp: {max(df_selected$local_timestamp, na.rm = TRUE)}"))
  
  prediction_vars <- train_object$prediction_vars[[1]]
  other_vars <- colnames(df_selected)[!(colnames(df_selected) %in% prediction_vars)]
  
  df_forecast <- df_selected[, other_vars] %>% 
    bind_cols(
      train_object$recipe[[1]] %>% 
        recipes$prep(training = train_object$df_train[[1]]) %>%
        recipes$bake(new_data = df_selected) 
    )
  
  complete_rows <- check_completeness(df_forecast, prediction_vars)
  
  df_result <- train_object$fitted_model[[1]] %>%
    stats$predict(new_data = df_forecast) %>%
    bind_cols(df_forecast[, other_vars]) %>%
    rename("forecast" = .pred) %>%
    mutate(complete = complete_rows) %>%
    add_postprocessed_forecast(target_var = train_object$target_var)
  
  return(df_result)
}

#' @export
write_forecast_object <- function(date, model_name, forecast_object) {
  path <- common$path_model_forecast_object(date = date, model_name = model_name)
  
  saveRDS(object = forecast_object, file = path)
  
  logger$log_info("Forecast object written to: {path}")
  
  invisible()
}

#' @export
run_forecast_model <- function(date, model_name, df_init, config, save_object = TRUE) {
  forecast_date <- as.Date(date) + lubridate$days(1) # date that is forecasted
  
  forecast_result <- create_forecast(
    date = date, 
    model_name = model_name, 
    df_init = df_init, 
    train_object = NULL
  )
  
  if (save_object) {
    write_forecast_object(date = forecast_date, model_name = model_name, forecast_object = forecast_result)
  }
  
  return(forecast_result)
}



