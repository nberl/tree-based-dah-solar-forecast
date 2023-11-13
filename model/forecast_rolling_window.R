box::use(
  train_model = model/train,
  model/common,
  model/forecast,
  preprocess/total_dataframe,
  dplyr[...],
  glue,
  lubridate,
  progress,
  purrr
)

first_train_then_forecast <- function(
    train_date, model_name, df_init, config, forecast_horizon_in_days = 1
) {
  # training of model and forecast are on the same day, we always predict from the next day
  
  df_train_result <- train_model$train_model(
    train_date = train_date,
    model_name = model_name,
    df_init = df_init,
    config = config
  )
  
  df_forecast_result <- forecast$create_forecast(
    date = train_date,
    model_name = model_name, 
    df_init = df_init, 
    train_object = df_train_result,
    forecast_horizon_in_days = forecast_horizon_in_days
  )
  
  return(df_forecast_result)
}

write_intermediate_test_set_result <- function(model_name, df_forecast_result) {
  path <- common$path_model_intermediate_test_set_object(model_name)
  
  saveRDS(df_forecast_result, path)
  
  return(invisible())
}

#' @export
get_test_set_results <- function(
    from_date, 
    to_date,
    model_name, 
    df_init, 
    config,
    forecast_horizon_in_days = 1,
    save_intermediate_result = FALSE
) {
  first_train_date <- as.Date(from_date) - lubridate$days(1)
  last_train_date <- as.Date(to_date) - lubridate$days(1)
  train_dates <- seq(
    first_train_date, 
    last_train_date, 
    by = glue$glue("{forecast_horizon_in_days} day")
  )
  
  pb <- progress::progress_bar$new(total = length(train_dates))
  
  df_forecast_result <- tibble()
  for (train_date in train_dates) {
    df_forecast_result_x <- first_train_then_forecast(
      train_date = train_date, 
      model_name = model_name, 
      df_init = df_init, 
      config = config, 
      forecast_horizon_in_days = forecast_horizon_in_days
    )
    
    df_forecast_result <- df_forecast_result %>%
      bind_rows(df_forecast_result_x)
    
    write_intermediate_test_set_result(model_name, df_forecast_result)
    
    pb$tick()
  }
  
  return(df_forecast_result)
}
