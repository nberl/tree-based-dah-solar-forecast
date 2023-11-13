box::use(
  load_config = config/`__init__`,
  forecast = model/forecast_rolling_window,
  model/common,
  preprocess/total_dataframe,
  dplyr[...],
  purrr
)

# load model config
config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
included_models <- names(config)

# initial data
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-06-30")

start_date_test_set <- as.Date("2022-01-01")
end_date_test_set <- as.Date("2023-06-30")

update_start_date_based_on_available_result <- function(
    start_date_test_set, end_date_test_set, selected_model
) {
  path <- common$path_model_test_set_object(
    start_date_set_test, end_date_set_test, selected_model
  )
  
  folder <- dirname(path)
  files <- list.files(folder)
  
  if (length(files) > 0) {
    start_end_dates <- gsub(".rds", "", files)
    df_dates <- tibble(start_end = start_end_dates) %>%
      tidyr::separate(start_end, into = c("start", "end"), sep = "_")
    
    start <- max(as.Date(df_dates$end)) + lubridate::days(1)
    print(paste0("Start date adapted to: ", start))
  } else {
    start <- start_date_test_set
  }
  
  return(start)
}

for (selected_model in included_models) {
  print(selected_model)
  selected_config <- config[[selected_model]]
  
  updated_start_date_test_set <- update_start_date_based_on_available_result(
    start_date_test_set, end_date_test_set, selected_model
  )

  if (updated_start_date_test_set < end_date_test_set) {
    print("Start forecast rolling window process: ")
    
    df_init <- total_dataframe$preprocess(start_date, end_date, selected_config$preprocess)
    
    df_forecast_result <- forecast$get_test_set_results(
      from_date = updated_start_date_test_set, 
      to_date = end_date_set_test,
      model_name = selected_model, 
      df_init = df_init, 
      config = selected_config,
      forecast_horizon_in_days = 1
    )
    
    path <- common$path_model_test_set_object(
      updated_start_date_test_set, end_date_set_test, selected_model
    )
    saveRDS(df_forecast_result, file = path)
    
    error <- df_forecast_result$elia_act_generation_solar_belgium - df_forecast_result$final_forecast
    rmse <- sqrt(mean((error)^2))
    mae <- mean(abs(error))
    print(paste("RMSE: ", rmse))
    print(paste("MAE: ", mae))
  } else {
    print("Already done!")
  }
}
