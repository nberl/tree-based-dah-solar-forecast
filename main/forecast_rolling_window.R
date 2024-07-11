box::use(
  load_config = config/`__init__`,
  forecast = model/forecast_rolling_window,
  model/common,
  preprocess/total_dataframe,
  dplyr[...],
  glue,
  purrr
)

# load model config
config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
included_models <- names(config)
tune_metric <- "mae"

# initial data
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-06-30")

start_date_test_set <- as.Date("2022-01-01")
end_date_test_set <- as.Date("2023-06-30")

update_start_date_based_on_available_result <- function(
    start_date_test_set, end_date_test_set, selected_model, tune_metric
) {
  path <- common$path_model_test_set_object(
    start_date_test_set, end_date_test_set, selected_model, tune_metric
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

is_same_hyperparameters_rmse <- function(selected_model, tune_metric) {
  path_mae <- common$path_model_hyperparameters(selected_model, tune_metric)
  path_rmse <- common$path_model_hyperparameters(selected_model, "rmse")
  
  if (!file.exists(path_mae) || !file.exists(path_rmse)) {
    print(glue$glue("Hyperparameter path does not exist for {selected_model}"))
    return(FALSE)
  }
  
  df_hp_mae <- readr::read_rds(path_mae)
  df_hp_rmse <- readr::read_rds(path_rmse)
  
  check_all_equal <- all.equal(df_hp_mae, df_hp_rmse)
  if (!isTRUE(check_all_equal)) {
    print(check_all_equal)
  }
  
  return(isTRUE(check_all_equal))
}

for (selected_model in included_models) {
  print(selected_model)
  selected_config <- config[[selected_model]]
  
  updated_start_date_test_set <- update_start_date_based_on_available_result(
    start_date_test_set, end_date_test_set, selected_model, tune_metric
  )
  
  if (tune_metric != "rmse") {
    use_rmse_result <- is_same_hyperparameters_rmse(selected_model, tune_metric)
    if (use_rmse_result) {
      print(glue$glue("{selected_model} for tune metric {tune_metric}, has the same hp as with rmse"))
    }
  }

  if (!use_rmse_result && (updated_start_date_test_set < end_date_test_set)) {
    print("Start forecast rolling window process: ")
    
    df_init <- total_dataframe$preprocess(start_date, end_date, selected_config$preprocess)
    
    df_forecast_result <- forecast$get_test_set_results(
      from_date = updated_start_date_test_set, 
      to_date = end_date_test_set,
      model_name = selected_model, 
      df_init = df_init, 
      config = selected_config,
      forecast_horizon_in_days = 1,
      tune_metric = tune_metric
    )
    
    path <- common$path_model_test_set_object(
      updated_start_date_test_set, end_date_test_set, selected_model, tune_metric
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

