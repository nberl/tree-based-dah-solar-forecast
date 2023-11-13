box::use(
  load_config = config/`__init__`,
  train_model = model/train,
  model/common,
  model/feature_importance,
  preprocess/total_dataframe,
  dplyr[...],
  lubridate,
  purrr
)

# load model config
config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
included_models <- names(config)

start_date_total_dataset <- as.Date("2019-01-01")
end_date_total_dataset <- as.Date("2023-06-30")

start_train_date <- as.Date("2022-01-01")
train_dates <- seq(start_train_date, end_date_total_dataset, by = "month")

for (selected_model in included_models[c(23)]) {
  selected_config <- config[[selected_model]]
  df_init <- total_dataframe$preprocess(
    start_date_total_dataset, end_date_total_dataset, selected_config$preprocess
  )
  
  if (file.exists(common$path_shap_result(selected_model))) {
    df_shap <- readRDS(common$path_shap_result(selected_model))
    train_dates <- train_dates[!(train_dates %in% df_shap$train_date)]
  } else {
    df_shap <- tibble()
  }
  for (i in c(1:length(train_dates))) {
    train_date <- train_dates[i]
    
    df_train_result <- train_model$train_model(
      train_date = train_date,
      model_name = selected_model,
      df_init = df_init,
      config = selected_config
    )
    
    start_date_test_set <- train_date
    end_date_test_set <- lubridate$ceiling_date(train_date, unit = "month") - lubridate$days(1)
      
    df_test <- df_init %>% 
      filter(between(date, start_date_test_set, end_date_test_set))
    
    start_time <- Sys.time()
    df_shap_x <- feature_importance$variable_importance_shap(
      train_object = df_train_result, 
      df_test = df_test,
      nsim = 30
    ) %>% 
      mutate(train_date = train_date)
    
    print(Sys.time() - start_time)
    
    df_shap <- df_shap %>%
      bind_rows(df_shap_x)
    
    saveRDS(df_shap, common$path_shap_result(selected_model))
  }
}

