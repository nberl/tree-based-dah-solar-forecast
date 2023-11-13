box::use(
  load_config = config/`__init__`,
  tune_model = model/tune,
  preprocess/total_dataframe,
  dplyr[...]
)

# load model config
config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
included_models <- names(config)

# initial data
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-06-30")
tune_date <- as.Date("2021-12-31")

# selected_model <- "solar_load_factor_be_fs2_av_xgb"

already_tuned <- list.files("results/hyperparameters/") %>%
  gsub(".rds", "", .)
to_be_tuned <- included_models[!(included_models %in% already_tuned)]
for (selected_model in to_be_tuned) {
  print(selected_model)
  selected_config <- config[[selected_model]]
  
  if (!is.null(config[[selected_model]]$model$tune)) {
    print("Start tuning process: ")
    
    df_init <- total_dataframe$preprocess(start_date, end_date, selected_config$preprocess)
  
    tune_result <- tune_model$run_tune_model(
      tune_date = tune_date,
      model_name = selected_model, 
      df_init = df_init, 
      config = selected_config, 
      save_tune_result = TRUE, 
      save_hyperparameters = TRUE,
      use_multiple_cores = FALSE
    )
  } else {
    print("No hyperparameters to tune, tuning skipped!")
  }
}
