box::use(
  load_config = config/`__init__`,
  train_model = model/train,
  preprocess/total_dataframe,
  dplyr[...]
)

# load model config
config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
included_models <- names(config)

# initial data
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-06-30")
train_date <- as.Date("2021-12-31")

run_times <- list()
for (selected_model in included_models) {
  print(selected_model)
  selected_config <- config[[selected_model]]
  
  print("Start training process: ")
    
  df_init <- total_dataframe$preprocess(start_date, end_date, selected_config$preprocess)
  start <- Sys.time()
  train_result <- train_model$run_train_model(
    train_date = train_date,
    model_name = selected_model, 
    df_init = df_init, 
    config = selected_config, 
    save_object = FALSE
  )
  run_time <- Sys.time() - start
  print(run_time)
  run_times[[selected_model]] <- run_time
}

