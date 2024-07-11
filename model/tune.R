box::use(
  model/common,
  model/preprocess_recipes,
  model/train,
  preprocess/total_dataframe,
  dials,
  dplyr[...],
  # doParallel,
  hardhat,
  logger,
  lubridate,
  parsnip,
  # parallel,
  purrr,
  recipes,
  rsample,
  tidyr,
  tune,
  workflows,
  yardstick
)

#' @export
get_start_end_date_from_analysis_assessment_split <- function(split) {
  df_analysis <- rsample$analysis(split)
  df_assessment <- rsample$assessment(split)
  
  df <- tibble(
    start_date_analysis = min(df_analysis[["date"]]),
    end_date_analysis = max(df_analysis[["date"]]),
    start_date_assessment = min(df_assessment[["date"]]),
    end_date_assessment = max(df_assessment[["date"]])
  )
  
  return(df)
}

#' @export
get_all_start_end_dates_from_analysis_assessment_splits <- function(splits) {
  df_start_end_dates <- splits %>% 
    purrr$map_dfr(~ get_start_end_date_from_analysis_assessment_split(.x))
  
  return(df_start_end_dates)
}

#' @export
create_folds <- function(df_tune, config_tune) {
  folds <- rsample$sliding_period(
    data = df_tune, 
    index = date,
    period = "day", 
    lookback =  config_tune$train_window_in_days,
    assess_stop = config_tune$validation_window,
    step = config_tune$validation_window
  )
  
  print("Folds: ")
  print(get_all_start_end_dates_from_analysis_assessment_splits(folds$splits))
  
  return(folds)
}

set_fixed_hyperparameters <- function(model, fixed_hyperparameters) {
  for (i in seq_along(fixed_hyperparameters)) {
    model$args[names(fixed_hyperparameters)[i]] <- fixed_hyperparameters[i]
  }
  
  return(model)
}

calculate_max_mtry_hyperparameter <- function(tune_hyperparameters, df_tune, recipe) {
  if ("mtry" %in% tune_hyperparameters) {
    df_preprocessed <- recipe %>% recipes$prep(., training = df_tune) %>% recipes$bake(df_tune[0, ])
    max_mtry <- df_preprocessed %>% select(-target) %>% NCOL(.)
  } else {
    max_mtry <- NULL
  }
  
  return(max_mtry)
}

set_hyperparameter_boundaries <- function(hyperparams, max_mtry = NULL) {
  if ("mtry" %in% hyperparams$name) {
    hyperparams <- hyperparams %>%
      recipes$update(mtry = dials$mtry(c(1, max_mtry)))
  }
  
  if ("tree_depth" %in% hyperparams$name) {
    hyperparams <- hyperparams %>%
      recipes$update(tree_depth = dials$tree_depth(c(1L, 15L)))
  }
  
  if ("min_n" %in% hyperparams$name) {
    hyperparams <- hyperparams %>%
      recipes$update(min_n = dials$min_n(c(2L, 120L)))
  }
  
  if ("learn_rate" %in% hyperparams$name) {
    hyperparams <- hyperparams %>%
      recipes$update(learn_rate = dials$learn_rate(c(-4L, -1L)))
  }
  
  if ("sample_size" %in% hyperparams$name) {
    hyperparams <- hyperparams %>%
      recipes$update(sample_size = dials$sample_prop(c(0.5, 1)))
  }
  
  return(hyperparams)
}


#' @export
tune_model <- function(tune_date, model_name, df_init, config, use_multiple_cores = FALSE) {
  set.seed(123) #  set seed for reproducibility
  
  start_time_tuning_process <- Sys.time()
  
  config_preprocess <- config$preprocess
  config_model <- config$model
  config_tune <- config_model$tune
  
  tune_date <- as.Date(tune_date) # date when model is tune
  
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
    filter(hour %in% c(5:21))  %>% # since solar production was only higher than 1 during these hours
    rename("target" = !!target_var) %>%
    tidyr$drop_na()
  
  recipe <- preprocess_recipes$get_recipe(df = df_tune, config_model = config_model)
  
  folds <- create_folds(df_tune, config_tune)
  
  tune_hyperparameters <- config_tune$tune_hyperparameters

  model <- train$get_model_specification(
    model = config_model$engine,
    tune_hyperparameters = tune_hyperparameters
  ) %>%
    parsnip$set_mode(config_model$mode) %>%
    parsnip$set_engine(config_model$engine) %>%
    set_fixed_hyperparameters(., fixed_hyperparameters = config_tune$fixed_hyperparameters)
  
  workflow <- workflows$workflow() %>%
    workflows$add_model(model) %>%
    workflows$add_recipe(recipe)
  
  print("Tune workflow:")
  print(workflow)
  
  if (use_multiple_cores) {
    cluster <- doParallel$makePSOCKcluster(parallel$detectCores(logical = FALSE))
    doParallel$registerDoParallel(cluster)
  }
  
  max_mtry <- calculate_max_mtry_hyperparameter(tune_hyperparameters, df_tune, recipe)
  
  hyperparameters <- workflow %>%
    hardhat$extract_parameter_set_dials() %>%
    set_hyperparameter_boundaries(
      max_mtry = max_mtry
    )
  
  logger$log_info("Start initial grid search ...")
  tune_result <- tune$tune_grid(
    object = workflow,
    resamples = folds,
    param_info = hyperparameters,
    grid = 25,
    metrics = yardstick$metric_set(yardstick::rmse, yardstick::mae),
    control = tune$control_grid(verbose = TRUE)
  )
  
  if (use_multiple_cores) {
    #  stop clusters
    parallel$stopCluster(cluster)
  }
  
  print(Sys.time() - start_time_tuning_process)
  
  df_output <- tibble(
    folds = list(folds),
    tune_result = list(tune_result),
    df_tune = list(df_tune),
    duration = Sys.time() - start_time_tuning_process 
  )
  
  return(df_output)
}

#' @export
write_tune_result <- function(date, model_name, tune_result) {
  path <- common$path_model_tune_object(date = date, model_name = model_name)
  
  saveRDS(object = tune_result, file = path)
  
  logger$log_info("Tune object written to: {path}")
  
  invisible()
}

select_hyperparameters <- function(tune_result, config, metric = "rmse") {
  selected_hyperparameters <- tune$select_best(tune_result, metric = metric) %>%
    select(-starts_with("."))
  
  # add fixed_hyperparameters
  fixed_hp <- config$fixed_hyperparameters
  for (i in seq_along(fixed_hp)) {
    selected_hyperparameters[names(fixed_hp)[i]] <- fixed_hp[i]
  }

  return(selected_hyperparameters)
}

write_hyperparameters <- function(model_name, hyperparameters, metric = "rmse") {
  hyperparameters <- hyperparameters[colnames(hyperparameters) %>% sort()]

  path <- common$path_model_hyperparameters(model_name = model_name, metric = metric)
  
  saveRDS(object = hyperparameters, file = path)
  
  logger$log_info("Hyperparameters written to: {path}")
  
  invisible()
}

write_best_hyperparameters <- function(model_name, tune_result, config, metric = "rmse") {
  hyperparameters <- select_hyperparameters(tune_result, config, metric = metric)
  
  write_hyperparameters(
    model_name = model_name, 
    hyperparameters = hyperparameters,
    metric = metric
  )
  
  invisible()
}


#' @export
run_tune_model <- function(
    tune_date, model_name, df_init, config, 
    save_tune_result = TRUE, 
    save_hyperparameters = TRUE,
    use_multiple_cores = FALSE
) {
  path <- common$path_model_tune_object(date = tune_date, model_name = model_name)
  if (!file.exists(path)) {
    df_output <- tune_model(tune_date, model_name, df_init, config, use_multiple_cores)
    
    if (save_tune_result) {
      write_tune_result(date = tune_date, model_name = model_name, tune_result = df_output)
    }
  } else {
    logger$log_info("{model_name} is already tuned!")
    df_output <- readr::read_rds(path)
  }
  
  if (save_hyperparameters) {
    tune_result <- df_output$tune_result[[1]]
    
    write_best_hyperparameters(
      model_name = model_name,
      tune_result = tune_result, 
      config = config$model$tune,
      metric = "rmse"
    )
    
    write_best_hyperparameters(
      model_name = model_name,
      tune_result = tune_result, 
      config = config$model$tune,
      metric = "mae"
    )
  }
  
  return(tune_result)
}


