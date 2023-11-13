box::use(
  model/common,
  model/preprocess_recipes,
  preprocess/total_dataframe,
  dplyr[...],
  glue,
  logger,
  lubridate,
  parsnip,
  purrr,
  recipes,
  tidyr,
  tune,
  workflows
)

#' @export
get_model_specification <- function(model_type, tune_hyperparameters) {
  model <- switch(
    model_type,
    lm = parsnip$linear_reg(
      penalty = tune(),
      mixture = tune()
    ),
    ranger = parsnip$rand_forest(
      mtry = tune(),
      min_n = tune(),
      trees = tune()
    ),
    rpart = parsnip$decision_tree(
      cost_complexity = tune(),
      tree_depth = tune(),
      min_n = tune()
    ),
    xgboost = parsnip$boost_tree(
      mtry = tune(),
      trees = tune(),
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune()
    )
  )
  
  all_hyperparameters <- names(model$args)
  default_hyperparameters <- all_hyperparameters[!(all_hyperparameters %in% tune_hyperparameters)]
  # set all hyperparameters that should not be tuned to default (NULL)
  for (hp in default_hyperparameters) {
    model$args[[hp]] <- NULL
  }
  
  if (model_type == "ranger") {
    model <- model %>% 
      parsnip$set_engine(model_type, importance = "none")
  } else {
    model <- model %>% 
      parsnip$set_engine(model_type)
  }
  
  return(model)
}

#' @export
get_hyperparameters <- function(model_name) {
  path <- common$path_model_hyperparameters(model_name = model_name)
  if (file.exists(path)) {
    df_hyperparameters <- readRDS(path)
  } else {
    print("No tuned hyperparameters found, using default hyperparameters instead!")
    df_hyperparameters <- tibble()
  }
  
  return(df_hyperparameters)
}

#' @export
train_model <- function(train_date, model_name, df_init, config) {
  set.seed(123)  # set seed for reproducibility
  
  config_preprocess <- config$preprocess
  config_model <- config$model
  
  train_date <- as.Date(train_date) # date when model is trained
  # the training dataset always contains data until train_date - 1
  last_observation_date <- train_date - lubridate$days(1)
  first_observation_date <- last_observation_date - lubridate$days(config_model$train_window_in_days)
  
  target_var <- config_model$target_var

  if (is.null(df_init)) {
    df_init <- total_dataframe$preprocess(first_observation_date, last_observation_date, config_preprocess)
  }
  
  df <- df_init %>%
    rename("target" = !!sym(config_model$target_var)) %>%
    filter(between(date, first_observation_date, last_observation_date)) %>%
    tidyr$drop_na()
  
  if (last_observation_date != max(df$date)) {
    stop(glue$glue("Train date = {train_date}, while last date in train dataset is {max(df$date)},
                   last date should be equal to train date - 1 day"))
  }
  
  # dates that should be removed during train process
  dates_outlier <- as.Date(config_model$dates_outlier)
  
  # outlier filter is added here, because we don't want to apply this filter in the recipe
  # since we still want to be able to forecast these days
  df <- df %>%
    filter(!(date %in% !!dates_outlier)) %>%
    filter(hour %in% c(5:21)) # since solar production was only higher than 1 during these hours
  
  print(glue$glue("First train date: {min(df$date)}"))
  print(glue$glue("Last train date: {max(df$date)}"))
  
  # recipe for preprocessing (remove other variables from df)
  recipe <- preprocess_recipes$get_recipe(
    df = df,
    config_model = config_model
  )
  
  df_hyperparameters <- get_hyperparameters(model_name = model_name)
  
  model <- get_model_specification(
    model_type = config_model$engine,
    tune_hyperparameters = colnames(df_hyperparameters)
  ) %>%
    parsnip$set_mode(config_model$mode)
  
  workflow <- workflows$workflow() %>%
    workflows$add_model(model) %>%
    workflows$add_recipe(recipe)
  
  final_workflow <- tune$finalize_workflow(workflow, df_hyperparameters)
  
  fitted_model <- parsnip$fit(final_workflow, data = df)

  prediction_vars <- recipe %>%
    recipes$prep(training = df) %>%
    recipes$bake(NULL) %>%
    select(-target) %>%
    colnames() %>%
    sort()
  
  # store relevant data of trained model into tibble
  df_output <- tibble(
    train_date = train_date,
    model_name = model_name,
    train_window_in_days = config_preprocess$window_in_days,
    target_var = target_var,
    model_engine = config_model$engine,
    fitted_model = list(fitted_model),
    prediction_vars = list(prediction_vars),
    df_train = list(df),
    recipe = list(recipe)
  )
  
  return(df_output)
}

#' @export
write_train_object <- function(date, model_name, train_object) {
  path <- common$path_model_train_object(date = date, model_name = model_name)
  
  saveRDS(object = train_object, file = path)
  
  logger$log_info("Train object written to: {path}")
  
  invisible()
}

#' @export
run_train_model <- function(train_date, model_name, df_init, config, save_object = TRUE) {
  train_date <- as.Date(train_date)  # date when model is trained

  train_result <- train_model(
    train_date = train_date, 
    model_name = model_name,
    df_init = df_init, 
    config = config
  )
  
  if (save_object) {
    write_train_object(date = train_date, model_name = model_name, train_object = train_result)
  }

  return(train_result)
}
