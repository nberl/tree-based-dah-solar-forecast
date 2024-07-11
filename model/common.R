box::use(
  glue
)

create_folder_if_not_exists <- function(path) {
  folder <- dirname(path)
  
  if (folder != "." && !dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  return(invisible())
}

#' @export
path_model_train_object <- function(date, model_name, tune_metric = "rmse") {
  main_path <- getOption("box.path")
  prefix <- glue$glue("{main_path}/results/train_results/tune_metric_{tune_metric}/{model_name}/{date}")
  filename <- "train_result.rds"
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
path_model_tune_object <- function(date, model_name) {
  main_path <- getOption("box.path")
  prefix <- glue$glue("{main_path}/results/tune_results/{model_name}/{date}")
  filename <- "tune_result.rds"
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
main_dir_hyperparameters <- function() {
  main_path <- getOption("box.path")
  dir <- glue$glue("{main_path}/results/hyperparameters")
  
  return(dir)
}

#' @export
path_model_hyperparameters <- function(model_name, metric) {
  main_path <- main_dir_hyperparameters()
  path <- glue$glue("{main_path}/{metric}/{model_name}.rds")

  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
path_model_forecast_object <- function(date, model_name, tune_metric = "rmse") {
  main_path <- getOption("box.path")
  prefix <- glue$glue("{main_path}/results/forecast_results/tune_metric_{tune_metric}/{model_name}/{date}")
  filename <- "forecast_result.rds"
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
dir_model_test_set_object <- function(model_name, tune_metric = "rmse") {
  main_path <- getOption("box.path")
  dir <- glue$glue("{main_path}/results/test_set_results/tune_metric_{tune_metric}/{model_name}")
  
  return(dir)
}

#' @export
path_model_test_set_object <- function(from_date, to_date, model_name, tune_metric = "rmse") {
  prefix <- dir_model_test_set_object(model_name, tune_metric)
  filename <- glue$glue("{from_date}_{to_date}.rds")
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
path_model_intermediate_test_set_object <- function(model_name, tune_metric = "rmse") {
  main_path <- getOption("box.path")
  prefix <- glue$glue("{main_path}/results/intermediate_test_set_results/tune_metric_{tune_metric}/{model_name}")
  filename <- "current_result.rds"
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
path_shap_result <- function(model_name) {
  main_path <- getOption("box.path")
  prefix <- glue$glue("{main_path}/results/shap")
  filename <- glue$glue("{model_name}.rds")
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}

#' @export
path_mcs_result <- function(metric, confidence_level, tune_metric = "rmse", hour = NULL) {
  main_path <- getOption("box.path")
  prefix <- glue$glue("{main_path}/results/mcs")
  if (!is.null(hour)) {
    filename <- glue$glue("mcs_{metric}_{confidence_level}_{tune_metric}_h{hour}.rds")
  } else {
    filename <- glue$glue("mcs_{metric}_{confidence_level}_{tune_metric}.rds")
  }
  
  path <- glue$glue("{prefix}/{filename}")
  
  create_folder_if_not_exists(path)
  
  return(path)
}
