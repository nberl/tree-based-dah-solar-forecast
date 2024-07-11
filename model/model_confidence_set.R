box::use(
  model/common,
  dplyr[...],
  MCS,
  purrr,
  tidyr
)

filter_hour <- function(df, hour) {
  if (!is.null(hour)) {
    df <- df %>%
      filter(hour == !!hour)
  }
  
  return(df)
}

#' @export
get_mcs_result <- function(
    included_models, 
    alpha, 
    metric = c("mae", "mse", "smape"),
    statistic = c("Tmax", "TR"),
    tune_metric = "rmse",
    hour = NULL
) {
  metric <- match.arg(metric)
  statistic <- match.arg(statistic)
  
  df_forecast_result <- included_models %>%
    purrr$map(~ { 
      model_name <- .x
      folder <- common$dir_model_test_set_object(.x, tune_metric = tune_metric)
      if (length(list.files(folder)) == 0) {
        folder <- common$dir_model_test_set_object(.x, tune_metric = "rmse")
      }
      df_fc_x <- list.files(folder) %>%
        purrr$map(~ {
          filename <- .x
          readRDS(file.path(folder, filename)) %>%  
            select(
              timestamp, local_timestamp, date, hour,
              elia_act_generation_solar_belgium, final_forecast
            ) %>%
            mutate(model_name = model_name)
        }) %>%
        purrr$list_rbind() %>%
        distinct(timestamp, .keep_all = TRUE)
      
      return(df_fc_x)
    }) %>%
    purrr$list_rbind()
  
  df_loss <- df_forecast_result %>%
    filter_hour(hour = hour) %>%
    mutate(
      error = elia_act_generation_solar_belgium - final_forecast,
      mse = error^2,
      mae = abs(error),
      is_div_by_zero = (elia_act_generation_solar_belgium + final_forecast) == 0,
      smape = if_else(
        !is_div_by_zero,
        2 * abs(error) / (elia_act_generation_solar_belgium + final_forecast),
        0
      )
    ) %>%
    tidyr$pivot_wider(
      id_cols = timestamp,
      names_from = model_name,
      values_from = c(mae, mse, smape)
    )
  
  loss_matrix <- as.matrix(
    df_loss %>%
      select(starts_with(metric)) %>%
      tidyr$drop_na()
  )
  print(dim(loss_matrix))
  
  mcs <- MCS$MCSprocedure(
    loss_matrix, 
    alpha = alpha, 
    B = 1000, 
    statistic = statistic, 
    cl = NULL
  )
  
  return(mcs)
}


