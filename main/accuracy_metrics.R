box::use(
  dplyr[...],
  glue,
  purrr,
  tidyr
)

get_metrics <- function(from_date, to_date, tune_metric = "rmse") {
  folder <- glue$glue("results/test_set_results/tune_metric_{tune_metric}")
  folder_rmse <- glue$glue("results/test_set_results/tune_metric_rmse")
  models <- gsub(paste0(folder, "/"), "", list.dirs(folder))[-1]
  
  df_metrics <- models %>%
    purrr$map(
      ~ {
        print(.x)
        model_name = .x
        dir <- file.path(folder, model_name)
        test_set_result_files <- list.files(dir)
        if (length(test_set_result_files) > 0) {
          df_result_model_x <- test_set_result_files %>% purrr::map(~ {
            readRDS(file.path(dir, .x))
          }) %>%
            bind_rows() %>%
            distinct(timestamp, .keep_all = TRUE) %>%
            mutate(model_name = model_name)
        } else {
          dir_rmse <- file.path(folder_rmse, model_name)
          test_set_result_files <- list.files(dir_rmse)
          df_result_model_x <- test_set_result_files %>% purrr::map(~ {
            readRDS(file.path(dir_rmse, .x))
          }) %>%
            bind_rows() %>%
            distinct(timestamp, .keep_all = TRUE) %>%
            mutate(model_name = model_name)
        }
      }
    ) %>%
    purrr$list_rbind() %>%
    filter(between(date, !!from_date, !!to_date)) %>%
    mutate(error = elia_act_generation_solar_belgium - final_forecast) %>%
    group_by(model_name) %>%
    summarise(
      rmse = sqrt(mean(error^2)),
      mae = mean(abs(error)),
      smape = 2 * mae / mean(elia_act_generation_solar_belgium + final_forecast),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(model_name = gsub("solar_load_factor_be_", "", model_name)) %>%
    tidyr$separate(model_name, into = c("fs", "cl", "m", "n_tree"), sep = "_") %>%
    tidyr$pivot_wider(
      id_cols = c(cl, m, n_tree),
      names_from = fs, values_from = c(rmse, mae, smape, n)
    )
  
  return(df_metrics)
}

from_date = as.Date("2022-01-01")
to_date = as.Date("2023-06-30")
df_metrics_rmse <- get_metrics(from_date, to_date, tune_metric = "rmse")
df_metrics_rmse

df_metrics_mae <- get_metrics(from_date, to_date, tune_metric = "mae")
df_metrics_mae

df_delta <- df_metrics_rmse %>% 
  left_join(df_metrics_mae, by = c("cl", "m", "n_tree")) %>%
  mutate(
    delta_rmse_fs1 = round(rmse_fs1.y - rmse_fs1.x, 1),
    delta_mae_fs1 = round(mae_fs1.y - mae_fs1.x, 1),
    delta_smape_fs1 = round(smape_fs1.y - smape_fs1.x, 4),
    delta_rmse_fs2 = round(rmse_fs2.y - rmse_fs2.x, 1),
    delta_mae_fs2 = round(mae_fs2.y - mae_fs2.x, 1),
    delta_smape_fs2 = round(smape_fs2.y - smape_fs2.x, 4)
  ) %>%
  select(cl, m, n_tree, contains("delta_"))
