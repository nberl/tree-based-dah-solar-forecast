box::use(
  dplyr[...],
  purrr,
  tidyr
)

get_metrics <- function(from_date, to_date) {
  folder <- "results/test_set_results/"
  models <- gsub(paste0(folder, "/"), "", list.dirs(folder))[-1]
  
  df_metrics <- models %>%
    purrr$map(
      ~ {
        model_name = .x
        dir <- file.path(folder, model_name)
        df_result_model_x <- list.files(dir) %>% purrr::map(~ {
          readRDS(file.path(dir, .x))
        }) %>%
          bind_rows() %>%
          distinct(timestamp, .keep_all = TRUE) %>%
          mutate(model_name = model_name)
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
    tidyr$separate(model_name, into = c("fs", "cl", "m"), sep = "_") %>%
    tidyr$pivot_wider(
      id_cols = c(cl, m),
      names_from = fs, values_from = c(rmse, mae, smape, n)
    )
  
  return(df_metrics)
}

from_date = as.Date("2022-01-01")
to_date = as.Date("2023-06-30")
df_metrics <- get_metrics(from_date, to_date)
df_metrics