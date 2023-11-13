box::use(
  load_config = config/`__init__`,
  model/common,
  model/model_confidence_set
)

config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
included_models <- names(config)

metrics <- c("mse", "mae", "smape")
alphas <- c(0.01, 0.05, 0.1)
statistic <- "Tmax"

mcs_results <- list()
for (metric in metrics) {
  for (alpha in alphas) {
    confidence_level <- 100 - (alpha * 100)
    name <- paste("mcs", metric, confidence_level, sep = "_")
    path <- common$path_mcs_result(metric, confidence_level)
    folder <- dirname(path)
    print(name)
    
    if (!(paste0(name, ".rds") %in% list.files(folder))) {
      print("Start MCS procedure")
      mcs <- model_confidence_set$get_mcs_result(
        included_models = included_models, 
        alpha = alpha,
        metric = metric,
        statistic = statistic
      )
      
      mcs_results[[name]] <- mcs
      saveRDS(mcs, path)
    } else {
      print("Already done!")
    }
  }
}

# per hour
metrics <- c("mse")
alphas <- c(0.1)
statistic <- "Tmax"
hours <- c(5:21)

mcs_results <- list()
for (metric in metrics) {
  for (alpha in alphas) {
    for (hour in hours) {
      confidence_level <- 100 - (alpha * 100)
      name <- paste("mcs", metric, confidence_level, paste0("h", hour), sep = "_")
      path <- common$path_mcs_result(metric, confidence_level, hour = hour)
      folder <- dirname(path)
      print(name)
      
      if (!(paste0(name, ".rds") %in% list.files(folder))) {
        print("Start MCS procedure")
        mcs <- model_confidence_set$get_mcs_result(
          included_models = included_models, 
          alpha = alpha,
          metric = metric,
          statistic = statistic,
          hour = hour
        )
        
        mcs_results[[name]] <- mcs
        saveRDS(mcs, path)
      } else {
        print("Already done!")
      }
    }
  }
}

mcs_test_name <- "mcs_mse_90"
df_superior_models_per_hour <- tibble(
  hour = as.numeric(gsub(paste0(mcs_test_name, "_h"), "", names(mcs_results))),
  superior_models = mcs_results %>% purrr::map(~ {
    gsub("mse_", "", .x@Info$model.names)
  })
)
for (model in included_models) {
  included <- df_superior_models_per_hour$superior_models %>%
    purrr::map(~ { print(.x) ; (model %in% .x) }) %>%
    purrr::list_c()
  df_superior_models_per_hour[, model] <- included
}
library(ggplot2)

map_model_type <- function(model_name) {
  if (grepl("_lm", model_name)) {
    return("LR")
  }
  
  if (grepl("_rt", model_name)) {
    return("RT")
  }
  
  if (grepl("_rf", model_name)) {
    return("RF")
  }
  
  if (grepl("_xgb", model_name)) {
    return("XGBoost")
  }
}

map_feature_set <- function(model_name) {
  if (grepl("_fs1_", model_name)) {
    return("(a)")
  }
  
  if (grepl("_fs2_", model_name)) {
    return("(b)")
  }
}

map_clusters <- function(model_name) {
  if (grepl("_av_", model_name)) {
    return("average")
  }
  
  if (grepl("_cl5_", model_name)) {
    return("5")
  }
  
  if (grepl("_cl12_", model_name)) {
    return("12")
  }
}

df_plot <- df_superior_models_per_hour %>%
  select(-superior_models) %>%
  tidyr::pivot_longer(
    -hour,
    names_to = "model",
    values_to = "included"
  ) %>%
  mutate(
    model_type = model %>% purrr::map(map_model_type) %>% purrr::list_c(),
    feature_set = model %>% purrr::map(map_feature_set) %>% purrr::list_c(),
    cluster = model %>% purrr::map(map_clusters) %>% purrr::list_c()
  ) %>%
  tidyr::unite(
    "model_name_in_paper", 
    all_of(c("model_type", "feature_set", "cluster")), 
    sep = "-",
    remove = TRUE
  ) %>%
  mutate(model_name_in_paper = factor(model_name_in_paper, levels = rev(unique(.$model_name_in_paper)))) 

df_plot %>%
  ggplot() +
  geom_tile(aes(x = hour, y = model_name_in_paper, fill = included), color = "gray") +
  labs(
    y = "Model", 
    x = "Hour during day"
  )

ggsave(
  paste0("plot/figures/", mcs_test_name, "_per_hour.pdf"),
  width = 17,
  height = 12,
  units = "cm"
)


