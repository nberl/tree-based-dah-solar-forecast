box::use(
  model/common,
  dplyr[...],
  forcats,
  ggplot2[...],
  ggpubr,
  ggtext,
  lubridate,
  purrr,
  stringr,
  tidyr
)

#' @export
plot_forecast_vs_actual_selected_models_over_selected_periods <- function(save = FALSE) {
  list_selected_models <- list(
    "solar_load_factor_be_fs2_cl5_xgb" = "XGBoost-(b)-5",
    # "solar_load_factor_be_fs2_cl12_rf" = "RF-(b)-12",
    "solar_load_factor_be_fs2_cl12_xgb" = "XGBoost-(b)-12"
  )
  
  start_dates_selected_periods <- c(
    as.Date("2022-01-01"),
    as.Date("2022-04-01"),
    as.Date("2022-06-01"),
    as.Date("2022-10-01")
  )
  
  df_forecast_result <- names(list_selected_models) %>%
    purrr$map(
      ~ {
        model_name = .x
        dir <- common$dir_model_test_set_object(.x)
        df_result_model_x <- list.files(dir) %>% 
          purrr$map(~ {
            readRDS(file.path(dir, .x))
          }) %>%
          bind_rows() %>%
          distinct(timestamp, .keep_all = TRUE) %>%
          mutate(
            model_name = model_name,
            type = list_selected_models[[.x]]
          ) %>%
          select(
            timestamp, local_timestamp, date,
            type, elia_act_generation_solar_belgium, final_forecast
          ) %>%
          rename("Actual" = elia_act_generation_solar_belgium)
      }
    ) %>%
    purrr$list_rbind()
  
  df_plot <- df_forecast_result %>%
    tidyr$pivot_wider(
      id_cols = c(timestamp, local_timestamp, date, Actual),
      names_from = type,
      values_from = final_forecast
    ) %>%
    tidyr$pivot_longer(
      -c(timestamp, local_timestamp, date),
      names_to = "Type",
      values_to = "Solar Power [MW]"
    ) %>%
    rename("Local timestamp" = local_timestamp)
  
  for (i in c(1:length(start_dates_selected_periods))) {
    sd <- start_dates_selected_periods[i]
    ed <- sd + lubridate$days(7)
    p <- df_plot %>%
      filter(between(date, sd, ed)) %>%
      ggplot() +
      theme_bw()+
      geom_line(aes(x = `Local timestamp`, y = `Solar Power [MW]`, color = Type), size = 0.6) +
      labs(y = "Solar Power [MW]", x = "Local Timestamp") +
      theme(
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = ggtext$element_markdown(size = 18),
        axis.text.y = ggtext$element_markdown(size = 18),
        legend.title = ggtext$element_markdown(size = 22),
        legend.title.align = 0.5,
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 22)
      ) +
      guides(color = guide_legend(override.aes = list(size = 7)))
    
    assign(paste0("p", i), p)
      
  }
  
  final_p <- ggpubr$ggarrange(
    p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom"
  )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, "example_forecasts_best_models.pdf"),
      width = 30,
      height = 18,
      units = "cm"
    )
  }
  
  return(final_p)
}

#' @export
plot_daily_metrics_test_set <- function(cumulative = FALSE, save = FALSE) {
  list_selected_models <- list(
    "solar_load_factor_be_fs1_av_lm" = "LR-(a)-average",
    "solar_load_factor_be_fs2_cl5_xgb" = "XGBoost-(b)-5",
    # "solar_load_factor_be_fs2_cl12_rf" = "RF-(b)-12",
    "solar_load_factor_be_fs2_cl12_xgb" = "XGBoost-(b)-12"
  )
  
  df_forecast_result <- names(list_selected_models) %>%
    purrr$map(
      ~ {
        model_name = .x
        dir <- common$dir_model_test_set_object(.x)
        df_result_model_x <- list.files(dir) %>% 
          purrr$map(~ {
            readRDS(file.path(dir, .x))
          }) %>%
          bind_rows() %>%
          distinct(timestamp, .keep_all = TRUE) %>%
          mutate(
            model_name = model_name,
            Model = list_selected_models[[.x]]
          ) %>%
          select(
            timestamp, local_timestamp, date,
            Model, elia_act_generation_solar_belgium, final_forecast
          ) %>%
          rename("actual" = elia_act_generation_solar_belgium)
      }
    ) %>%
    purrr$list_rbind()
  
  df_daily_metrics <- df_forecast_result %>%
    mutate(error = actual - final_forecast) %>%
    group_by(date, Model) %>%
    summarise(
      rmse = sqrt(mean(error^2)),
      mae = mean(abs(error)),
      smape = 2 * mae / mean(actual + final_forecast),
      .groups = "drop"
    )
  
  if (cumulative) {
    df_daily_metrics <- df_daily_metrics %>%
      group_by(Model) %>%
      arrange(date) %>%
      mutate(
        rmse = cumsum(rmse),
        mae = cumsum(mae),
        smape = cumsum(smape)
      )
    
    y_lab_rmse <- "Cumulative RMSE"
    y_lab_smape <- "Cumulative SMAPE"
    filename <- "cumulative_daily_metrics.pdf"
  } else {
    y_lab_rmse <- "RMSE"
    y_lab_smape <- "SMAPE"
    filename <- "daily_metrics.pdf"
  }
  
  p_rmse <- df_daily_metrics %>% 
    select(date, rmse, Model) %>% 
    ggplot() +
    geom_line(aes(x = date, y = rmse, color = Model)) +
    theme_bw() +
    labs(y = y_lab_rmse, x = "Date")
  
  p_smape <- df_daily_metrics %>% 
    select(date, smape, Model) %>% 
    ggplot() +
    geom_line(aes(x = date, y = smape, color = Model)) +
    theme_bw() +
    labs(y = y_lab_smape, x = "Date")
  
  p <- ggpubr$ggarrange(
    p_rmse, p_smape, ncol=2, nrow=1, 
    common.legend = TRUE, legend = "right"
  )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, filename),
      width = 21,
      height = 7,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_feature_type_importance <- function(model_name, save = FALSE) {
  df_shap <- readRDS(common$path_shap_result(model_name))

  label_mapping <- c(
    "azimuth" = "Azimuth", 
    "zenith" = "Zenith", 
    "surface_net_solar_radiation" = "SNR",
    "surface_solar_radiation_downwards" = "SSD", 
    "relative_humidity" = "RH", 
    "total_cloud_cover" = "TTC", 
    "temperature_2m" = "T2m", 
    "wind_chill_index" = "WCI"
  )
  
  df_plot <- df_shap %>%
    group_by(variable) %>%
    summarise(
      importance = mean(Importance),
      .groups = "drop"
    ) %>%
    mutate(
      lon_lat = gsub("[a-z]|2m", "", variable),
      lon_lat = gsub("^_+", "", lon_lat),
      variable_type = stringr$str_replace(variable, paste0("_", lon_lat), ""),
      variable_type = label_mapping[variable_type]
    ) %>%
    group_by(variable_type) %>%
    summarise(
      importance = mean(importance),
      .groups = "drop"
    ) %>%
    mutate(
      variable_type = forcats$fct_reorder(variable_type, importance)
    )
  
  p <- df_plot %>% 
    ggplot(aes(x = importance, y = variable_type)) +  
    geom_col() +
    scale_y_discrete(expand = c(0.05, 0)) +
    scale_x_continuous(expand = c(0.0, 0)) +
    xlab("mean(|SHAP value|)") +
    theme(
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.text.x = ggtext$element_markdown(size = 9, hjust = -0.05),
      axis.text.y = ggtext$element_markdown(size = 9),
      legend.title = ggtext$element_markdown(size = 9),
      legend.title.align = 0.5,
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(size = 7.6),
      legend.key.height = unit(11, "pt")
    ) +
    labs(y = NULL)
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, paste0("feature_type_importance_", model_name, ".pdf")),
      width = 10,
      height = 5,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_variable_importance_heatmap <- function(model_name, save = FALSE){
  label_mapping <- c(
    "surface_net_solar_radiation" = "SNR", 
    "surface_solar_radiation_downwards" = "SSD", 
    "relative_humidity" = "RH", 
    "total_cloud_cover" = "TTC", 
    "temperature_2m" = "T2m", 
    "wind_chill_index" = "WCI"
  )
  
  df_shap <- readRDS(common$path_shap_result(model_name)) 
  
  df_plot <- df_shap %>%
    filter(!grepl("azimuth|zenith", variable)) %>%
    group_by(variable) %>%
    summarise(
      importance = mean(Importance),
      .groups = "drop"
    ) %>%
    mutate(
      lon_lat = gsub("[a-z]|2m", "", variable),
      lon_lat = gsub("^_+", "", lon_lat),
      variable_type = stringr$str_replace(variable, paste0("_", lon_lat), ""),
      variable_type = label_mapping[variable_type],
      lon_lat = factor(lon_lat),
      variable_type = factor(
        variable_type, levels = rev(unname(label_mapping))
      )
    )
  
  p <- df_plot %>%
    ggplot(aes(y = variable_type, x = lon_lat, fill = importance)) +
    geom_tile(show.legend = TRUE) +
    theme_classic() +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = c(0.05, 0)) +
    scale_fill_gradient(name="Mean(|Shap value|)", low= "gray97", high="#0000FF", expand=c(0, 0), limits=c(0, NA)) +
    labs(
      y = "Meteorological feature", 
      x = "Location (Longitude_Latitude)"
    ) +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = ggtext$element_markdown(size= 12, angle = 90, hjust = -0.05),
      axis.text.y = ggtext$element_markdown(size = 12),
      legend.title = ggtext$element_markdown(size = 12),
      legend.title.align = 0.5,
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.height = unit(11, "pt")
    )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, paste0("variable_importance_heatmap_", model_name, ".pdf")),
      width = 14,
      height = 10,
      units = "cm"
    )
  }
  
  return(p)
}
