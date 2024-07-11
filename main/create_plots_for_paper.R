box::use(
  geo_vis = plot/geo_visualisation,
  load_config = config/`__init__`,
  load_data = data/`__init__`,
  result_vis = plot/results,
  ts_vis = plot/time_series,
  window_vis = plot/windows,
  preprocess/elia_solar_power,
  preprocess/total_dataframe,
  dplyr[...]
)

config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-06-30")
SAVE_PLOTS <- TRUE

# time series plots for solar power
df_solar_power <- elia_solar_power$preprocess(
  start_date = start_date,
  end_date = end_date,
  config = selected_config$preprocess
)

ts_vis$plot_solar_power_time_series_over_selected_periods(df_solar_power, save = SAVE_PLOTS)
ts_vis$plot_target_engineering(df_solar_power, save = SAVE_PLOTS)
ts_vis$plot_load_factor(df_solar_power, save = SAVE_PLOTS)

# weather variable related plots
df_averaged_weather <- total_dataframe$preprocess(
  start_date, end_date, config[["solar_load_factor_be_fs2_av_lm"]]$preprocess
)
ts_vis$plot_weather_variables_time_series(df_averaged_weather, save = SAVE_PLOTS)

ts_vis$plot_correlation_heatmap_variables(df_averaged_weather, save = SAVE_PLOTS)

# geo plots
df_included_coords <- load_data$read_rds("weather/df_weather_locations.rds")
geo_vis$plot_included_weather_locations(df_included_coords, save = SAVE_PLOTS)
geo_vis$plot_clusters(df_included_coords, n_clusters = 5, save = SAVE_PLOTS)
geo_vis$plot_clusters(df_included_coords, n_clusters = 12, save = SAVE_PLOTS)

geo_vis$plot_silhouette_score_per_cluster(save = SAVE_PLOTS)

# tune/validation & train/forecast windows
selected_model <- "solar_load_factor_be_fs1_av_rt"
df_init <- total_dataframe$preprocess(
  start_date, end_date, config[[selected_model]]$preprocess
)
window_vis$plot_tune_validation_windows(
  df_init = df_init, 
  config = config[[selected_model]],
  save = SAVE_PLOTS
)
window_vis$plot_train_forecast_windows(
  df_init = df_init, 
  config = config[[selected_model]],
  save = SAVE_PLOTS
)

# plot results
result_vis$plot_forecast_vs_actual_selected_models_over_selected_periods(save = SAVE_PLOTS)
result_vis$plot_daily_metrics_test_set(cumulative = FALSE, save = SAVE_PLOTS)
result_vis$plot_daily_metrics_test_set(cumulative = TRUE, save = SAVE_PLOTS)

result_vis$plot_feature_type_importance(
  model_name = "solar_load_factor_be_fs2_cl12_xgb", save = SAVE_PLOTS
)
geo_vis$plot_location_importance(
  model_name = "solar_load_factor_be_fs2_cl12_xgb", save = SAVE_PLOTS
)
result_vis$plot_variable_importance_heatmap(
  model_name = "solar_load_factor_be_fs2_cl12_xgb", save = SAVE_PLOTS
)

result_vis$plot_feature_type_importance(
  model_name = "solar_load_factor_be_fs2_cl12_rf", save = SAVE_PLOTS
)
geo_vis$plot_location_importance(
  model_name = "solar_load_factor_be_fs2_cl12_rf", save = SAVE_PLOTS
)
result_vis$plot_variable_importance_heatmap(
  model_name = "solar_load_factor_be_fs2_cl12_rf", save = SAVE_PLOTS
)






