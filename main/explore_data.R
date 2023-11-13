box::use(
  geo_vis = plot/geo_visualisation,
  load_config = config/`__init__`,
  load_data = data/`__init__`,
  ts_vis = plot/time_series,
  preprocess/centroids,
  preprocess/elia_solar_power,
  preprocess/total_dataframe,
  util/check_dataframe,
  corrr,
  dplyr[...],
  utils
)

# load model config
config <- load_config$load_config("solar_models.yml", mode = "default")$included_models
print(names(config))
selected_model <- "solar_load_factor_be_fs2_cl12_xgb"
selected_config <- config[[selected_model]]

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-06-30")

# Analysis of weather locations

df_included_coords <- load_data$read_rds("weather/df_weather_locations.rds")

geo_vis$plot_included_weather_locations(df_included_coords)

geo_vis$plot_clusters(df_included_coords, selected_config$preprocess$n_cluster)

df_selected_coord <- centroids$determine_centroids(
  weighted_kmeans = weighted_kmeans,
  grid_resolution = selected_config$preprocess$grid_resolution
) 

print(df_selected_coord)
print(paste("Number of included weather locations:", NROW(df_included_coords)))

# Analyse solar power time series

df_solar_power <- elia_solar_power$preprocess(
  start_date = start_date,
  end_date = end_date,
  config = selected_config$preprocess
)

print(df_solar_power %>% utils$head(4))
print(df_solar_power %>% utils$tail(4))

ts_vis$plot_solar_power_time_series(df_solar_power, start_date, end_date)
ts_vis$plot_target_engineering(df_solar_power)
ts_vis$plot_load_factor(df_solar_power)

# Analyse weather variables
df_included_weather_vars <- load_data$read_rds(file = "weather/era5_weather.rds")
included_weather_vars <- df_included_weather_vars %>%
  select(-timestamp, -longitude, -latitude) %>%
  colnames()
print(included_weather_vars)

# Analyse 12 cluster model with feature setting 2
df_init <- total_dataframe$preprocess(start_date, end_date, selected_config$preprocess)
df_init %>% check_dataframe$check_na()
print(df_init)

glimpse(df_init)
corr <- df_init %>% select(where(is.numeric)) %>%  corrr$correlate() 
print(corr %>% corrr$as_matrix())
