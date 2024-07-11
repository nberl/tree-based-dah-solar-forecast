box::use(
  load_data = data/`__init__`,
  model/analyse_clusters,
  model/common,
  preprocess/centroids,
  cpprfast,
  dplyr[...],
  ggplot2[...],
  ggtext,
  readr,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  tidyr
)

#' @export
plot_area <- function(df_coords) {
  world <- rnaturalearth$ne_countries(scale = "medium", returnclass = "sf")
  
  min_longitude <- min(df_coords$longitude) - 0.3
  max_longitude <- max(df_coords$longitude) + 0.3
  min_latitude <- min(df_coords$latitude) - 0.3 
  max_latitude <- max(df_coords$latitude) + 0.3
  
  p <- ggplot() +
    geom_sf(data = world) +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(
      xlim = c(min_longitude, max_longitude), 
      ylim = c(min_latitude, max_latitude), 
      expand = FALSE
    )
  
  return(p)
}

#' @export
plot_included_weather_locations <- function(df_coords, save = FALSE) {
  p <- plot_area(df_coords)
  
  p <- p +
    geom_point(
      aes(x = longitude, y = latitude), 
      shape = 8, 
      alpha = 0.9,
      data = df_coords
    )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, "included_weather_locations.pdf"), 
      width = 12,
      height = 9,
      units = "cm"
    )
  }
  
  return(p)   
}

#' @export
plot_clusters <- function(df_coords, n_clusters, save = FALSE) {
  weighted_kmeans <- cpprfast$weighted_kmeans(
    df = df_coords %>% select(longitude, latitude),
    n_cluster = n_clusters,
    n_start = 5,
    max_iter = 1000,
    weight_col_name = NULL,
    seed = 10
  )
  
  p <- plot_area(df_coords)
  
  df_coords <- df_coords %>%
    mutate(clusters = as.factor(!!weighted_kmeans$clusters))
  
  p <- p +
    geom_point(
      aes(x = longitude, y = latitude, color = clusters), 
      shape = 8, 
      alpha = 0.9,
      data = df_coords
    ) +
    scale_size_area(max_size = 3) +
    guides(color = guide_legend(ncol = 2),
           size = "none")
  
  df_selected_coord <- centroids$determine_centroids(
    weighted_kmeans = weighted_kmeans, 
    grid_resolution = 0.25
  )
  
  p <- p +
    geom_point(
      aes(x = longitude, y = latitude), 
      size = 4,
      data = df_selected_coord
    ) +
    scale_shape_manual(values = c(8, 10)) +    
    theme(
      plot.margin = ggplot2::margin(
        t = 0,  # Top margin
        r = 0,  # Right margin
        b = 0,  # Bottom margin
        l = 5)
    )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, paste0("clusters_", n_clusters,".pdf")),
      width = 12,
      height = 8,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_location_importance <- function(model_name, save = FALSE) {
  df_provinces_belgium <- sf$st_read(dsn = "data/provinces_belgium/BELGIUM_-_Provinces/") %>%
    mutate(
      province = c(
        "Antwerp", "Brussels", "Flemish-Brabant", "Walloon-Brabant", "West-Flanders",
        "East-Flanders", "Hainaut", "Liège", "Limburg", "Luxembourg", "Namur"
      )
    )
  
  df_installed_capacity_per_province <- readr$read_delim(
    "data/solar_power/elia_solar_power_2023-10-30.csv",
    show_col_types = FALSE
  ) %>%
    select(Region, `Monitored capacity`) %>%
    filter(!(Region %in% c("Belgium", "Flanders", "Wallonia"))) %>%
    rename(
      "province" = Region,
      "installed_capacity" = `Monitored capacity`
    ) %>%
    group_by(province) %>%
    summarise(installed_capacity = max(installed_capacity))
  
  df_provinces <- df_provinces_belgium %>%
    left_join(df_installed_capacity_per_province, by = "province")
  
  df_shap_locations <- readRDS(common$path_shap_result(model_name)) %>%
    filter(!grepl("azimuth|zenith", variable)) %>%
    group_by(variable) %>%
    summarise(
      importance = mean(Importance),
      .groups = "drop"
    ) %>%
    mutate(
      lon_lat = gsub("[a-z]|2m", "", variable),
      lon_lat = gsub("^_+", "", lon_lat)
    ) %>%
    tidyr$separate(
      lon_lat, 
      into = c("longitude", "latitude"), 
      sep = "_", 
    ) %>%
    mutate(
      longitude = as.numeric(gsub("_$", "", longitude)),
      latitude = as.numeric(latitude)
    ) %>%
    group_by(longitude, latitude) %>%
    summarise(
      importance = sum(importance), # sum over locations
      .groups = "drop"
    )
  
  df_included_coords <- load_data$read_rds("weather/df_weather_locations.rds")
  
  p <- ggplot() +
    geom_sf(data = df_provinces, aes(fill = installed_capacity), color = "gray") +
    scale_fill_gradient(name="IC [MW]", low = "gray92", high = "#F8766D", expand = c(0,0)) +
    theme(
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      legend.title = ggtext$element_markdown(size = 9),
      legend.title.align = 0.5,
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(size = 7),
      legend.key.height = unit(10, "pt")
    )
  
  p <- p + 
    geom_point(
      aes(x = longitude, y = latitude, size = importance), 
      data = df_shap_locations
    ) +
    scale_shape_manual(values = c(8, 10)) +
    labs(size = expression("Sum(mean(|Shap value|)")) +
    theme(
      legend.title = ggtext$element_markdown(size = 9),
      legend.title.align = 0.5,
      legend.text = element_text(size = 8),
      legend.key.height = unit(8, "pt")
    ) +
    guides(
      colour = guide_legend(order = 1), 
      shape = guide_legend(order = 2)
    )
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, paste0("location_importance_", model_name, ".pdf")),
      width = 14,
      height = 7.5,
      units = "cm"
    )
  }
  
  return(p)
}

#' @export
plot_silhouette_score_per_cluster <- function(save = FALSE) {
  df_ss <- analyse_clusters$get_silhouette_score_per_n_cluster(c(2:20))
  
  p <- df_ss %>%
    ggplot(aes(x = n_cluster, y = silhouette_score)) +
    geom_line() +
    geom_point()
  
  
  folder <- "plot/figures/"
  if (save) {
    ggsave(
      file.path(folder, "silhouette_score_per_cluster.pdf"),
      width = 9,
      height = 7.5,
      units = "cm"
    )
  }
  
  return(p)
}

