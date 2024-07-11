box::use(
  load_data = data/`__init__`,
  cpprfast,
  cluster,
  dplyr[...],
  ggplot2[...],
  purrr,
  stats
)

get_distance_matrix_with_haversine <- function(df_coords) {
  n_locations <- NROW(df_coords)
  location_numbers <- seq_len(NROW(df_coords))
  
  m_distance <-  matrix(0, ncol = n_locations, nrow = n_locations)
  rownames(m_distance) <- location_numbers
  colnames(m_distance) <- location_numbers
  
  for (i in location_numbers) {
    for (j in location_numbers) {
      point_i <- c(as.numeric(df_coords[i, 1]), as.numeric(df_coords[i, 2]))
      point_j <- c(as.numeric(df_coords[j, 1]), as.numeric(df_coords[j, 2]))
      dist_ij <- cpprfast$haversine_distance(point_i, point_j)
      
      m_distance[i, j] <- dist_ij
    }
  }
  
  dist_matrix <- stats$as.dist(m_distance)
  
  return(dist_matrix)
}

get_silhouette_score <- function(n_cluster, df_coords) {
  weighted_kmeans <- cpprfast$weighted_kmeans(
    df = df_coords,
    n_cluster = n_cluster,
    n_start = 5,
    max_iter = 1000,
    weight_col_name = NULL,
    seed = 10
  )
  
  dist_matrix <- get_distance_matrix_with_haversine(df_coords = df_coords)
  
  silhouette_scores <- cluster$silhouette(x = weighted_kmeans$clusters, dist = dist_matrix)
  
  return(mean(silhouette_scores[, 3]))
}

#' @export
get_silhouette_score_per_n_cluster <- function(n_clusters) {
  df_weather_locations <- load_data$read_rds("weather/df_weather_locations.rds") %>%
    select(longitude, latitude)
  
  df_ss <- n_clusters %>%
    purrr$map( ~ {
      df_x <- tibble(
        n_cluster = .x,
        silhouette_score = get_silhouette_score(.x, df_coords = df_weather_locations)
      )
    }) %>%
    bind_rows()
  
  return(df_ss)
}

