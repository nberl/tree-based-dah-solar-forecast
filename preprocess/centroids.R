box::use(
  dplyr[...]
)

round_to_nearest_multiple <- function(x, multiple) {
  return(multiple * round(x / multiple))
}

#' @export
determine_centroids <- function(weighted_kmeans, grid_resolution) {
  df_centroid <- tibble(
    longitude = weighted_kmeans$centroids[, 1],
    latitude = weighted_kmeans$centroids[, 2]
  ) %>%
    mutate(across(everything(), ~ round_to_nearest_multiple(., multiple = !!grid_resolution))) %>%
    distinct()
  
  return(df_centroid)
}