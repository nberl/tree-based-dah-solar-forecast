box::use(
  dplyr[...],
  maps,
  purrr,
  tidyr
)

get_country <- function(df_coords) {
  country <- purrr$map2_chr(
    .x = df_coords$longitude,
    .y = df_coords$latitude,
    .f = ~ {
      return(tolower(maps$map.where(x = .x, y = .y)))
    })
  
  return(country)
}

#' @export
determine_locations <- function(df_coords) {
  min_longitude <- floor(min(df_coords$longitude) - 0.3)
  max_longitude <- ceiling(max(df_coords$longitude) + 0.3)
  min_latitude <- floor(min(df_coords$latitude) - 0.3) 
  max_latitude <- ceiling(max(df_coords$latitude) + 0.3)
  
  df_full_grid <- tidyr$expand_grid(
    longitude = seq(min_longitude, max_longitude, by = 0.25),
    latitude = seq(min_latitude, max_latitude, by = 0.25)
  ) %>%
    mutate(country = get_country(.))
  
  df_selected_locations <- df_full_grid %>%
    filter(country == "belgium")
  
  return(df_selected_locations)
}

