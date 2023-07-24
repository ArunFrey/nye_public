
#' Loads shp file
#'
#' @param shp_path Path to shp file
#'
#' @return Shp file
#' @export
load_shp <- function(shp_path) {
  
  shp <- st_read(shp_path, quiet = TRUE) %>%
    mutate(community_id = SDV_RS,
           key = AGS) %>%
    select(community_id, key, geometry)
  
  return(shp)
}


#' group districts in shp
#' 
#' Function to merge districts in shp file that need to be joined
#'
#' @param shp shp file
#' @param districts_to_group_shp Nested lists of districts to group
#'
#' @return Grouped shape file
#' @export
group_districts <- function(shp, districts_to_group) {
  
  for (g in districts_to_group) {
    shp <- shp %>%
      mutate(key = case_when(key == g[1] ~ g[2],
                             TRUE ~ key))
  }
  
  shp <- shp %>%
    group_by(key) %>%
    summarise(geometry = sf::st_union(geometry))
  
  return(shp)
}


distance_from_district <- function(shp, district) {
  
  distance <- st_distance(st_centroid(shp[shp$key=="05315", ]), st_centroid(shp))
  
  shp$dist <- NA
  shp$dist <- as.vector(distance)
  # all distances below 10km are set to 10km
  shp$dist[shp$dist<10000] <- 10000
  return(shp)
  
}


process_shp_data <- function(config) {
  
  path <- config$data_config$shp$input
  groups <- config$data_config$shp$districts_to_group
  distance <- config$data_config$shp$distance_from
  
  shp <- load_shp(path) %>%
    group_districts(groups) %>%
    distance_from_district(distance)
  
  return(shp)
  
}