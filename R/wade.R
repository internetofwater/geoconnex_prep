make_wade_nldi <- function(wade_sites, out_geojson) {
  if(!file.exists(out_geojson)) {
  
  wade_sites <- wade_sites %>%
    mutate(feature_name = paste("State:", StateID, 
                                "Source:", DataSourceOrganizationID,
                                "ID:", id)) %>%
    select(feature_id = id, feaure_name = feature_name, feature_uri = uri)
  
  sf::write_sf(wade_sites, out_geojson)
  
  } 
  
  wade_sites
}

crawl_wade <- function(wade_uri, cores) {
  cl <- parallel::makeCluster(rep("localhost", cores), type = "SOCK")
  
  out <- pbapply::pblapply(wade_uri, jsonlite::read_json, cl)
  
  parallel::stopCluster(cl)
  
  out
}