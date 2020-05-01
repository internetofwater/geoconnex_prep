get_hu02 <- function(wbd_gdb, hu02_layer, gnis_base, pid_base, 
                     out_geojson, landing_base, csv_out) {
  
  hu02 <- sf::read_sf(wbd_gdb, hu02_layer)
  
  hu02 <- rmapshaper::ms_simplify(hu02)
  
  hu02$gnis_url <- paste0(gnis_base,
                          hu02$GNIS_ID)
  
  hu02$uri <- paste0(pid_base, hu02$HUC2)
  
  hu02 <- dplyr::select(hu02, uri, NAME, gnis_url, GNIS_ID, HUC2, LOADDATE)
  
  unlink(out_geojson, force = TRUE)
  
  sf::write_sf(hu02, out_geojson)
  
  out <- dplyr::tibble(id = hu02$uri,
                        target = paste0(landing_base,
                                        hu02$HUC2),
                        creator = "dblodgett@usgs.gov",
                        description = "two digit hydrologic units reference")
  
  readr::write_csv(out, path = csv_out)
}
