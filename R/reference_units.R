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

write_nat_aq <- function(nat_aq, pid_base, landing_base, out_geojson, out_csv) {
  nat_aq <- rmapshaper::ms_simplify(nat_aq)
  
  nat_aq$LINK[nat_aq$AQ_CODE == 610] <- "https://water.usgs.gov/ogw/aquiferbasics/pacnorbr.html"
  nat_aq$NAT_AQFR_CD[nat_aq$AQ_CODE == 610] <- "N100PCFNWV"
  
  nat_aq <- dplyr::group_by(nat_aq, NAT_AQFR_CD) %>%
    dplyr::summarize(ROCK_NAME = ROCK_NAME[1], ROCK_TYPE = ROCK_TYPE[1], AQ_NAME = AQ_NAME[1],
              AQ_CODE = AQ_CODE[1], LINK = LINK[1])
  
  nat_aq$uri <- paste0(pid_base, nat_aq$NAT_AQFR_CD)
  
  nat_aq <- dplyr::filter(nat_aq, !is.na(nat_aq$NAT_AQFR_CD)) %>%
    select(uri, LINK, NAT_AQFR_CD, AQ_NAME, AQ_CODE, ROCK_NAME, ROCK_TYPE)
  
  unlink(out_geojson, force = TRUE)
  
  sf::write_sf(nat_aq, out_geojson)
  
  nat_aq$uri <- paste0(pid_base, nat_aq$NAT_AQFR_CD)
  
  out <- dplyr::tibble(id = nat_aq$uri,
                       target = paste0(landing_base,
                                       nat_aq$NAT_AQFR_CD),
                       creator = "dblodgett@usgs.gov",
                       description = "National Aquifer Reference")
  
  readr::write_csv(out, path = out_csv)
}
