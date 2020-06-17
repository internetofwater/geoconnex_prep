get_hu <- function(wbd_gdb, hu_layer, id_attribute, gnis_base, pid_base, 
                   out, landing_base, csv_out, description, creator = "dblodgett@usgs.gov") {
  hu <- sf::read_sf(wbd_gdb, hu_layer)
  
  hu <- rmapshaper::ms_simplify(hu, sys = TRUE)
  
  if(!is.na(hu$GNIS_ID)) {
    hu$gnis_url <- paste0(gnis_base,
                            hu$GNIS_ID)
  } else {
    hu$gnis_url <- ""
  }
  
  names(hu)[names(hu) == id_attribute] <- "temp_id"
  
  hu$uri <- paste0(pid_base, hu$temp_id)
  
  hu <- dplyr::select(hu, uri, NAME, gnis_url, GNIS_ID, temp_id, LOADDATE)
  
  hu_level <- nchar(hu$temp_id[1])
  
  if(hu_level > 2) {
    replace <- substr(pid_base, 28, 29)
    replacement <- paste0(0, (as.numeric(replace) - 2))
    
    new_pid_base <- gsub(replace, replacement, pid_base)
      
    hu$containingCatchment <- paste0(new_pid_base, substr(hu$temp_id, 1, (hu_level - 2)))
  }
  
  unlink(out, force = TRUE)
  
  out_pid <- dplyr::tibble(id = hu$uri,
                       target = paste0(landing_base,
                                       hu$temp_id),
                       creator = creator,
                       description = description,
                       c1_type = "QueryString",
                       c1_match = "?f=.*",
                       c1_value = paste0(landing_base,
                                         hu$temp_id,
                                         "?f=${C:f:1}"))

  names(hu)[names(hu) == "temp_id"] <- id_attribute
  
  sf::write_sf(hu, out)
    
  readr::write_csv(out_pid, path = csv_out)
  
  hu
}

write_nat_aq <- function(nat_aq, pid_base, landing_base, out, out_csv) {
  nat_aq <- rmapshaper::ms_simplify(nat_aq)
  
  nat_aq$LINK[nat_aq$AQ_CODE == 610] <- "https://water.usgs.gov/ogw/aquiferbasics/pacnorbr.html"
  nat_aq$NAT_AQFR_CD[nat_aq$AQ_CODE == 610] <- "N100PCFNWV"
  
  nat_aq <- dplyr::group_by(nat_aq, NAT_AQFR_CD) %>%
    dplyr::summarize(ROCK_NAME = ROCK_NAME[1], ROCK_TYPE = ROCK_TYPE[1], AQ_NAME = AQ_NAME[1],
              AQ_CODE = AQ_CODE[1], LINK = LINK[1])
  
  nat_aq$uri <- paste0(pid_base, nat_aq$NAT_AQFR_CD)
  
  nat_aq <- dplyr::filter(nat_aq, !is.na(nat_aq$NAT_AQFR_CD)) %>%
    select(uri, LINK, NAT_AQFR_CD, AQ_NAME, AQ_CODE, ROCK_NAME, ROCK_TYPE)
  
  unlink(out, force = TRUE)
  
  sf::write_sf(nat_aq, out)
  
  nat_aq$uri <- paste0(pid_base, nat_aq$NAT_AQFR_CD)
  
  out <- dplyr::tibble(id = nat_aq$uri,
                       target = paste0(landing_base,
                                       nat_aq$NAT_AQFR_CD),
                       creator = "dblodgett@usgs.gov",
                       description = "National Aquifer Reference",
                       c1_type = "QueryString",
                       c1_match = "?f=.*",
                       c1_value = paste0(landing_base,
                                         nat_aq$NAT_AQFR_CD,
                                         "?f=${C:f:1}"))
  
  readr::write_csv(out, path = out_csv)
}
