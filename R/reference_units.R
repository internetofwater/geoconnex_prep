get_hu <- function(wbd_gdb, hu_layer, id_attribute, pid_base, 
                   out, landing_base, csv_out, description, 
                   creator = "dblodgett@usgs.gov") {
  
  if(!file.exists(csv_out)) {
  
  hu <- sf::read_sf(wbd_gdb, hu_layer)
  
  hu <- sf::st_make_valid(hu)
  
  check <- sf::sf_use_s2()
  sf_use_s2(FALSE)
  hu <- sf::st_simplify(hu, dTolerance = 0.005)
  sf::sf_use_s2(check)
  
  names(hu)[names(hu) == id_attribute] <- "temp_id"
  
  hu$uri <- paste0(pid_base, hu$temp_id)
  
  hu <- dplyr::select(hu, uri, name, temp_id, loaddate)
  
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
                       c1_match = "f=.*",
                       c1_value = paste0(landing_base,
                                         hu$temp_id,
                                         "?f=${C:f:1}"))

  names(hu)[names(hu) == "temp_id"] <- id_attribute
  
  sf::write_sf(hu, out)
    
  readr::write_csv(out_pid, path = csv_out)
  
  } else {
    hu <- sf::read_sf(out)
  }
  
  hu
}

write_nat_aq <- function(nat_aq, pid_base, landing_base, pa_base, links, out, out_csv) {
  nat_aq <- rmapshaper::ms_simplify(nat_aq)
  
  nat_aq$LINK[nat_aq$AQ_CODE == 610] <- "https://water.usgs.gov/ogw/aquiferbasics/pacnorbr.html"
  nat_aq$NAT_AQFR_CD[nat_aq$AQ_CODE == 610] <- "N100PCFNWV"
  
  nat_aq <- dplyr::group_by(nat_aq, NAT_AQFR_CD) %>%
    dplyr::summarize(ROCK_NAME = ROCK_NAME[1], ROCK_TYPE = ROCK_TYPE[1], AQ_NAME = AQ_NAME[1],
              AQ_CODE = AQ_CODE[1], LINK = LINK[1])
  
  nat_aq$uri <- paste0(pid_base, nat_aq$NAT_AQFR_CD)
  
  nat_aq <- dplyr::filter(nat_aq, !is.na(nat_aq$NAT_AQFR_CD)) %>%
    dplyr::select(uri, LINK, NAT_AQFR_CD, AQ_NAME, AQ_CODE, ROCK_NAME, ROCK_TYPE) %>%
    dplyr::mutate(sameas = paste0(pa_base, AQ_CODE))
  
  nat_aq <- left_join(
    nat_aq,
    select(
      links, Code, valid_states = `Valid States`
    ), by = c("NAT_AQFR_CD" = "Code")
  )
  
  gis_metadata = links$`Archived GIS coverage of outcrop or extent href`
  
  nat_aq$gis_data2 <- nat_aq$gis_metadata2 <- nat_aq$gis_data <- nat_aq$gis_metadata <- ''
  
  for(i in seq_len(nrow(nat_aq))) {
    l <- paste0("https://water.usgs.gov/ogw/", gis_metadata[[i]])
    ll <- length(l)
    if(ll > 0) {
      nat_aq$gis_metadata <- l[1]
    }
    if(ll > 1) {
      nat_aq$gis_data <- l[2]
    } 
    if(ll > 2) {
      nat_aq$gis_metadata2 <- l[3]
    } 
    if(ll > 3) {
      nat_aq$gis_data2 <- l[4]
    }
  }
  
  unlink(out, force = TRUE)
  
  sf::write_sf(nat_aq, out)
  
  nat_aq$uri <- paste0(pid_base, nat_aq$NAT_AQFR_CD)
  
  out <- dplyr::tibble(id = nat_aq$uri,
                       target = paste0(landing_base,
                                       nat_aq$NAT_AQFR_CD),
                       creator = "dblodgett@usgs.gov",
                       description = "National Aquifer Reference",
                       c1_type = "QueryString",
                       c1_match = "f=.*",
                       c1_value = paste0(landing_base,
                                         nat_aq$NAT_AQFR_CD,
                                         "?f=${C:f:1}"))
  
  readr::write_csv(out, path = out_csv)
  
  nat_aq
}

write_pa <- function(pa, pid_base, landing_base, nat_aq, out, out_csv) {
  pa <- rmapshaper::ms_simplify(pa)
  
  pa <- dplyr::group_by(pa, AQ_CODE) %>%
    dplyr::summarize(ROCK_NAME = ROCK_NAME[1], ROCK_TYPE = ROCK_TYPE[1], AQ_NAME = AQ_NAME[1]) %>%
    dplyr::left_join(
      dplyr::select(
        sf::st_drop_geometry(nat_aq),
        AQ_CODE, sameas = uri
      ), by = "AQ_CODE"
    )
  
  pa <- st_cast(pa, "MULTIPOLYGON")
  
  pa$uri <- paste0(pid_base, pa$AQ_CODE)
  
  pa <- dplyr::filter(pa, !is.na(pa$AQ_CODE)) %>%
    select(uri, sameas, AQ_CODE, AQ_NAME, ROCK_NAME, ROCK_TYPE)
  
  unlink(out, force = TRUE)
  
  sf::write_sf(pa, out)
  
  pa$uri <- paste0(pid_base, pa$AQ_CODE)
  
  out <- dplyr::tibble(id = pa$uri,
                       target = paste0(landing_base,
                                       pa$AQ_CODE),
                       creator = "dblodgett@usgs.gov",
                       description = "Principle Aquifer Reference",
                       c1_type = "QueryString",
                       c1_match = "f=.*",
                       c1_value = paste0(landing_base,
                                         pa$AQ_CODE,
                                         "?f=${C:f:1}"))
  
  readr::write_csv(out, path = out_csv)
}

write_shr <- function(shr, pid_base, landing_base, shr_ids, out, out_csv) {
  shr <- rmapshaper::ms_simplify(shr)
  
  shr <- left_join(shr, shr_ids, 
                   by = c("SHR" = "SHR_Name"))
  
  shr$uri <- paste0(pid_base, shr$SHR_CODE)
  shr$id <- shr$SHR_CODE
  
  shr <- select(shr, uri, id, SHR, PrimaryLit, Type, GeologicPr, Subprovinc)
  
  shr <- sf::st_transform(shr, 4326)
  
  unlink(out, force = TRUE)
  
  sf::write_sf(shr, out)
  
  out <- dplyr::tibble(id = shr$uri,
                       target = paste0(landing_base, shr$id),
                       creator = "dblodgett@usgs.gov",
                       description = "Secondary Hydrogeologic Region Reference",
                       c1_type = "QueryString",
                       c1_match = "f=.*",
                       c1_value = paste0(landing_base,
                                         shr$id,
                                         "?f=${C:f:1}"))
  
  readr::write_csv(out, path = out_csv)
}

make_nwis_aq <- function(nwis_well_sites, aq_names, 
                         area_filter = units::set_units(1e12, "m^2"),
                         min_wells = 100) {
  
  old_s2 <- sf_use_s2()
  sf::sf_use_s2(TRUE)
  
  # Get the well sites ready to union.
  nwis_well_sites <- nwis_well_sites %>%
    # must have aquifer code
    filter(!is.null(aqfr_cd) & !is.na(aqfr_cd)) %>%
    # need numeric or NA well depth
    mutate(well_depth_va = ifelse(well_depth_va == ".", NA, well_depth_va)) %>%
    mutate(well_depth_va = as.numeric(well_depth_va)) %>%
    # Don't want some attributes
    # Note we may bring national aquifer code back deliberately later.
    select(-station_nm, -nat_aqfr_cd, -state_cd, -aqfr_type_cd, 
           -hole_depth_va, -alt_va, -alt_datum_cd) %>%
    distinct() %>%
    # convert to a sf table
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

  aq_geom <- nwis_well_sites %>%
    # summarize into a multipoint dataset per aqfr_cd
    group_by(aqfr_cd) %>%
    summarise(wells_in_aq = n(),
              min_well_depth = min(well_depth_va, na.rm = TRUE),
              max_well_depth = max(well_depth_va, na.rm = TRUE),
              do_union = FALSE)
  
  make_hull <- function(x, geom) {
    # if we got a point
    if(length(geom[[x]]) == 2) 
      return(st_buffer(geom[x], units::set_units(100, "m"))[[1]])
    
    # we have something to work with
    geom <- st_convex_hull(geom[x])
    
    # If it's not a polygon
    if(st_geometry_type(geom) != "POLYGON")
      return(st_buffer(geom, units::set_units(100, "m"))[[1]])
      
    geom[[1]]
  }
  
  
  aq <- lapply(1:nrow(aq_geom), make_hull, 
               geom = sf::st_geometry(aq_geom)) %>%
    st_sfc(crs = st_crs(aq_geom)) %>%
    st_cast("POLYGON")
  
  aq_out <- st_sf(st_drop_geometry(aq_geom), 
                  geom = aq)

  aq_out <- aq_out[aq_out$wells_in_aq > min_wells, ] %>%
    sf::st_make_valid() %>%
    mutate(area = st_area(.))

  good_names <- aq_names %>%
    # avoid aquifers that aren't actually aquifers
    filter(!grepl("rocks$|erathem$|system$|systems$", 
                  .data$`Local Aquifer Name`, 
                  ignore.case = TRUE) & 
             .data$`Local Aquifer Code` %in% aq_out$aqfr_cd) %>%
    select(-`State Code`, 
           aqfr_nm = `Local Aquifer Name`, 
           aqfr_cd = `Local Aquifer Code`) %>%
    distinct() %>%
    # There are some duplicate names on these codes.
    group_by(aqfr_cd) %>%
    filter(row_number() == 1)
  
  sf_use_s2(old_s2)
  
  # join to the keepers and filter out massive ones.
  right_join(aq_out, good_names) %>%
    filter(area < area_filter)
  
}
