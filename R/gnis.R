get_gnis <- function(gnis_f, out_f) {
  
  # metadata:
  # https://geonames.usgs.gov/docs/metadata/gnis.txt
  # https://geonames.usgs.gov/docs/pubs/Nat_State_Topic_File_formats.pdf
  gnis_d <- data.table::fread(gnis_f, quote = "")
  
  gnis <- gnis_d %>%
    sf::st_as_sf(coords = c("PRIM_LONG_DEC", "PRIM_LAT_DEC"), 
                 crs = 4326, remove = FALSE) %>%
    select(gnis_id = FEATURE_ID,
           name = FEATURE_NAME, # https://schema.org/name
           gnis_type = FEATURE_CLASS,
           state = STATE_ALPHA,
           state_id = STATE_NUMERIC, 
           county = COUNTY_NAME,
           county_id = COUNTY_NUMERIC, 
           elev_m = ELEV_IN_M,
           dateCreated = DATE_CREATED, # https://schema.org/dateCreated
           dateModified = DATE_EDITED, # https://schema.org/dateModified
           longitude = PRIM_LONG_DEC, # # https://schema.org/longitude
           latitude = PRIM_LAT_DEC) %>% # https://schema.org/latitude
    mutate(state_id = sprintf("%02d", state_id),
           county_id = sprintf("%03d", county_id)) %>%
    mutate(uri = paste0("https://geoconnex.us/ref/gnis/", gnis_id), # for @id
           sameAs = paste0("https://geonames.usgs.gov/apex/f?p=gnispq:3:::NO::P3_FID:", gnis_id), # https://schema.org/sameAs
           geoWithin_state = paste0("https://geoconnex.us/ref/states/", state_id), # https://schema.org/geoWithin
           geoWithin_county = paste0("https://geoconnex.us/ref/counties/", state_id, county_id), # https://schema.org/geoWithin
           dateCreated = format(as.Date(dateCreated, "%d/%m/%Y"), "%Y-%m-%d"),
           dateModified = format(as.Date(dateModified, "%d/%m/%Y"), "%Y-%m-%d"),
           type = "https://schema.org/Place") %>% # for @type 
    select(uri, type, name, gnis_id, gnis_type, 
           longitude, latitude, elev_m,
           state, state_id, county, county_id, 
           geoWithin_state, geoWithin_county, 
           dateCreated, dateModified)
  
  sf::write_sf(gnis, out_f)
  
  zip::zip(paste0(out_f, ".zip"), out_f)
  
  out_f
}
