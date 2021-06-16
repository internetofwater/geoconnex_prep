index_pois <- function(POIs, v1_v2, flines) {
  transboundary <- POIs$TBto
  
  POIs <- POIs$POIs
  
  # GF11 in a USGS namespace
  uri <- paste0("https://geoconnex.us/usgs/nhm_gf11/", POIs$seg_id_nhm)
  
  lookup <- foreign::read.dbf(normalizePath(v1_v2), as.is = TRUE)
  
  POIs <- left_join(POIs, select(lookup, V2_ComID, V1_ComID, XWalkType), 
                    by = c("NHDPlusID" = "V1_ComID"))  
  
  NA_POIs <- filter(POIs, is.na(XWalkType))
  
  NA_POI_index <- nhdplusTools::get_flowline_index(flines, 
                                                   sf::st_zm(NA_POIs), 
                                                   search_radius = 0.1)
  
  NA_POIs$id <- seq_len(nrow(NA_POIs))
  
  NA_POIs <- left_join(NA_POIs, NA_POI_index, by = "id")
  
  NA_POIs <- select(NA_POIs, -id)
  
  not_1_1_POIs <- filter(POIs, !is.na(XWalkType) & XWalkType != "1-1")
  
  not_1_1_POIs_index <- nhdplusTools::get_flowline_index(
    filter(flines, COMID %in% not_1_1_POIs$V2_ComID),
    not_1_1_POIs, 
    search_radius = 0.1
  )
  
  not_1_1_POIs <- bind_cols(not_1_1_POIs, select(not_1_1_POIs_index, -id))
  
  ## Seg ID NHM is a sequence with some funny replacement near the border
  mapped_POIs <- filter(POIs, !seg_id_nhm %in% c(NA_POIs$seg_id_nhm, 
                                                 not_1_1_POIs$seg_id_nhm))
  
  mapped_POIs_index <- nhdplusTools::get_flowline_index(
    filter(flines, COMID %in% mapped_POIs$V2_ComID),
    mapped_POIs, 
    search_radius = 0.1
  )
  
  mapped_POIs <- bind_cols(mapped_POIs, select(mapped_POIs_index, -id))

  
  really_no_match <- filter(NA_POIs, is.na(COMID))
  
  
  bind_rows(mapped_POIs, not_1_1_POIs, mapped_POIs)
}

make_poi_ld <- function(gf11_pois, nhdplus2_attributes) {
  
  nhdplus2_attributes <- select(nhdplus2_attributes, COMID, GNIS_ID, StreamOrde, TotDASqKM)
  
  gf11_pois <- left_join(gf11_pois, nhdplus2_attributes, by = c("V2_ComID" = "COMID"))
  
  # GNIS
  # nhdplusv21 comid
  # NWIS gage
  
  out <- select(gf11_pois, seg_id_nhm, GNIS_ID, GNIS_NAME, V2_ComID, V1_ComID = NHDPlusID, gage = Type_Gage, NHD_Unit, REACHCODE, REACH_meas, offset, StreamOrde, TotDASqKM)
  
}