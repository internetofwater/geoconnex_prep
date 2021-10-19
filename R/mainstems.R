make_mainstems <- function(ms_gpkg) {
  ms <- sf::read_sf(ms_gpkg)
  
  ms_out <- dplyr::filter(ms, !is.na(outlet_GNIS_NAME) & !is.na(outlet_latestHUC12)) %>%
    dplyr::group_by(LevelPathI) %>%
    filter(dplyr::row_number() == 1) %>% # May be able to get rid of this .. a bug?
    mutate(id = paste0("https://geoconnex.us/ref/mainstem/", LevelPathI), 
           type = "https://www.opengis.net/def/schema/hy_features/hyf/HY_FlowPath",
           name_at_outlet = outlet_GNIS_NAME,
           name_at_outlet_gnis_id = outlet_GNIS_ID,
           head_nhdpv2_COMID = paste0("https://geoconnex.us/nhdplusv2/comid/", head_nhdpv2_COMID),
           outlet_nhdpv2_COMID = paste0("https://geoconnex.us/nhdplusv2/comid/", outlet_nhdpv2_COMID),
           head_nhdpv2HUC12 = paste0("https://geoconnex.us/nhdplusv2/huc12/", head_nhdpv2HUC12),
           outlet_nhdpv2HUC12 = paste0("https://geoconnex.us/nhdplusv2/huc12/", outlet_nhdpv2HUC12),
           lengthkm = round(length, digits = 1),
           outlet_drainagearea_sqkm = round(totdasqkm, digits = 1)) %>%
    select(id, type, name_at_outlet, name_at_outlet_gnis_id, head_nhdpv2_COMID, outlet_nhdpv2_COMID, 
           head_nhdpv2HUC12, outlet_nhdpv2HUC12, 
           lengthkm, outlet_drainagearea_sqkm,
           head_rf1ID, outlet_rf1ID, 
           head_nhdpv1_COMID, outlet_nhdpv1_COMID, 
           head_latestHUC12, outlet_latestHUC12)
    
  sf::write_sf(ms_out, "temp.gpkg")
}