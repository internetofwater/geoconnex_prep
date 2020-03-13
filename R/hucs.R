make_nwis_huc_redirects <- function(nwis_sites, out_file) {
  all_hu <- unique(nwis_sites$huc_cd)
  
  hu_08 <- unique(all_hu[nchar(all_hu) >= 8])
  
  hu_06 <- unique(substr(all_hu[nchar(all_hu) >= 6], 1, 6))
  
  hu_04 <- unique(substr(all_hu[nchar(all_hu) >= 4], 1, 4))
  
  hu_02 <- unique(substr(all_hu[nchar(all_hu) >= 2], 1, 2))
  
  hu <- c(hu_02, hu_04, hu_06, hu_08)
  
  out <- tibble::tibble(id = paste0("https://geoconnex.us/usgs/hydrologic-unit/",
                                    hu),
                        target = paste0("https://waterdata.usgs.gov/hydrological-unit/",
                                        hu),
                        creator = "dblodgett@usgs.gov",
                        description = "hydrologic units in waterdata.usgs.gov")
  
  readr::write_csv(out, path = out_file)
  
  out
}