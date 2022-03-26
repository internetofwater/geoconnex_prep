get_nwis_sites <- function() {
  
  st_code <- c(paste0("0", c(1:9)), as.character(c(10:98)))
  
  sites <- do.call(rbind, lapply(st_code, function(x) {
    url <- paste0("https://waterservices.usgs.gov/nwis/site/?site_output=expanded&format=rdb&stateCd=", x)
    
    message(url)
    
    try(importRDB1(url))
  }))
  
  sites
}

get_nwis_well_sites <- function(sites) {
  filter(sites, grepl("^GW.*|^sb.*", site_tp_cd) & 
                      !is.na(dec_long_va) & 
                      !is.na(dec_lat_va)) %>%
    select(dec_lat_va, dec_long_va, site_no, station_nm, site_no, 
           nat_aqfr_cd, aqfr_cd, aqfr_type_cd, state_cd,
           well_depth_va, hole_depth_va, alt_va, alt_datum_cd)
}

get_nwis_wells <- function(gw_site) {
  
  gw_site %>%  
    group_by(site_no) %>% 
    arrange(nat_aqfr_cd) %>% # Ensures we select ones that have a national aquifer code
    filter(n() == 1) %>% 
    ungroup() %>%
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>%
    mutate(description = paste0("USGS NWIS Subsurface Site ", site_no, ": ", station_nm),
           subjectOf = paste0("https://waterdata.usgs.gov/monitoring-location/", site_no),
           # uri = paste0("https://geoconnex.us/ref/gages/", n()),
           provider = "https://waterdata.usgs.gov",
           provider_id = site_no,
           national_aquifer = ifelse(!is.na(nat_aqfr_cd), 
                                     paste0("https://geoconnex.us/ref/nat_aq/", 
                                            nat_aqfr_cd), "")) %>%
    select(name = station_nm, 
           description,
           subjectOf,
           provider,
           provider_id,
           national_aquifer)
  
}



get_wbd_gdb <- function(wbd_dir) {
  nhdplusTools::download_wbd(outdir = wbd_dir, url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip")
}

get_gf_11_poi <- function() {
  temp_gdb <- file.path(tempdir(check = TRUE), "GFv1.1.gdb.zip")
  
  sbtools::item_file_download("5e29d1a0e4b0a79317cf7f63", 
                              names = "GFv1.1.gdb.zip", 
                              destinations = temp_gdb, 
                              overwrite_file = TRUE)
  
  zip::unzip(temp_gdb, exdir = tempdir())
  
  temp_gdb <- gsub(".zip", "", temp_gdb)
  
  list(POIs = sf::read_sf(temp_gdb, "POIs_v1_1"), 
       TBto = sf::read_sf(temp_gdb, "TBtoGFv1_POIs"))
}

get_nhdplus_crosswalk <- function() {
  url <- "https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_V1_To_V2_Crosswalk_01.7z"
  
  zip_file <- "data/NHDPlusV21_NationalData_V1_To_V2_Crosswalk_01.7z"
  
  out_dir <- "data/v1_v2/"
  
  if(!dir.exists(out_dir)) {
    httr::GET(url, config = httr::write_disk(zip_file, overwrite = TRUE))
  
    system(paste0("7z -o", normalizePath(out_dir), " e ", normalizePath(zip_file)), intern = TRUE)
  } 
  
  normalizePath(file.path(out_dir, "NHDPlusV1Network_V2Network_Crosswalk.dbf"))
}

get_v2_flowlines <- function() {
  nhd_data <- "data/nhdp/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb"
  if(!dir.exists(nhd_data)) {
    stop(paste("expect nhdp data available at \n", nhd_data))
  }
  sf::read_sf(nhd_data, 
              "NHDFlowline_Network")
}

get_mainstems_db <- function() {
  ms_gpkg <- "data/mainstems/mainstems_summary.gpkg"
  
  if(!file.exists(ms_gpkg)) {
    dir.create(dirname(ms_gpkg), recursive = TRUE, showWarnings = FALSE)
    f <- sbtools::item_file_download("60cb5edfd34e86b938a373f4", names = "mainstems_summary.gpkg", destinations = ms_gpkg)
  }
  
  ms_gpkg
}

download_gnis <- function() {
  
  gnis_zip <- "data/gnis/NationalFile.zip"
  
  if(!file.exists(gnis_zip)) {
    dir.create(dirname(gnis_zip), recursive = TRUE, showWarnings = FALSE)
    f <- download.file("https://geonames.usgs.gov/docs/stategaz/NationalFile.zip", gnis_zip, mode = "wb")
    
    zip::unzip(gnis_zip, exdir = dirname(gnis_zip))
  }
  
  "data/gnis/NationalFile_20210825.txt"
  
}

get_principle_aquifers <- function() {
  aq_zip <- "data/g_aquifr.tar.gz"
  
  if(!file.exists(aq_zip)) {
    download.file("https://water.usgs.gov/GIS/dsdl/aquifers_us.zip", 
                  aq_zip, mode = "wb")
    zip::unzip(aq_zip, exdir = dirname(aq_zip))
    
  }
  "data/us_aquifers.shp"
  
}

get_secondary_gydrogeologic_regions <- function() {
  shr_zip <- "data/SHRs_Conterminous_US_Geodatabase.zip"
  
  if(!file.exists(shr_zip)) {

    f <- sbtools::item_file_download("5a5643b6e4b01e7be24449fc", 
                                     names = basename(shr_zip), 
                                     destinations = shr_zip)
    zip::unzip(shr_zip, exdir = dirname(shr_zip))
  }
  
  "data/Secondary_Hydrogeologic_Regions.gdb"
}

# caching this file in case the html goes away
get_national_aquifer_html <- function(cache_file = "data/nat_aq_links.html") {
  
  if(!file.exists(cache_file)) {
    page <- rvest::read_html("https://water.usgs.gov/ogw/NatlAqCode-reflist.html")
    xml2::write_html(page, cache_file)
  } else {
    page <- rvest::read_html(cache_file)
  }
  
  df <- html_table(page)[[2]]
  
  rows<-html_nodes(page, "table") %>% 
    html_nodes("tr") 
  
  rows <- rows[3: length(rows)]
  
  urls <- bind_rows(lapply(rows, function(x) {
    x <- html_nodes(x, "td")
    row <- lapply(x, function(y) {
      html_attr(html_nodes(y, "a"), "href")
    })
    
    row[lengths(row) == 0] <- ""
    
    out <- as.data.frame(row[1:6])
    out[,7] <- list(row[7])
    
    names(out) <- paste(names(df), "href")
    
    out
  }))
  
  df <- bind_cols(df, urls)
  
}
