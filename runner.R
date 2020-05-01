library(dataRetrieval)
library(sf)
library(rmapshaper)
library(readr)
library(dplyr)
library(drake)

sourced <- sapply(list.files("R", pattern = "*.R", full.names = TRUE), source)

plan <- drake_plan(nwis_sites = get_nwis_sites(),
                   huc = target(
                     make_nwis_huc_redirects(nwis_sites,
                                             file_out("out/hydrologic-unit.csv"))),
                   wbd_gdb = get_wbd_gdb("data/wbd/"),
                   hu02 = get_hu02(wbd_gdb = wbd_gdb,
                                   hu02_layer = "WBDHU2", 
                                   gnis_base = "https://geonames.usgs.gov/apex/f?p=gnispq:3:::NO::P3_FID:", 
                                   pid_base = "https://geoconnex.us/ref/hu02/", 
                                   out_geojson = file_out("out/hu02.geojson"), 
                                   landing_base = "https://info.geoconnex.us/collections/hu02/items/",
                                   csv_out = file_out("out/hu02.csv")))

make(plan)
