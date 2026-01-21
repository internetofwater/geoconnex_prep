# Packages used by targets during build
library(targets)
library(tarchetypes) # optional, but handy for patterns/cues

# Packages your targets rely on during execution
tar_option_set(packages = c(
  "dataRetrieval",
  "sf",
  "rmapshaper",
  "readr",
  "dplyr",
  "rvest",
  "units"
))

# Global setup (runs once before the pipeline)
sf::sf_use_s2(FALSE)

# Ensure mapshaper is available (fail early, like your original check)
if (!"mapshaper-xl" %in% names(rmapshaper::check_sys_mapshaper())) {
  stop("rmapshaper 'mapshaper-xl' must be available on the system PATH.")
}

targets::tar_source()

# Make sure the output directory exists
if (!dir.exists("out")) dir.create("out", recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Pipeline
# -------------------------------------------------------------------------
list(
  
  # # NWIS wells workflow ----------------------------------------------------
  # tar_target(nwis_sites, get_nwis_sites()),
  # tar_target(nwis_well_sites, get_nwis_well_sites(nwis_sites)),
  # tar_target(nwis_wells, get_nwis_wells(nwis_well_sites)),
  
  tar_target(
    nwis_wells_out,
    {
      sf::write_sf(nwis_wells, "out/nwis_wells.geojson")
      "out/nwis_wells.geojson"
    },
    format = "file"
  ),
  
  # POIs / NHDPlus v2 ------------------------------------------------------
  tar_target(POIs, get_gf_11_poi()),
  tar_target(v1_v2, get_nhdplus_crosswalk()),
  tar_target(nhdplusv2_flines, get_v2_flowlines()),
  tar_target(nhdplus2_attributes, sf::st_drop_geometry(nhdplusv2_flines)),
  
  tar_target(gf11_pois, index_pois(POIs, v1_v2, flines = nhdplusv2_flines)),
  
  # HUC redirects (CSV file output) ---------------------------------------
  tar_target(
    huc,
    {
      make_nwis_huc_redirects(nwis_sites, "out/hydrologic-unit.csv")
      "out/hydrologic-unit.csv"
    },
    format = "file"
  ),
  
  # WBD geodatabase (treated as file dependency) --------------------------
  tar_target(wbd_gdb, get_wbd_gdb("data/wbd/"), format = "file"),
  
  # HU02/04/06/08/10 writers (each returns two files) ---------------------
  tar_target(
    hu02,
    {
      get_hu(
        wbd_gdb = wbd_gdb,
        hu_layer = "WBDHU2",
        id_attribute = "huc2",
        pid_base = "https://geoconnex.us/ref/hu02/",
        out = "out/hu02.gpkg",
        landing_base = "https://reference.geoconnex.us/collections/hu02/items/",
        csv_out = "out/hu02.csv",
        description = "two digit hydrologic units reference",
        creator = "dblodgett@usgs.gov"
      )
      c("out/hu02.gpkg", "out/hu02.csv")
    },
    format = "file"
  ),
  
  tar_target(
    hu04,
    {
      get_hu(
        wbd_gdb = wbd_gdb,
        hu_layer = "WBDHU4",
        id_attribute = "huc4",
        pid_base = "https://geoconnex.us/ref/hu04/",
        out = "out/hu04.gpkg",
        landing_base = "https://reference.geoconnex.us/collections/hu04/items/",
        csv_out = "out/hu04.csv",
        description = "four digit hydrologic units reference",
        creator = "dblodgett@usgs.gov"
      )
      c("out/hu04.gpkg", "out/hu04.csv")
    },
    format = "file"
  ),
  
  tar_target(
    hu06,
    {
      get_hu(
        wbd_gdb = wbd_gdb,
        hu_layer = "WBDHU6",
        id_attribute = "huc6",
        pid_base = "https://geoconnex.us/ref/hu06/",
        out = "out/hu06.gpkg",
        landing_base = "https://reference.geoconnex.us/collections/hu06/items/",
        csv_out = "out/hu06.csv",
        description = "six digit hydrologic units reference",
        creator = "dblodgett@usgs.gov"
      )
      c("out/hu06.gpkg", "out/hu06.csv")
    },
    format = "file"
  ),
  
  tar_target(
    hu08,
    {
      get_hu(
        wbd_gdb = wbd_gdb,
        hu_layer = "WBDHU8",
        id_attribute = "huc8",
        pid_base = "https://geoconnex.us/ref/hu08/",
        out = "out/hu08.gpkg",
        landing_base = "https://reference.geoconnex.us/collections/hu08/items/",
        csv_out = "out/hu08.csv",
        description = "eight digit hydrologic units reference",
        creator = "dblodgett@usgs.gov"
      )
      c("out/hu08.gpkg", "out/hu08.csv")
    },
    format = "file"
  ),
  
  tar_target(
    hu10,
    {
      get_hu(
        wbd_gdb = wbd_gdb,
        hu_layer = "WBDHU10",
        id_attribute = "huc10",
        pid_base = "https://geoconnex.us/ref/hu10/",
        out = "out/hu10.gpkg",
        landing_base = "https://reference.geoconnex.us/collections/hu10/items/",
        csv_out = "out/hu10.csv",
        description = "ten digit hydrologic units reference",
        creator = "dblodgett@usgs.gov"
      )
      c("out/hu10.gpkg", "out/hu10.csv")
    },
    format = "file"
  )
  
  # # National aquifers (NGWMN WFS + writers) -------------------------------
  # tar_target(
  #   ngwmn_wfs_call,
  #   "https://cida.usgs.gov/ngwmn/geoserver/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName=ngwmn:aquifrp025&outputFormat=application%2Fjson"
  # ),
  # 
  # tar_target(
  #   nwis_aq_names,
  #   {
  #     tbls <- rvest::html_table(rvest::read_html("https://help.waterdata.usgs.gov/aqfr_cd"))
  #     as.data.frame(tbls[[1]])
  #   }
  # ),
  # 
  # tar_target(
  #   nwis_aq,
  #   make_nwis_aq(
  #     nwis_well_sites,
  #     nwis_aq_names,
  #     area_filter = units::set_units(1e12, "m^2"),
  #     min_wells = 100
  #   )
  # ),
  # 
  # tar_target(nat_aq, sf::read_sf(ngwmn_wfs_call)),
  # 
  # tar_target(nat_aq_links, get_national_aquifer_html()),
  # 
  # tar_target(
  #   nat_aq_out,
  #   {
  #     write_nat_aq(
  #       nat_aq,
  #       pid_base = "https://geoconnex.us/ref/nat_aq/",
  #       landing_base = "https://reference.geoconnex.us/collections/nat_aq/items/",
  #       pa_base = "https://geoconnex.us/ref/principal_aq/",
  #       links = nat_aq_links,
  #       out = "out/nat_aq.gpkg",
  #       out_csv = "out/nat_aq.csv"
  #     )
  #     c("out/nat_aq.gpkg", "out/nat_aq.csv")
  #   },
  #   format = "file"
  # ),
  # 
  # # Principal aquifers -----------------------------------------------------
  # tar_target(pa_shp, get_principal_aquifers(), format = "file"),
  # tar_target(principal_aquifer, sf::read_sf(pa_shp)),
  # 
  # tar_target(
  #   pa_out,
  #   {
  #     write_pa(
  #       principal_aquifer,
  #       pid_base = "https://geoconnex.us/ref/principal_aq/",
  #       landing_base = "https://reference.geoconnex.us/collections/principal_aq/items/",
  #       nat_aq_out, # dependency on nat_aq outputs
  #       out = "out/princi_aq.gpkg",
  #       out_csv = "out/princi_aq.csv"
  #     )
  #     c("out/princi_aq.gpkg", "out/princi_aq.csv")
  #   },
  #   format = "file"
  # ),
  # 
  # # Secondary hydrogeologic regions ---------------------------------------
  # # Track the input CSV as a file dependency to rerun when it changes
  # tar_target(shr_ids_file, "data/SHR_ID.csv", format = "file"),
  # tar_target(shr_ids, readr::read_csv(shr_ids_file)),
  # 
  # tar_target(shr_gdb, get_secondary_gydrogeologic_regions(), format = "file"),
  # tar_target(secondary_hydrogeologic_regions, sf::read_sf(shr_gdb, "Secondary_Hydrogeologic_Regions")),
  # 
  # tar_target(
  #   shr_out,
  #   {
  #     write_shr(
  #       secondary_hydrogeologic_regions,
  #       pid_base = "https://geoconnex.us/ref/sec_hydrg_reg/",
  #       landing_base = "https://reference.geoconnex.us/collections/sec_hydrg_reg/items/",
  #       shr_ids = shr_ids,
  #       out = "out/sec_hydrg_reg.gpkg",
  #       out_csv = "out/sec_hydrg_reg.csv"
  #     )
  #     c("out/sec_hydrg_reg.gpkg", "out/sec_hydrg_reg.csv")
  #   },
  #   format = "file"
  # ),
  # 
  # # GNIS writer (file output) ---------------------------------------------
  # tar_target(
  #   gnis,
  #   {
  #     get_gnis(gnis_f, "out/gnis.gpkg")
  #     "out/gnis.gpkg"
  #   },
  #   format = "file"
  # )
)
