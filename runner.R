library(dataRetrieval)
library(drake)

sourced <- sapply(list.files("R", pattern = "*.R", full.names = TRUE), source)

plan <- drake_plan(nwis_sites = get_nwis_sites(),
                   huc = target(
                     make_nwis_huc_redirects(nwis_sites,
                                             file_out("out/hydrologic-unit.csv"))))

make(plan)
