library(dataRetrieval)
library(drake)

sourced <- sapply(list.files("R", pattern = "*.R", full.names = TRUE), source)

plan <- drake_plan(nwis_sites = get_nwis_sites())

make(plan)
