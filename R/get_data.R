get_nwis_sites <- function() {
  hu02 <- c(paste0("0", c(1:9)), paste0("1", c(0:9)), "20", "21")
  
  sites <- do.call(rbind, lapply(hu02, function(x) whatNWISsites(huc = x)))
}