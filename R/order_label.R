order_label <- function(grts_sp, strata = "stratum"){
  
  ord_strata <- unique(grts_sp@data[, strata])
  lengths <- tapply(grts_sp@data$siteID, grts_sp@data[, strata], length)
  plot_order <- sapply(seq_along(ord_strata), function(i) {
    strat <- ord_strata[i]
    n <- lengths[strat]
    1:n
  })
  if (is.list(plot_order)) {
    grts_sp@data$order <- unlist(plot_order)
  } else {
    grts_sp@data$order <- plot_order[, 1]
  }
  
  grts_sp@data
}