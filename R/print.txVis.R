#' @export

print.txVis <- function(x, ...) {
  
  # Special print method for txVis:
  
  cat('*** txVis object ***\n\n')
  cat(paste0(nrow(x[[1]]), ' unique treatment/patient pairs.\n'))
  cat(paste0(length(unique(x[[1]]$pt_id)), ' unique patients.\n'))
  cat(paste0(length(unique(x[[1]]$tx)), ' unique treatment types.\n\n'))
  cat(paste0("Date range from ", min(x[[1]]$start_date)," to ", max(x[[1]]$start_date), '\n'))
  if (nrow(x[[2]]) == 0) {
    cat("No coded event data.\n\n")
  } else {
  cat(paste0(nrow(x[[2]]), " coded events over the course of the study set.\n\n"))
  cat(paste0(length(unique(x[[2]]$ev_pt_id )), ' unique patients with events.\n'))
  cat(paste0(length(unique(levels(x[[2]]$events ))), ' types of events.\n'))
  }
  
  print.data.frame(head(x[[1]]))
  cat('        . . .\n')
  print.data.frame(tail(x[[1]]))
}
