#' @export

print.txvis <- function(x, ...) {
  
  # Special print method for txvis:
  
  cat('*** txvis object ***\n\n')
  cat(paste0(nrow(x[[1]]), ' unique treatment/patient pairs.\n'))
  cat(paste0(length(unique(x[[1]]$pt_id)), ' unique patients.\n'))
  cat(paste0(length(unique(x[[1]]$tx)), ' unique treatment types.\n\n'))
  cat(paste0("Date range from ", min(x[[1]]$start_date)," to ", max(x[[1]]$start_date), '\n'))

  print.data.frame(utils::head(x[[1]]))
  cat('        . . .\n')
  print.data.frame(utils::tail(x[[1]]))

  cat('        \n')  
    if (nrow(x[[2]]) == 0) {
    cat("No coded event data.\n\n")
  } else {
    cat(paste0(nrow(x[[2]]), " coded events over the course of the study set.\n"))
    cat(paste0(length(unique(x[[2]]$ev_pt_id )), ' unique patients with events.\n'))
    cat(paste0(length(unique(levels(x[[2]]$event ))), ' types of events.\n\n'))
   
  print.data.frame(utils::head(x[[2]]))
  cat('        . . .\n')
  print.data.frame(utils::tail(x[[2]]))
  }
}
