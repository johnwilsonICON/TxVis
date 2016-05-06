#' A helper function.
#' @example
#' 
#' 
#' 
#' @export

reform_dates <- function(x, start = NULL, end = NULL, interval = "month"){
  
  # Basic check:
  if (!"txVis" %in% class(x)) {
    stop("You must pass a txVis object.")
  }
  
  # The default will be to use the date range:
  if (is.null(start)) {
    start <- min(x[[1]]$start)
  }
  
  if (is.null(end)) {
    end <- max(x[[1]]$end)
  }
  
  # Generate the sequence of dates:
  out_dates <- seq(start, end, by = interval)
  out_intervals <- lubridate::interval(out_dates[1:length(out_dates) - 1],
                                       out_dates[2:length(out_dates)])
  
  x[[1]]$intervals <- lubridate::interval(x[[1]]$start, x[[1]]$end)
  
  do.call(rbind.data.frame, lapply(1:nrow(x[[1]]), function(i) {
                                   data.frame(pt_id    = as.character(x[[1]]$pt_id[i]),
                                              tx       = as.character(x[[1]]$tx[i]),
                                              interval = out_intervals[int_overlaps(x[[1]]$intervals[i], out_intervals)])
                                }))
  
  test_interval <- function(patient, treatment) {
    treat_co <- subset(x[[1]], pt_id %in% patient & tx %in% treatment)
    
  }
  
  
  lapply(unique(x[[1]]$pt_id), function(pt) {
    patient <- subset(x[[1]], pt_id %in% pt)
    treatment <- rep(NA, length(out_dates))
    
    sequence <- lapply(unique(as.character(patient$tx)), function(tx) {
                        pt_treat <- patient[which(patient$tx %in% tx),]
                        
                        treatment <- rep(NA, length(out_dates))
                        
                        treat <- findInterval(out_dates, 
                                              c(pt_treat$start_date, 
                                                pt_treat$end_date)) == 1
                        treatment[treat] <- tx
                        
                        treatment
                      })
    
  })
  
  list(nodes = nodes, edges = edges)
  
}
