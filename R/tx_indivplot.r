#' Generate an icicle plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @import ggplot2
#' @param txVis An object of class \code{txVis}.
#' @param nsample The number of patients to show sequence data for.  Default is 10.
#' @param aligned Should treatment sequences be displayed by date, or from a common origin.  Default is FALSE.
#' @param clustered Organize treatments so that similar treatments are adjacent to one another.  Not implemented.
#' @param events Should individual events be superimposed onto the treatment sequences. Defaults to FALSE.
#' 
#' @return Returns a `ggplot2` object.
#' 
#' @examples
#'
#'  hlth_data <- create_txVis(patient        = treat$pat_id, 
#'                            treatment      = treat$treatment,
#'                            start          = treat$start,
#'                            end            = treat$end,
#'                            date_format    = "%B %d, %Y",
#'                            ev_patient     = events$pat_id,
#'                            events         = events$event,
#'                            event_date     = events$start,
#'                            event_end_date = events$end)
#'                            
#'  # Plot without events:                          
#'  tx_indiv(hlth_data)
#'  
#'  # Plot with event data:
#'  tx_indiv(hlth_data, events = TRUE)
#'  
#'  #  Add additional ggplot2 styline:
#'  tx_indiv(hlth_data) + theme_bw()
#' 
#' @export

tx_indiv <- function(txVis, 
                     nsample=NULL, 
                     aligned = FALSE,
                     clustered = FALSE,
                     events = FALSE) {
  
  nsamp <- ifelse(!is.null(nsample), 
                  nsample, 
                  10)  #defaults to 10 if not entered by user
  
  # If the sample size is smaller than the 
  unique.pid <- unique(txVis[[1]]$pt_id)
  rand.ind <- sample(1:length(unique.pid), nsamp)
  rand.pid <- unique.pid[rand.ind]
  
  # encode the treatments:
  treats <- txVis[[1]]
  treats <- treats[treats$pt_id %in% rand.pid,]
  
  treats <- merge(treats, aggregate(start_date~pt_id,
                                    data = treats,
                                    function(x) min(x)),
                  by = "pt_id", all.x = TRUE)
  
  colnames(treats)[c(3,5)] <- c("start_date", "index_date")
  treats <- treats[order(treats[,"pt_id"], treats[,"start_date"]),]
  
  treats["dur"] <- as.numeric(treats$end_date - treats$start_date) + 1
  treats["days_from_index"] <- as.numeric(treats$start_date - treats$index_date)

  tmp <- aggregate(days_from_index~pt_id, 
                   data = treats, function(x) min(x))

  tx_long_all <- do.call(rbind, lapply(unique(treats$pt_id), 
                                        function(x) {
                                          tx <- subset(treats, pt_id == x)
                                          
                                          if (aligned == TRUE) { 
                                            tx$end_date <- as.numeric(tx$end_date - min(tx$start_date))
                                            tx$start_date <- as.numeric(tx$start_date - min(tx$start_date))
                                          }
                                          
                                          
                                          tx_long <- data.frame(pt_id = x,
                                                                dates = seq(from = min(tx$start_date), 
                                                                            to = max(tx$end_date), by = 1))
                                          tx_long$tx <- NA
                                          
                                          for (i in 1:nrow(tx)) {
                                            tx_long$tx[findInterval(tx_long$dates, tx[i,c('start_date', 'end_date')]) == 1] <- as.character(tx$tx[i])
                                          }
                                          tx_long
                                        }))
                  
  
  # Event processing:
  
  #colors <- colorRampPalette(c("dark blue", "white"))(length(unique(tx_long_all$tx)))
  
  p <- ggplot2::ggplot(tx_long_all) + 
    ggplot2::geom_tile(aes(x = dates, y = pt_id, fill = tx)) +
    labs(fill="Treatment",x=if(aligned==T) {"Days from index date"} else {"Year"},y="Patient ID")

  if (!is.null(txVis[[2]]) & events == TRUE) {
    # We want to add points to the figure:
    # If events is missing, even if the 
    
    evt <- txVis[[2]]
    evt$ev_date <- as.Date(evt$ev_date,format = "%d-%b-%y")
    
    if (aligned == TRUE) {
      evt$ev_date <- evt$ev_date - min(txVis[[1]]$start_date)
    }
    
    evt$ev_pt_id <- as.character(evt$ev_pt_id)
    evt$event <- as.character(evt$event)
    
    evt <- evt[evt$ev_pt_id %in% rand.pid,]
    un.evt <- unique(evt$event)
    
    p <- p + 
      ggplot2::geom_point(data = evt,
                          aes(x = ev_date, y = ev_pt_id, color = event))
    }
  
  return(p)
  
}