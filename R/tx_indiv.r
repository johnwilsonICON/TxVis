#' Generate an icicle plot for treatment data.
#' 
#' @description Using a \code{txvis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @param txvis An object of class \code{txvis}.
#' @param nsample The number of patients to show sequence data for.  Default is 10.
#' @param aligned Should treatment sequences be displayed by date, or from a common origin.  Default is FALSE.
#' @param clustered Organize treatments so that similar treatments are adjacent to one another.  Not implemented.
#' @param events Should individual events be superimposed onto the treatment sequences. Defaults to FALSE.
#' 
#' @return Returns a \code{ggplot2} object.
#' 
#' @examples
#'
#'  hlth_data <- create_txvis(patient        = treat$pat_id, 
#'                            treatment      = treat$treatment,
#'                            start          = treat$start,
#'                            end            = treat$end,
#'                            date_format    = "%d%b%Y",
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
#'  library(ggplot2)
#'  
#'  tx_indiv(hlth_data) + theme_bw()
#'  
#'  # Show many more samples & events, and assign a common start date.
#'  tx_indiv(hlth_data, nsample = 50, aligned = TRUE) +
#'    theme(axis.text.y = element_text(size = 8))
#'  
#' 
#' @export

tx_indiv <- function(txvis, 
                     nsample=NULL, 
                     aligned = FALSE,
                     clustered = FALSE,
                     events = FALSE) {
  
  if (!"txvis" %in% class(txvis)) {
    # Make sure we know what data type we're dealing with.
    stop('You must pass a txvis object.')
  }
  
  nsamp <- ifelse(!is.null(nsample), 
                  nsample, 
                  10)  #defaults to 10 if not entered by user
  
  # If the sample size is smaller than the 
  unique.pid <- unique(txvis[[1]]$pt_id)
  rand.ind <- sample(1:length(unique.pid), nsamp)
  rand.pid <- unique.pid[rand.ind]
  
  # encode the treatments:
  treats <- txvis[[1]]
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
    ggplot2::geom_tile(ggplot2::aes(x = dates, y = pt_id, fill = tx)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"), 
                   panel.border     = ggplot2::element_rect(colour = "black",
                                                            fill = NA,
                                                            size = 1),
                   legend.text  = ggplot2::element_text(size = 14),
                   axis.title.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14)) + 
    ggplot2::labs(fill = "Treatment", 
         x = ifelse(aligned == TRUE, "Days from index date", "Year"),
         y = "Patient ID")
  
  if (class(tx_long_all$dates) == "Date") {
    p <- p + ggplot2::scale_x_date(expand = c(0,0))
  } else {
    p <- p + ggplot2::scale_x_continuous(expand = c(0,0))
  }
    
  if (!is.null(txvis[[2]]) & events == TRUE) {
    # We want to add points to the figure:
    # If events is missing, even if the 
    
    evt <- txvis[[2]]

    colnames(evt)[1] <- "pt_id"
    evt <- evt[evt$pt_id %in% rand.pid,]
    
    evt$ev_date <- as.Date(evt$ev_date, format = "%d-%b-%y")
    
    if (aligned == TRUE) {
      evt <- merge(evt, aggregate(start_date ~ pt_id,
                                        data = treats,
                                        function(x) min(x)),
                      by = "pt_id", all.x = TRUE)
      
      colnames(evt)[5] <- c("index_date")
      evt$ev_date <- evt$ev_date - evt$index_date
      evt$ev_end_date <- evt$ev_end_date - evt$index_date
    }
    
    
    evt1 <- evt[!is.na(evt$ev_end_date),c("pt_id","event","ev_date")]
    evt2 <- evt[!is.na(evt$ev_end_date),c("pt_id","event","ev_end_date")]
    colnames(evt2) <- colnames(evt1)

    evt.los <- rbind(evt1,evt2)
    evt <- evt[is.na(evt$ev_end_date),c("pt_id", "event", "ev_date")]
    evt$pt_id <- as.character(evt$pt_id)
    evt$event <- as.character(evt$event)
    evt.los$pt_id <- as.character(evt.los$pt_id)
    evt.los$event <- as.character(evt.los$event)
    
    un.evt <- unique(evt$event)

    p <- p + 
      ggplot2::geom_point(data = evt, 
                          size = 50 / nsamp, 
                          color = 1, 
                          ggplot2::aes(x = ev_date, y = pt_id, shape = event))
    p <- p + 
      ggplot2::geom_line(data = evt.los, color = 1,
                          ggplot2::aes(x = ev_date, y = pt_id, linetype = event))

  } #end if events exist

  return(p)
  
}