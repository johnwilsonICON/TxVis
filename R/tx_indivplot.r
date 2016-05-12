#' Generate an icicle plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @import ggplot2
#' @param txVis An object of class \code{txVis}.
#' @param nsample The number of patients to show sequence data for.
#' 
#' @export

tx_indiv <- function(txVis, nsample=NULL, 
                     aligned = FALSE,
                     clustered = FALSE) {
  
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
  
  treats_ind <- treats[rep(row.names(treats), treats$dur),]  #creates a row for each data point
  
  tmp <- aggregate(days_from_index~pt_id, 
                   data = treats, function(x) min(x))
  
  treats_ind["days"] <- 0
  
  do.call(rbind, lapply(1:nrow(treats)))
  
  for (i in 2:nrow(treats_ind)) {
    if (treats_ind[i,"pt_id"] == treats_ind[i - 1,"pt_id"]) {
      treats_ind[i,"days"] <- treats_ind[i - 1, "days"] + 1
    }
  }
  
  # Event processing:
  evt <- events
  evt$event_date <- as.Date(evt$event_date,format = "%d-%b-%y")
  evt$patient <- as.character(evt$patient)
  evt$events <- as.character(evt$events)

  colnames(evt)[c(1, 4)] <- c("pt_id", "event") #will delete when fixed.
  evt <- evt[evt$pt_id %in% rand.pid,]
  un.evt <- unique(evt$event)
  
  evt <- merge(evt, unique(treats[ , c("pt_id", "index_date")]), 
               by = "pt_id", all.x = TRUE)
  evt["evt_day"] <- as.numeric(evt$event_date - evt$index_date)
  
  if (nrow(evt) > 0) { evt[evt$evt_day < 0, "evt_day"] <- NA }
  
  colors <- colorRampPalette(c("dark blue", "white"))(length(unique(treats_ind$tx)))
  
  p <- ggplot(treats_ind) + 
    geom_tile(aes(x = days, y = pt_id, fill = tx)) +
    scale_fill_manual(values = colors) + 
    theme_bw()
  
  if (length(un.evt) == 0) { return(p) }

  if (length(un.evt) == 1) { 
    p <- p + 
      geom_point(data = evt[evt$event == un.evt[1],],
                 aes(x = evt_day, y = pt_id), 
                 shape = 1, size = 4)  #will make size a function of n.
  }
  if (length(un.evt) == 2) { 
    p <- p + 
      geom_point(data = evt[evt$event == un.evt[1],],
                 aes(x = evt_day, y = pt_id),
                 shape = 1, size = 4) + #will make size a function of n.
      geom_point(data = evt[evt$event == un.evt[2],],
                 aes(x = evt_day, y = pt_id), 
                 shape = 2, size = 4)  #will make size a function of n.
  } 
  
  return(p)
  #currently hard-coding in 2 event types but will need to make flexible.
}


