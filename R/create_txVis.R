#' Create a txVis object from raw data.
#'
#' The \code{txVis} object contains encoding for patient, treatment and dates, and can have
#' encoding for additional discrete events.  The class is used throughout the package.
#' @param patient Patient encoding for treatments.
#' @param treatment Treatment encoding.
#' @param start Start date for treatment in a format defined using \code{date_format}.
#' @param end End date for treatment in a format defined using \code{date_format}.
#' @param date_format Date format, as a character string.  See \code{as.Date}.
#' @param ev.patient A patient encoding for each individual event.
#' @param events Any point event in the treatment of each individual.
#' @param event_date The date, formatted as \code{start} or \code{end}.
#' 
#' @author Ellen Korol \email{ellen.korol@@iconplc.com}
#' @return This command returns an object of class \code{txVis}.
#'
#' @examples \dontrun{
#'
#'  data(treat)
#'  hlth_data <- create_txVis(patient   = treat$patient, 
#'                            treatment = treat$treatment,
#'                            start     = treat$start,
#'                            end       = treat$end,
#'                            date_format = "%m %d, %Y")
#'
#' }
#' @references
#' txVis Package: https://github.com/johnwilsonICON/TxVis
#'
#' @keywords utilities
#' @export

# Notes:
# object.name - the name 

create_txVis <- function(patient, 
                         treatment, 
                         start, 
                         end, 
                         date_format,
                         ev_patient = NULL,
                         events = NULL, 
                         event_date = NULL) {

  # first element of list
  patient <- as.character(patient) # opinion on having this as character
  treatment <- as.factor(treatment)
  start <- as.Date(start, date_format)
  end <- as.Date(end, date_format)

  treats <- data.frame(pt_id            = patient,
                       tx               = treatment,
                       start_date       = start,
                       end_date         = end,
                       stringsAsFactors = FALSE)
  
  treats <- treats[order(start), ]
  
  if(is.null(ev_patient) | is.null(events)){
    events <- data.frame()
  } else {
    
    event_date <- as.Date(event_date, date_format)
    
    events <- data.frame(patient    = ev_patient,
                         event      = events,
                         event_date = event_date)
    
    events <- events[order(event_date), ]
  }
  
  output <- list(treats, events)

  class(output) = c('txVis','list')

# SJG: I'm commenting this out.  Also, should we not be using only a single date?
#      or might we expect that some events are multi-day?
#  
#   #second element of list
#   #just doing this for now as a placeholder, but it will be the events data set
# 
#   name2 <- data.frame(pt_id = patient,
#                    tx = treatment,
#                    start_date = start,
#                    end_date = end , stringsAsFactors = FALSE)
# 
#   class(name2) = c('data.frame')
#  
#  output <- list(txVis)
#  
#  class(output) = c('txVis', 'list')

  return(output)

}
