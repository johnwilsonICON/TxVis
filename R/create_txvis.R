#' Create a txvis object from raw data.
#'
#' The \code{txvis} object contains encoding for patient, treatment and dates, and can have
#' encoding for additional discrete events.  The class is used throughout the package.
#' 
#' @param patient Patient encoding for treatments.
#' @param treatment Treatment encoding.
#' @param start Start date for treatment in a format defined using \code{date_format}.
#' @param end End date for treatment in a format defined using \code{date_format}.
#' @param date_format Date format, as a character string.  See \code{as.Date}.
#' @param ev_patient A patient encoding for each individual event.
#' @param events Any point event in the treatment of each individual.
#' @param event_date The date of event, formatted as \code{date_format}.
#' @param event_end_date The date that event finished, formatted as \code{date_format}.
#' @param ev_date_format Date format for event dates, as a character string. See \code{as.Date}.
#' 
#' @author Ellen Korol \email{ellen.korol@@iconplc.com}
#' @return This command returns an object of class \code{txvis}.
#'
#' @examples
#'
#'  # `treat` is bundled with the package.
#'  
#'  hlth_data <- create_txvis(patient        = treat$pat_id, 
#'                            treatment      = treat$treatment,
#'                            start          = treat$start,
#'                            end            = treat$end,
#'                            date_format    = "%d%B%Y",
#'                            ev_patient     = events$pat_id,
#'                            events         = events$event,
#'                            event_date     = events$start,
#'                            event_end_date = events$end)
#'                            
#'  hlth_data
#'
#' 
#' @references
#' txvis Package: https://github.com/johnwilsonICON/txvis
#'
#' @keywords utilities
#' @export

create_txvis <- function(patient, 
                         treatment, 
                         start, 
                         end, 
                         date_format,
                         ev_patient = NULL,
                         events = NULL, 
                         event_date = NULL,
                         event_end_date = NULL,
                         ev_date_format = date_format) {

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

  # second element of list - default to NULL  
  if (is.null(ev_patient) | is.null(events)) {
    events <- data.frame()
  } else {
    
    event_date <- as.Date(event_date, date_format) 
    
    event_end_date[is.na(event_end_date)] <- event_date[is.na(event_end_date)]
    
    event_end_date <- as.Date(event_end_date, date_format)
    
    events <- data.frame(ev_pt_id    = ev_patient,
                         event       = events,
                         ev_date     = event_date,
                         ev_end_date = event_end_date)
    
    events <- events[order(event_date), ]
  }
  
  # create class object
  output <- list(treats, events)

  class(output) = c('txvis','list') 

  return(output)

}
