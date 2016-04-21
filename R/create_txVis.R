#' Create a txVis object from raw data.
#'
#' The \code{txVis} object contains encoding for patient, treatment and dates, and can have
#' encoding for additional discrete events.  The class is used throughout the package.
#'
#' @param x An object returned by one of the \code{get_*} commands for download, site or dataset.
#' @param  ... other objects of the same class.
#' @author Ellen Korol \email{ellen.korol@@iconplc.com}
#' @return This command returns an object of class \code{txVis}.
#'
#' @examples \dontrun{
#'
#'  x <- read.csv( "treat.csv" )
#'  create_txVis( . . . )
#'
#' }
#' @references
#' txVis Package: https://github.com/johnwilsonICON/TxVis
#'
#' @keywords utilities
#' @export

# Notes:
# object.name - the name 

create_txVis <- function( object.name, data, patient, treatment, start, end, date_format, max_seq, events, event_date ) {

  # first element of list
  patient <- as.character( patient ) # opinion on having this as character
  treatment <- as.factor( treatment )
  start <- as.Date( start, date_format)
  end <- as.Date( end,date_format)

  treats <- data.frame( pt_id = patient,
                        tx = treatment,
                        start_date = start,
                        end_date = end , stringsAsFactors =F)
  
  events <- data.frame( pt_id = patient,
                        event_date = event_date )
  
  txVis <- list( treats, events )

  class(name1) = c( 'txVis','data.frame')

  name1 <- name1[with(name1, order(pt_id, start_date)), ]

  #second element of list
  #just doing this for now as a placeholder, but it will be the events data set
  name2 <- data.frame(pt_id = patient,
                     tx = treatment,
                     start_date = start,
                     end_date = end , stringsAsFactors =F)
  class(name2) = c('data.frame')

  #third element of list
  name3 <- max_seq

  name <- list (name1, name2, name3)
  class(name) = c('txVis', 'list')

  return(name)


}

