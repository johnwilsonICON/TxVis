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

create_txVis <- function(object.name, data, pt_id, tx, start_date, end_date ) {
  
  # Ensure that the patient ID is a factor:
  pt_id <- as.factor( pt_id )
  tx <- as.factor( tx )
  start_date <- as.Date( start_date )
  end_date <- as.Date( end_date)
  
  name <- data.frame(patient = pt_id,
                     treatment = tx,
                     start = start_date,
                     end = end_date )

  class(name) = c('txVis', 'data.frame')
  
  return(name)
  
}
