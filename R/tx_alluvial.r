#' Generate an alluvial plot for treatment data.
#' 
#' @description Using a txvis object, plot the sequencing of treatments using an alluvial plot.
#' 
#' @param txvis An object of class \code{txvis}.
#' @param nsequ The maximum number of sequences to plot.
#' @param sequence Boolean.  Should treatments plot by sequence (\code{TRUE}) or by date.
#' @param start Arguments to pass to \code{reform_dates} or \code{reform_seq}.
#' @param end Arguments to pass to \code{reform_dates} or \code{reform_seq}.
#' @param interval Arguments to pass to \code{reform_dates} or \code{reform_seq}.  Default is 4 months.
#' @param conflict Arguments to pass to \code{reform_dates} or \code{reform_seq}.
#' @param tx_cw Width of the category axis.
#' @param ... Arguments to pass to the \code{alluvial} function.
#' 
#' @return NULL
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
#'  tx_alluvial(hlth_data)
#'  
#' @export


tx_alluvial <- function(txvis,
                        nsequ = NULL,
                        sequence = TRUE,
                        start = NULL, 
                        end = NULL,
                        interval = "4 months", 
                        conflict = "majority",
                        tx_cw = 0.05,
                        ...) {
  
  if (!requireNamespace("alluvial", quietly = TRUE)) {
    message("This function requires the non-CRAN package `alluvial` installed from GitHub.")
    user_inp <-  readline(prompt = "Do you want to install this package? (y/n)")
    if (user_inp == "y") {
      devtools::install_github("mbojan/alluvial")
      requireNamespace(alluvial)
    } else {
      stop(paste0("You must install the package `alluvial` for this function to work.\n",
                  " You can install the package directly from GitHub using:\n\n",
                  "> library(devtools)\n> devtools::install_github('mbojan/alluvial')"))
    }
  }

  if (sequence == TRUE) {
    txvis.ref <- data.frame(t(apply(reform_seq(txvis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  } else {
    txvis.ref <- data.frame(t(apply(reform_dates(txvis, nsequ, 
                                                 start, end, 
                                                 interval, conflict), 
                                    1, 
                                    function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  }
  
  seq.cols <- paste0(rep("seq_", (ncol(txvis.ref) - 1)),
                      c(1:(ncol(txvis.ref) - 1)))
  
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  input_agged_seq <- stats::aggregate(data = txvis.ref, 
                               stats::as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     
  
  
  #input_agged_seq <- aggregate(data = treats_seq, pt_id ~ seq_1 + seq_2 + seq_3 + seq_4, FUN=length)     #flexible code to permit more seq - not just a series of case/if statements?
  colnames(input_agged_seq)[ncol(input_agged_seq)] <- "freq"

  # run alluvial plot
  
  output <- alluvial::alluvial(input_agged_seq[,1:(ncol(txvis.ref) - 1)], 
                               freq = input_agged_seq$freq,
                               blocks = TRUE, 
                               col = "#1f78b4",
                               border = "#1f78b4",
                               cw = tx_cw,
                               ...)
  
  NULL
}
