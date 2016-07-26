#' Generate an sunburst plot for treatment data.
#' 
#' Using a \code{txvis} object, plot the sequencing of treatments using a sunburst plot.
#'
#' @param txvis An object of class \code{txvis}.
#' @param nsequ What is the maximum number of sequences
#' @param seq.v.dat Should the data be plotted by sequence order or by date?
#' @param start If supplied the data will be truncated to all sequences after a start date.
#' @param end If supplied the data will be truncated to all sequences before the end date.
#' @param interval Length of time for intervening intervals for data plotted by date.
#' @param conflict If two treatments fall within the same date interval, which should be displayed?
#' @param tx_color A \code{colorRampPalette} to color the sequnces.  Note that the first element of the color vector always codes to "None".
#' @export

tx_sunburst <- function(txvis, nsequ=NULL, seq.v.dat="seq",
                        start = NULL, end = NULL, interval = "month", 
                        conflict = "majority", tx_color = NULL) {
  
  if (!"txvis" %in% class(txvis)) {
    stop('You must pass a txvis object.')
  }
  
  if (!requireNamespace("sunburstR", quietly = TRUE)) {
    message("This function requires the non-CRAN package `sunburstR` installed from GitHub.")
    user_inp <-  readline(prompt = "Do you want to install this package? (y/n)")
    if (user_inp == "y") {
      devtools::install_github("timelyportfolio/sunburstR")
      requireNamespace(sunburstR)
    } else {
      stop(paste0("You must install the package `sunburstR` for this function to work.\n",
                  " You can install the package directly from GitHub using:\n\n",
                  "> library(devtools)\n> devtools::install_github('timelyportfolio/sunburstR')"))
    }
  }
  

    if (seq.v.dat == "seq") {
      txvis.ref <- data.frame(t(apply(reform_seq(txvis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
    } else {
      txvis.ref <- data.frame(t(apply(reform_dates(txvis,nsequ,
                                                   start, end, interval, conflict), 1, 
                                      function(x) {x[is.na(x)] <- "None";(x)})),
                              stringsAsFactors = FALSE)
    }
 
  seq.cols <- paste0(rep("seq_", (ncol(txvis.ref) - 1)) , c(1:(ncol(txvis.ref) - 1)))
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  input_agged_seq <- stats::aggregate(data = txvis.ref, 
                               stats::as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     

  sequence_burst <- data.frame(sequence = apply(input_agged_seq, 1,
                                                function(x) { 
                                                  paste(x[1:ncol(input_agged_seq) - 1], collapse = '-')
                                                }),
                             count = input_agged_seq[,ncol(input_agged_seq)])
  
  if (is.null(tx_color)) { 
    sunburstR::sunburst(sequence_burst)
  } else { 
    sunburstR::sunburst(sequence_burst,colors = tx_color)
  }
}

