#' Generate an sunburst plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using a sunburst plot.
#' 
#' @import sunburstR
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_sunburst <- function(txVis) {
  seq.cols <- paste0( rep("seq", txVis[[3]]) , c(1:nseq))
  seq.fun  <- paste0(seq.list, collapse = " + ")
  
  input_agged_seq <- aggregate(data = reform_seq(txVis)[seq.cols], 
                               paste0("pt_id ~ ", seq.fun) ,
                               FUN = length)     

  sequence_burst <- data.frame(sequence = apply(input_agged_seq, 1,
                                                function(x) paste(x[1:ncol(input_agged_seq)-1], collapse = '-')),
                             count = input_agged_seq[,ncol(input_agged_seq)])

  sunburst(sequence_burst)

}