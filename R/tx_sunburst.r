#' Generate an sunburst plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using a sunburst plot.
#'
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_sunburst <- function(txVis,nsequ=NULL) {
  
  if (!require("sunburstR",character.only = TRUE)) {
    message("This function requires the non-CRAN package `sunburstR` installed from GitHub.")
    user_inp <-  readline(prompt = "Do you want to install this package? (y/n)")
    if (user_inp == "y") {
      devtools::install_github("timelyportfolio/sunburstR")
      require(sunburstR)
    } else {
      stop("You must install `sunburstR` for this function to work.")
    }
  }
  
  nseq<- ifelse (!is.null(nsequ), nsequ, 4)  #defaults to 4 if not entered by user
  seq.cols <- paste0( rep("seq_", nseq) , c(1:nseq) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  input_agged_seq <- aggregate(data = reform_seq(txVis,nseq), 
                               as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     

  sequence_burst <- data.frame(sequence = apply(input_agged_seq, 1,
                                                function(x) paste(x[1:ncol(input_agged_seq)-1], collapse = '-')),
                             count = input_agged_seq[,ncol(input_agged_seq)])
  
  sunburst(sequence_burst)

}

