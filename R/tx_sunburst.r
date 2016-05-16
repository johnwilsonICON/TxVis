#' Generate an sunburst plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using a sunburst plot.
#'
#' @param txVis An object of class \code{txVis}.
#' @export

tx_sunburst <- function(txVis,nsequ=NULL,seq.v.dat="seq",start = NULL, end = NULL, interval = "month", conflict = "majority",tx_colour=NULL) {
  
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
  

    if(seq.v.dat=="seq") {
      txVis.ref<-data.frame(t(apply(reform_seq(txVis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
    } else {
      txVis.ref<-data.frame(t(apply(reform_dates(txVis,nsequ,start, end, interval, conflict), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
    }
 
  seq.cols <- paste0( rep("seq_", (ncol(txVis.ref)-1)) , c(1:(ncol(txVis.ref)-1)) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  input_agged_seq <- aggregate(data = txVis.ref, 
                               as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     

  sequence_burst <- data.frame(sequence = apply(input_agged_seq, 1,
                                                function(x) { 
                                                  paste(x[1:ncol(input_agged_seq) - 1], collapse = '-')
                                                }),
                             count = input_agged_seq[,ncol(input_agged_seq)])
  
  if (is.null(tx_colour)) { 
    sunburstR::sunburst(sequence_burst)
  } else { 
    sunburstR::sunburst(sequence_burst,colors=tx_colour)
  }
}

