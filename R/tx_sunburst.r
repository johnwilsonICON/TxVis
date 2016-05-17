#' Generate an sunburst plot for treatment data.
#' 
#' Using a \code{txvis} object, plot the sequencing of treatments using a sunburst plot.
#'
#' @param txvis An object of class \code{txvis}.
#' @export

tx_sunburst <- function(txvis,nsequ=NULL,seq.v.dat="seq",start = NULL, end = NULL, interval = "month", conflict = "majority",tx_colour=NULL) {
  
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
      txvis.ref<-data.frame(t(apply(reform_seq(txvis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
    } else {
      txvis.ref<-data.frame(t(apply(reform_dates(txvis,nsequ,start, end, interval, conflict), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
    }
 
  seq.cols <- paste0( rep("seq_", (ncol(txvis.ref)-1)) , c(1:(ncol(txvis.ref)-1)) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  input_agged_seq <- aggregate(data = txvis.ref, 
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

