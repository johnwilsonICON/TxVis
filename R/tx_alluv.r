#' Generate an alluvial plot for treatment data.
#' 
#' Using a txVis object, plot the sequencing of treatments using an alluvial plot.
#' 
#' @param txVis An object of class \code{txVis}.
#' @param nsequ The maximum number of sequences to plot.
#' 
#' @export


tx_alluvial <- function(txVis,nsequ=NULL,seq.v.dat="seq",start = NULL, end = NULL, interval = "month", conflict = "majority",tx_colour=NULL,tx_cw=0.05) {
  
  if (!require("alluvial",character.only = TRUE)) {
    message("This function requires the non-CRAN package `alluvial` installed from GitHub.")
    user_inp <-  readline(prompt = "Do you want to install this package? (y/n)")
    if (user_inp == "y") {
      devtools::install_github("mbojan/alluvial")
      library(alluvial)
    } else {
      stop("You must install `alluvial` for this function to work.")
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
  
  
  #input_agged_seq <- aggregate(data = treats_seq, pt_id ~ seq_1 + seq_2 + seq_3 + seq_4, FUN=length)     #flexible code to permit more seq - not just a series of case/if statements?
  colnames(input_agged_seq)[ncol(input_agged_seq)] <- "freq"

  # run alluvial plot
  if (is.null(tx_colour)) { 
    alluvial::alluvial(input_agged_seq[,1:(ncol(txVis.ref)-1)], freq = input_agged_seq$freq,blocks=T,cw=0.05,col="#1f78b4",border="#1f78b4",cw=tx_cw)
  } else { 
    alluvial::alluvial(input_agged_seq[,1:(ncol(txVis.ref)-1)], freq = input_agged_seq$freq,col=tx_colour,border=tx_colour,cw=tx_cw)
  }
  
}
