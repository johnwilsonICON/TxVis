#' Generate an icicle plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @import ggplot2
#' @import reshape2
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_icicle <- function(txVis, nsequ=NULL,seq.v.dat="seq",start = NULL, end = NULL, interval = "month", conflict = "majority",tx_colour=NULL) {
  
  if (!class(txVis) %in% "txVis") {
    stop('You must pass a txVis object.')
  }

  if(seq.v.dat=="seq") {
    txVis.ref<-data.frame(t(apply(reform_seq(txVis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  } else {
    txVis.ref<-data.frame(t(apply(reform_dates(txVis,nsequ,start, end, interval, conflict), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  }
  
#  treats<-data.frame(t(apply(reform_seq(txVis,nseq), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)

  seq.cols <- paste0( rep("seq_", (ncol(txVis.ref)-1)) , c(1:(ncol(txVis.ref)-1)) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")

  input_agged_seq <- aggregate(data = txVis.ref, 
                               as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     
  input_agged_seq<-input_agged_seq[do.call(order,input_agged_seq[,seq.cols]),]
  input_agged_seq <- input_agged_seq[rep(row.names(input_agged_seq), input_agged_seq$pt_id),]  #creates a row for each data point
  input_agged_seq["x"]<-1:nrow(input_agged_seq)
  
  plot_input<-reshape2::melt(input_agged_seq,id=c("pt_id","x"))
  #plot_input["y"]<-rep(1:nseq,each=nrow(input_agged_seq))
  plot_input$variable<-as.character(plot_input$variable)
  plot_input[nchar(plot_input$variable) == 5,"variable"] <-paste0(substr(plot_input[nchar(plot_input$variable) == 5,"variable"],1,4),"0",substr(plot_input[nchar(plot_input$variable) == 5,"variable"],5,5))
  plot_input$variable<-as.numeric(substr(plot_input$variable,5,6))
  if (is.null(tx_colour)) { 
    colors <- colorRampPalette(c("dark blue", "white"))(length(unique(plot_input$value)))
  } else { 
    colors <- tx_colour
  }

  
  ggplot(plot_input, aes(x= 100*x/max(x),y = variable, fill = value)) + 
    geom_tile() +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_rect(fill="white"), panel.border=element_rect(colour="black",fill=NA,size=2)) + 
    scale_fill_manual(values=colors) +
    labs(fill="Treatment",x="Percent",y="Sequence number")
  
    
  #need to order treatments so that "none" is white
  #need to suppress axis labels
}
