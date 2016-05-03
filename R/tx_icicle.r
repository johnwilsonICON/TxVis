#' Generate an icicle plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @import ggplot2
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_icicle <- function(txVis, nsequ=NULL) {
  
  if (!class(txVis) %in% "txVis") {
    stop('You must pass a txVis object.')
  }
<<<<<<< HEAD
 
=======
  
>>>>>>> aaf9af2d3fe7cc0bd59c68e5675e686633a1c410
  nseq<- ifelse (!is.null(nsequ), nsequ, 4)  #defaults to 4 if not entered by user
  seq.cols <- paste0( rep("seq_", nseq) , c(1:nseq) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  treats<-data.frame(t(apply(reform_seq(txVis,nseq), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  input_agged_seq <- aggregate(data = treats, 
                               as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     
  input_agged_seq<-input_agged_seq[do.call(order,input_agged_seq[,seq.cols]),]
  input_agged_seq <- input_agged_seq[rep(row.names(input_agged_seq), input_agged_seq$pt_id),]  #creates a row for each data point
  input_agged_seq["x"]<-1:nrow(input_agged_seq)
  
  plot_input<-melt(input_agged_seq,id=c("pt_id","x"))
  #plot_input["y"]<-rep(1:nseq,each=nrow(input_agged_seq))
  plot_input$variable<-as.character(plot_input$variable)  
  colors <- colorRampPalette(c("dark blue", "white"))(length(unique(plot_input$value)))
  
  ggplot(plot_input, aes(x= x,y = variable, fill = value)) + 
    geom_tile() +
    theme_bw() +
    scale_fill_manual(values=colors)
  
  #need to order treatments so that "none" is white
  #need to suppress axis labels
}
