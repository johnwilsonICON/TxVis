#' Generate an alluvial plot for treatment data using D3.js.
#' 
#' Using a \code{txvis} object, plot the sequencing of treatments using an alluvial plot.
#' 
#' @importFrom networkD3 sankeyNetwork
#' @param txvis An object of class \code{txvis}.
#' 
#' @export

tx_d3alluvial <- function(txvis,nsequ=NULL,seq.v.dat="seq",start = NULL, end = NULL, interval = "month", conflict = "majority") {
  #library(networkD3)

  if(seq.v.dat=="seq") {
    txvis.ref<-data.frame(t(apply(reform_seq(txvis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  } else {
    txvis.ref<-data.frame(t(apply(reform_dates(txvis,nsequ,start, end, interval, conflict), 1, function(x) {x[is.na(x)] <- "None";(x)})),stringsAsFactors = F)
  }
  
  seq.cols <- paste0( rep("seq_", (ncol(txvis.ref)-1)) , c(1:(ncol(txvis.ref)-1)) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  # reform_nodes turns the sequenceing into a network format of edges & nodes, by sequence.
  #  Since the first column of the txvis object is the patient ID we drop it.
  networked_list <- lapply(1:3, function(x)reform_nodes(x = reform_seq(txvis,nsequ), y = x + 1))
  
  edges <- do.call(rbind.data.frame, lapply(networked_list, function(x) x$edges))
  nodes <- do.call(rbind.data.frame, lapply(networked_list, function(x) x$nodes))
  
  nodes <- nodes[!duplicated(nodes[,1]),]
  
  ##########################
  #D3
  ##########################
  
  colnames(edges) <- c('Source', 'Target', 'Value')
  
  nodes_d3 <- data.frame(name   = substr(nodes$ID, 3, 1000))
  
  links_d3 <- data.frame(source = match(edges$Source, nodes$ID) - 1,
                         target = match(edges$Target, nodes$ID) - 1,
                         value  = edges$Value)
  
  sankeyNetwork(Links = links_d3, 
                           Nodes = nodes_d3, 
                           Source = "source",
                           Target = "target", 
                           Value = "value", 
                           NodeID = "name")
  
}
