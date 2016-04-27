#' Generate an alluvial plot for treatment data using D3.js.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using an alluvial plot.
#' 
#' @importFrom networkD3 sankeyNetwork
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_d3alluvial <- function(txVis,nsequ=NULL) {
  #library(networkD3)

  nseq<- ifelse (!is.null(nsequ), nsequ, 4)  #defaults to 4 if not entered by user
  seq.cols <- paste0( rep("seq_", nseq) , c(1:nseq) )
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  networked_list <- lapply(1:3, reform_nodes, x = reform_seq(txVis,nseq))
  
  edges <- do.call(rbind.data.frame, lapply(networked_list, function(x)x$edges))
  nodes <- do.call(rbind.data.frame, lapply(networked_list, function(x)x$nodes))
  
  nodes <- nodes[!duplicated(nodes[,1]),]
  
  ##########################
  #D3
  ##########################
  
  colnames(edges) <- c('Source', 'Target', 'Value')
  
  nodes_d3 <- data.frame(name   = substr(nodes$ID, 3, 1000))
  
  links_d3 <- data.frame(source = match(edges$Source, nodes$ID) - 1,
                         target = match(edges$Target, nodes$ID) - 1,
                         value  = edges$Value)
  
  networkD3::sankeyNetwork(Links = links_d3, 
                           Nodes = nodes_d3, 
                           Source = "source",
                           Target = "target", 
                           Value = "value", 
                           NodeID = "name")
  
}
