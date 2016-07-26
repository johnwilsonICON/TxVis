#' Generate an alluvial plot for treatment data using D3.js.
#' 
#' Using a \code{txvis} object, plot the sequencing of treatments using an alluvial plot.
#' 
#' @param txvis An object of class \code{txvis}.
#' @param nsequ The number of sequences to be shown (default is 4)
#' @param seq.v.dat Should the data be plotted by sequence order or by date?
#' @param start If supplied the data will be truncated to all sequences after a start date.
#' @param end If supplied the data will be truncated to all sequences before the end date.
#' @param interval Length of time for intervening intervals for data plotted by date.
#' @param conflict If two treatments fall within the same date interval, which should be displayed?
#' @param link_coloring Used to indicate the source of the link (path) coloring. \code{"from"} (default) colors based on the treatment source,  or \code{"to"} colors based on destination, or \code{"none"}.
#'  
#' @return NULL
#' @examples
#'  hlth_data <- create_txvis(patient        = treat$pat_id, 
#'                            treatment      = treat$treatment,
#'                            start          = treat$start,
#'                            end            = treat$end,
#'                            date_format    = "%d%b%Y",
#'                            ev_patient     = events$pat_id,
#'                            events         = events$event,
#'                            event_date     = events$start,
#'                            event_end_date = events$end)
#'                            
#'  # Basic plotting:                          
#'  tx_d3alluvial(hlth_data)

#' 
#' @export

tx_d3alluvial <- function(txvis,
                          nsequ = NULL,
                          seq.v.dat = "seq",
                          start = NULL,
                          end = NULL,
                          interval = "month",
                          conflict = "majority",
                          link_coloring = "from") {
  
  if (!"txvis" %in% class(txvis)) {
    stop('You must pass a txvis object.')
  }
  
  if (!link_coloring %in% c("from", "to", "none")) {
    stop("The parameter 'link_coloring' must be one of 'from', 'to' or 'none'.")
  }
  
  if (seq.v.dat == "seq") {
    # If the plot is to be sequence order & not by date:
    txvis.ref <- data.frame(t(apply(reform_seq(txvis,nsequ), 1, 
                                    function(x) {x[is.na(x)] <- "None";(x)})),
                            stringsAsFactors = FALSE)
  } else {
    # Otherwise, bin into date windows:
    txvis.ref <- data.frame(t(apply(reform_dates(txvis, nsequ, start, 
                                                 end, interval, conflict), 1, 
                                    function(x) {x[is.na(x)] <- "None"; (x)})),
                            stringsAsFactors = FALSE)
  }
  
  seq.cols <- paste0(rep("seq_", (ncol(txvis.ref) - 1)),
                     c(1:(ncol(txvis.ref) - 1)))
  
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  
  # reform_nodes turns the sequenceing into a network format of edges & nodes, by sequence.
  #  Since the first column of the txvis object is the patient ID we drop it.
  networked_list <- lapply(1:3, function(x){
    reform_nodes(x = reform_seq(txvis,nsequ), y = x + 1)})
  
  edges <- do.call(rbind.data.frame, lapply(networked_list, function(x) x$edges))
  nodes <- do.call(rbind.data.frame, lapply(networked_list, function(x) x$nodes))
  
  nodes <- nodes[!duplicated(nodes[,1]),]
  
  ##########################
  #D3
  ##########################
  
  colnames(edges) <- c('Source', 'Target', 'Value')
  
  nodes_d3 <- data.frame(name   = substr(nodes$ID, 3, 1000),
                         stringsAsFactors = FALSE)
  
  nodes_d3[which(nodes_d3[,1] == "NA"),1] <- ""
  
  nodes_d3$name <- nodes_d3$name
  nodes_d3$name_group <- gsub(" ", "_", nodes_d3$name)
  
  links_d3 <- data.frame(source = match(edges$Source, nodes$ID) - 1,
                         target = match(edges$Target, nodes$ID) - 1,
                         value  = edges$Value)
  
  if (link_coloring == "from") {
    links_d3$link_group <- as.character(gsub(" ", "_", nodes_d3[links_d3$source + 1,1]))
  }
  if (link_coloring == "to") {
    links_d3$link_group <- as.character(gsub(" ", "_", nodes_d3[links_d3$target + 1,1]))
  }
  if (link_coloring == "none") {
    links_d3$link_group <- NA
  }
  
  networkD3::sankeyNetwork(Links = links_d3, 
                           Nodes = nodes_d3, 
                           Source = "source",
                           Target = "target", 
                           Value = "value", 
                           NodeID = "name",
                           NodeGroup = "name_group",
                           LinkGroup = "link_group")

}
