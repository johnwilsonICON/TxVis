#' A helper function.
#' 
#' @param x A \code{txVis} object
#' @param y A treatment sequence.
#' @export

reform_nodes <- function(x, y){

  inp_table <- data.frame(ID = 1:nrow(x),
                          N1 = x[,y],
                          N2 = x[,y+1],
                          stringsAsFactors = FALSE)

  unique_set <- list(N1 = unique(inp_table$N1),
                     N2 = unique(inp_table$N2))

  nodes <- data.frame(ID     = c(paste0(y, '_', unique_set[[1]]),
                                 paste0(y + 1, '_', unique_set[[2]])),
                      x      = rep(y:(y + 1), sapply(unique_set, length)),
                      labels = unlist(unique_set))

  edges  <- aggregate(data = inp_table, ID ~ N1 + N2, FUN = length)

  colnames(edges)[3] <- "Value"
  edges$N1 <- paste0(y, '_', edges$N1)
  edges$N2 <- paste0(y+1, '_', edges$N2)

  list(nodes = nodes, edges = edges)

}
