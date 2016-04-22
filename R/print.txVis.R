#' @export

print.txVis <- function(x, ...){
  class(x[[1]]) <- 'data.frame'
  
  x[[1]]
  
}
