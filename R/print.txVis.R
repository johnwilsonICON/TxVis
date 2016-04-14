#' @export

print.txVis <- function(x, ...){
  class(x) <- 'data.frame'
  
  x
  
}
