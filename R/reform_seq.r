#' A helper function to turn a long table into a wide table.
#' 
#' @param txvis A \code{txvis} object.
#' @param nsequ The maximum number of sequences to return.
#' 
#' @export

reform_seq <- function(txvis, nsequ=NULL) {

  #apply sequencing
  nseq <- ifelse(!is.null(nsequ), nsequ, 4)  #defaults to 4 if not entered by user
  
  treats <- within(txvis[[1]], {
    seq <- as.numeric(stats::ave(pt_id, list(pt_id), FUN = seq_along))
    })
  
  treats    <- treats[treats$seq <= nseq,]  
  treats$tx <- as.character(treats$tx)
  
  treats <- suppressWarnings(reshape2::dcast(treats, pt_id ~ seq, 
                                             max, na.rm = TRUE, 
                                             value.var = "tx"))
  
  seq.cols <- paste(rep("seq",nseq), c(1:nseq), sep = "_")
  colnames(treats) <- c("pt_id",seq.cols)
  return(treats)

}