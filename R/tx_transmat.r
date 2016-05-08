#' Generate a graphical transition matrix.
#' 
#' To show the rates at which one sequence passes to the next within a treatment
#' regime.  The method uses the \code{corrplot} package to 
#' 
#' @importFrom corrplot corrplot
#' @param txVis An object of class \code{txVis}.
#' @param sequences A vector of length two indicating the sequences for which the corrplot will be drawn.
#' @param nseq What is the maximum number of sequences
#' @param ... additional arguments to \code{corrplot}
#' 
#' @return A square matrix.
#' 
#' @examples
#' 
#' # Create the txVis object:
#' 
#' hlth_data <- create_txVis(patient   = treat$patient, 
#'                           treatment = treat$treatment,
#'                           start     = treat$start,
#'                           end       = treat$end,
#'                           date_format = "%B %d, %Y")
#' 
#' # A simple correlation plot:
#' tx_transmat(hlth_data, sequences = c(1, 2))
#' 
#' # Some cutomization:
#' tx_transmat(hlth_data, 
#'             sequences = c(1, 2), 
#'             mar = c(2, 2, 2, 2),
#'             order = "hclust")
#' 
#' @export

tx_transmat <- function(txVis, sequences = c(1,2), nseq = NULL, ...) {

  if (!"txVis" %in% class(txVis)) {
    stop("txVis must be of class txVis.")
  }
  
  if (!length(sequences) == 2) {
    stop("tx_transmat only displays the transition between two sequences.")
  }
  
  treat_seq <- reform_seq(txVis, nseq)
  
  seq_cols <- colnames(treat_seq)[sequences + 1]
  seq_fun  <- paste0(seq_cols, collapse = " + ")
  
  # Coding decision: allow user-defined ordering of tx?  Currently alphabetical.
  tx_levels <- sort(unique(unlist(treat_seq[,-1])))

  input_agged <- aggregate(data = treat_seq, 
                           as.formula(paste0("pt_id ~ ", seq_fun)) ,
                           FUN = length)
  
  input_agged$pt_id <- input_agged$pt_id / sum(input_agged$pt_id)
  
  input_corr <- dcast(input_agged, seq_1 ~ seq_2, 
                      drop = FALSE, value.var = "pt_id")
  
  rownames(input_corr) <- input_corr[,1]

  cp_data <- as.matrix(input_corr[,-1])
  cp_data[is.na(cp_data)] <- 0
  
  corrplot(cp_data, is.corr = FALSE, tl.col = 1, 
           method = "circle", outline = T, ...)

}

