#' Generate a graphical transition matrix.
#' 
#' To show the rates at which one sequence passes to the next within a treatment
#' regime.  The method uses the \code{corrplot} package to 
#' 
#' @param txvis An object of class \code{txvis}.
#' @param sequences A vector of length two indicating the sequences for which the corrplot will be drawn.
#' @param nseq What is the maximum number of sequences
#' @param ... additional arguments to \code{corrplot}
#' 
#' @return A square matrix.
#' 
#' @examples
#' 
#' # Create the txvis object:
#' 
#' hlth_data <- create_txvis(patient   = treat$pat_id, 
#'                           treatment = treat$treatment,
#'                           start     = treat$start,
#'                           end       = treat$end,
#'                           date_format = "%d%B%Y")
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

tx_transmat <- function(txvis, sequences = c(1,2), nseq = NULL, ...) {

  if (!"txvis" %in% class(txvis)) {
    stop("txvis must be of class txvis.")
  }
  
  if (!length(sequences) == 2) {
    stop("tx_transmat only displays the transition between two sequences.")
  }
  
  treat_seq <- reform_seq(txvis, nseq)
  
  seq_cols <- colnames(treat_seq)[sequences + 1]
  seqs     <- paste0("seq_", sequences)
  seq_fun  <- paste0(seq_cols, collapse = " + ")
  
  # Coding decision: allow user-defined ordering of tx?  Currently alphabetical.
  tx_levels <- sort(unique(unlist(treat_seq[,-1])))

  input_agged <- stats::aggregate(data = treat_seq, 
                                  stats::as.formula(paste0("pt_id ~ ", seq_fun)) ,
                                  FUN = length)
  
  input_agged$pt_id <- input_agged$pt_id / sum(input_agged$pt_id)
  
  input_corr <- reshape2::dcast(input_agged, 
                                stats::as.formula(paste0(seqs[1], '~', seqs[2])), 
                                drop = FALSE, value.var = "pt_id")
  
  rownames(input_corr) <- input_corr[,1]

  cp_data <- as.matrix(input_corr[,-1])
  cp_data[is.na(cp_data)] <- 0
  
  corrplot::corrplot(cp_data, is.corr = FALSE, tl.col = 1, 
           method = "circle", outline = T, ...)

}