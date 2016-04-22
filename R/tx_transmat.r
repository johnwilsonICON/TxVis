#' Generate a transition matrix.
#' 
#' I'm not quite sure why we have this.
#' 
#' @import sunburstR
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_transmat <- function (txVis) {
  seq.cols <- paste( rep("seq",txvis[[3]]) , c(1:nseq), sep="_" )
  treats_seq <- reform_seq(txvis[[1]])
  tx_levels <- sort(unique(seq.cols))             #Coding decision: allow user-defined ordering of tx?      Currently alphabetical.
  
  for (i in seq.cols) {
    treats_seq[seq.cols[i]]<-factor(treats_seq[seq.cols[i]],levels=tx_levels)
  }

  #1 to 2
  cp_data_12<-data.frame( tx1=rep(NA,length(levels(treats_seq[seq.cols[1]]))), tx2=rep(NA,length(levels(treats_seq[seq.cols[1]]))))
  
  for (i in 1:length(levels(treats_seq[seq.cols[1]]))) {     #i is the sequence 1 treatment, captured in the rows
    for (j in 1:length(levels(treats_seq[seq.cols[1]]))) {    #j is the sequence 3 treatment, captured in the columns
      cp_data_12[i,j]<-nrow(treats_seq[as.numeric(treats_seq[seq.cols[1]]) == i & as.numeric(treats_seq[seq.cols[2]]) == j,])/nrow(treats_seq[as.numeric(treats_seq[seq.cols[1]]) == i ,])
    } #next j
  } #next i
  #end for loop

  colnames(cp_data_12)<-levels(treats_seq[seq.cols[1]])
  rownames(cp_data_12)<-levels(treats_seq[seq.cols[1]])
  cp_data_12
  cp_data_12<-as.matrix(cp_data_12)

  corrplot(cp_data_12)

  #2 to 3
  #...

#3 to 4
#...

}

