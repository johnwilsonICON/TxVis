#' Generate a transition matrix.
#' 
#' I'm not quite sure why we have this.
#' 
#' @import corrplot
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_transmat <- function (txVis,sequA=NULL,sequB=NULL) {

  seqA<- ifelse (!is.null(sequA), sequA, 1)  #defaults to 1 if not entered by user
  seqB<- ifelse (!is.null(sequB), sequB, 2)  #defaults to 2 if not entered by user
 
  seq.cols <- c( paste("seq", seqA, sep="_") ,paste("seq", seqB, sep="_"))
  seq.fun  <- paste0(seq.cols, collapse = " + ")
  treats_seq<-reform_seq(txVis,nseq)
  tx_levels <- unique(sort(unique(treats_seq[,seq.cols[1]],treats_seq[,seq.cols[2]])))             #Coding decision: allow user-defined ordering of tx?      Currently alphabetical.

  input_agged_12 <- aggregate(data = treats_seq, 
                               as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)     
  colnames(input_agged_12)<-c("A","B","pt_id")
  
  cp_data_12<-data.frame( tx1=rep(NA,length(tx_levels)), tx2=rep(NA,length(tx_levels)))
  
  for (i in 1:length(tx_levels)) {     #i is the sequence 1 treatment, captured in the rows
    for (j in 1:length(tx_levels)) {    #j is the sequence 3 treatment, captured in the columns
      ntrans<-input_agged_12[input_agged_12$A == tx_levels[i] & input_agged_12$B == tx_levels[j],"pt_id"] 
      if(length(ntrans)==0) {cp_data_12[i,j]<-0}
      if(length(ntrans)==1) {cp_data_12[i,j]<-ntrans/sum(input_agged_12$pt_id)}
    } #next j
  } #next i
  #end for loop
  
  
  cp_data_12
  cp_data_12<-as.matrix(cp_data_12)
  colnames(cp_data_12)<-tx_levels

  sum(cp_data_12)
  
  corrplot(cp_data_12,is.corr = FALSE,tl.col=1,method="circle",outline=T)

}

