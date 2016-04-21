
tx_alluvial <- function (txvis) {
  seq.cols <- paste( rep("seq",txvis[[3]]) , c(1:nseq),sep="_" )
  seq.fun  <- paste(seq.list,collapse = " + ")
  
  input_agged_seq <- aggregate(data = reform_seq(txvis)[seq.cols], paste ("pt_id ~ ",seq.fun, sep="") , FUN=length)     

  #input_agged_seq <- aggregate(data = treats_seq, pt_id ~ seq_1 + seq_2 + seq_3 + seq_4, FUN=length)     #flexible code to permit more seq - not just a series of case/if statements?
  colnames(input_agged_seq)[ncol(input_agged_seq)]<-"freq"

  # run alluvial plot
  alluvial(input_agged_seq[,1:txvis[[3]]], freq = input_agged_seq$freq)
 }
 
 