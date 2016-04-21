tx_sunburst <- function (txvis) {
  seq.cols <- paste( rep("seq",txvis[[3]]) , c(1:nseq),sep="_" )
  seq.fun  <- paste(seq.list,collapse = " + ")
  
  input_agged_seq <- aggregate(data = reform_seq(txvis)[seq.cols], paste ("pt_id ~ ",seq.fun, sep="") , FUN=length)     

  sequence_burst <- data.frame(sequence = apply(input_agged_seq, 1,
                                              function(x) paste(x[1:ncol(input_agged_seq)-1], collapse = '-')),
                             count = input_agged_seq[,ncol(input_agged_seq)])

  sunburst(sequence_burst)

}

