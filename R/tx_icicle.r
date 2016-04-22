#' Generate an icicle plot for treatment data.
#' 
#' Using a \code{txVis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @import ggplot2
#' @param txVis An object of class \code{txVis}.
#' 
#' @export

tx_icicle <- function(txVis) {
  
  if (!class(txVis) %in% "txVis") {
    stop('You must pass a txVis object.')
  }
  
  # This needs to be completely re-coded :)
  
  txVis[txVis$seq == 2,c("seq_1","seq_2")] <- txVis[txVis$seq == 1,c("seq_1","seq_2")]
  txVis[txVis$seq == 3,c("seq_1","seq_2","seq_3")] <- txVis[txVis$seq == 1,c("seq_1","seq_2","seq_3")]
  txVis[txVis$seq == 4,c("seq_1","seq_2","seq_3","seq_4")] <- txVis[txVis$seq == 1,c("seq_1","seq_2","seq_3","seq_4")]
  txVis[txVis$seq == 1,c("seq_2","seq_3","seq_4")] <- NA
  txVis[txVis$seq == 2,c("seq_3","seq_4")] <- NA
  txVis[txVis$seq == 3,c("seq_4")] <- NA
  
  
  txVis.seq1<- txVis[txVis$seq==1,]
  txVis.seq1<- txVis.seq1[order(txVis.seq1$tx),]
  
  txVis.seq2<- txVis[txVis$seq==2,]
  txVis.seq2<- txVis.seq2[order(txVis.seq1$tx,txVis.seq2$tx),]
  
  txVis.seq3<- txVis[txVis$seq==3,]
  txVis.seq3<- txVis.seq3[order(txVis.seq1$tx,txVis.seq2$tx,txVis.seq3$tx),]
  
  txVis.seq4<- txVis[txVis$seq==4,]
  txVis.seq4<- txVis.seq4[order(txVis.seq1$tx,txVis.seq2$tx,txVis.seq3$tx,txVis.seq4$tx),]
  
  txVis2<-rbind(txVis.seq1,txVis.seq2,txVis.seq3,txVis.seq4)
  
  
  txVis2["x"]<-rep(1:nrow(txVis_seq),4)  #hard-coded 4
  colors <- colorRampPalette(c("dark blue", "white"))(6)
  
  ggplot(txVis2, aes(x= x,y = seq, fill = tx)) + 
    geom_tile() +
    theme_bw() +
    scale_fill_manual(values=colors)
  
  #need to invert this one. Also handle NAs (stopped)
  
  
  #now stacked (I forget what this is... some kind of stacked bar graph)
  txVis3      <- txVis[ order(txVis$seq,txVis$tx), ]
  txVis3["x"] <- rep(1:nrow(txVis_seq),4)
  txVis3$seq  <- -1*txVis3$seq
  
  colors <- colorRampPalette(c("dark blue", "white"))(6)
  ggplot(txVis3, aes(x= x,y = seq, fill = tx)) + 
    geom_tile() +
    theme_bw() +
    scale_fill_manual(values=colors)
  
}