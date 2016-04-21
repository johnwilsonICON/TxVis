#######################################
#Icicle
#######################################
treats[treats$seq == 2,c("seq_1","seq_2")]<-treats[treats$seq == 1,c("seq_1","seq_2")]
treats[treats$seq == 3,c("seq_1","seq_2","seq_3")]<-treats[treats$seq == 1,c("seq_1","seq_2","seq_3")]
treats[treats$seq == 4,c("seq_1","seq_2","seq_3","seq_4")]<-treats[treats$seq == 1,c("seq_1","seq_2","seq_3","seq_4")]
treats[treats$seq == 1,c("seq_2","seq_3","seq_4")]<-NA
treats[treats$seq == 2,c("seq_3","seq_4")]<-NA
treats[treats$seq == 3,c("seq_4")]<-NA


treats.seq1<- treats[treats$seq==1,]
treats.seq1<- treats.seq1[order(treats.seq1$tx),]

treats.seq2<- treats[treats$seq==2,]
treats.seq2<- treats.seq2[order(treats.seq1$tx,treats.seq2$tx),]

treats.seq3<- treats[treats$seq==3,]
treats.seq3<- treats.seq3[order(treats.seq1$tx,treats.seq2$tx,treats.seq3$tx),]

treats.seq4<- treats[treats$seq==4,]
treats.seq4<- treats.seq4[order(treats.seq1$tx,treats.seq2$tx,treats.seq3$tx,treats.seq4$tx),]

treats2<-rbind(treats.seq1,treats.seq2,treats.seq3,treats.seq4)


treats2["x"]<-rep(1:nrow(treats_seq),4)  #hard-coded 4
colors <- colorRampPalette(c("dark blue", "white"))(6)
ggplot(treats2, aes(x= x,y = seq, fill = tx)) + geom_tile() +theme_bw() +scale_fill_manual(values=colors)
#need to invert this one. Also handle NAs (stopped)


#now stacked (I forget what this is... some kind of stacked bar graph)
treats3<-treats[ order(treats$seq,treats$tx), ]
treats3["x"]<-rep(1:nrow(treats_seq),4)
treats3$seq<- -1*treats3$seq

colors <- colorRampPalette(c("dark blue", "white"))(6)
ggplot(treats3, aes(x= x,y = seq, fill = tx)) + geom_tile() +theme_bw() +scale_fill_manual(values=colors)
