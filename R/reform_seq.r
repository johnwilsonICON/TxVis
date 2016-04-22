#' Another helper function
#' @export

reform_seq <- function (txvis) {

  #apply sequencing
  nseq<- ifelse (!is.na(txvis[[3]]), txvis[[3]], 4)  #defaults to 4 if not entered by user
  treats<- within(txvis[[1]], {seq <- as.numeric(ave(pt_id, list(pt_id), FUN=seq_along))})
  treats<-treats[treats$seq <= nseq,]  

  #now cast the dataset into num columns that equals the max seq #
  #treats1<-cast(treats,pt_id~seq,max)
  #couldn't do the cast thing - bad bad loop, currently doesn't handle situations where seq DNE
  for (i in 1:nseq)  {    
      treats[paste("seq",i,sep="_")]<-NA
      treats[treats$seq==i,paste("seq",i,sep="_")]<-as.character(treats[treats$seq==i,"tx"])
      treats[,paste("seq",i,sep="_")]<-tapply(as.factor(treats$pt_id),as.character(treats[,paste("seq",i,sep="_")]),FUN=max) 
  } #end for

  treats<-treats[treats$seq == 1,]

  return(treats)
} 

#will sort out dates later


######
#date-related cuts

#then apply time cuts, currently hard-coding 30 day brackets
#CODING DECISION: align all starting dates, so that time is from index date, rather than a set calendar date.
#Could be user input
#treats<-merge(treats,aggregate(start_date~pt_id,data=treats,function(x) min(x)),by="pt_id", all.x=T)
#colnames(treats)[c(3,6)]<-c("start_date","index_date")
#treats["date_brk"]<- cut(as.numeric(treats$start_date - treats$index_date)+.001,breaks=seq(from=0,to=30*10,by=30))
#treats["dur"]<-as.numeric(treats$end_date - treats$start_date)
#treats["days_from_index"]<-as.numeric(treats$start_date - treats$index_date)
