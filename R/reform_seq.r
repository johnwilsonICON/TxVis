#' Another helper function
#' @export

reform_seq <- function (txVis,nsequ) {

  #apply sequencing
  nseq<- ifelse (!is.null(nsequ), nsequ, 4)  #defaults to 4 if not entered by user
  treats<- within(txVis[[1]], {seq <- as.numeric(ave(pt_id, list(pt_id), FUN=seq_along))})
  treats<-treats[treats$seq <= nseq,]  
  treats$tx<-as.character(treats$tx)
  treats<-reshape2::dcast(treats, pt_id ~ seq, max, na.rm=TRUE, value.var="tx")
  
  seq.cols<-paste( rep("seq",nseq),c(1:nseq),sep="_")
  colnames(treats)<-c("pt_id",seq.cols)
  return(treats)

  } 



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
