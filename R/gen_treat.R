#' Generate treatment data.

#' @importFrom random date

treatments<-c(1,2,3,4,5,6,7,8)
classes<-c("A","A","A","B","B","B","C","C")
probs<-c(.3,.2,.15,.05,.1,.08,.08,.04)
numpat<-1000
maxreg<-5


numtreat<-sample.int(maxreg,size=numpat,replace=TRUE,prob=NULL)

#pat_id<-paste0(randomStrings(n=numpat,len=3,digits=FALSE,loweralpha=F),sample.int(900,size=numpat,replace=TRUE,prob=NULL)+99)

#create empty data set
#treat<-data.frame(rep(pat_id,numtreat),NA,NA,NA,NA,NA)
#colnames(treat)<-c("pat_id","treatment","class","sequence","start","end")

#create first row of data
#treat[1,2]<-sample(treatments,1,prob=probs)
#treat[1,3]<-classes[treat[1,2]]
#treat[1,4]<-1
#treat[1,5]<-as.date(sample.int(700,1)+17000)
#treat[1,6]<-as.date(sample.int(330,1)+treat[1,5]+30)

#confirm data structure
#treat[,1]<-as.character(treat[,1])
#treat[,2]<-as.numeric(treat[,2])
#treat[,3]<-as.character(treat[,3])
#treat[,4]<-as.numeric(treat[,4])
#treat[,5]<-as.date(treat[,5])
#treat[,6]<-as.date(treat[,6])

#fill in rest of data
#for(i in 2:nrow(treat)){
#  if(treat[i,1]!=treat[i-1,1]){
#    treat[i,2]<-sample(treatments,1,prob=probs)
#    treat[i,3]<-classes[treat[i,2]]
#    treat[i,4]<-1
#    treat[i,5]<-as.date(sample.int(700,1)+17000)
#    treat[i,6]<-sample.int(330,1)+treat[i,5]+30
#    
#  } else {
#    rem<-as.numeric(treat[i-1,2])
#    if(rem==1 | rem==3 | rem==4){
#      tempprob<-probs[-rem]
#      tempprob[2]<-0.5
#      treat[i,2]<-sample(treatments[!treatments==rem],1,prob=tempprob)
#    } else {
#      treat[i,2]<-sample(treatments[!treatments==rem],1,prob=probs[-rem])
#    }
#    treat[i,3]<-classes[treat[i,2]]
#    treat[i,4]<-treat[i-1,4]+1
#    treat[i,5]<-sample.int(330,1)+treat[i-1,5]
#    treat[i,6]<-sample.int(330,1)+treat[i,5]+30
#  }
#}


#treat$treatment<-paste0("Treatment ",treat$treatment)
#treat$class<-paste0("Class ",treat$class)

#save(treat,file="data/treat.Rda")
