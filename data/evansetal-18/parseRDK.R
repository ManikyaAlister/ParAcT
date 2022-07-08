rm(list=ls())
library(here)
parseFeedbackType="Optim"
parseBlockType="Trial"

files=dir(here("data/evansetal-18/raw"))

data=NULL
subjgroups1=NULL
subjgroups2=NULL
subjgroups3 = NULL

for (n in 1:(length(files))) {
  snum=as.numeric(substr(files[n],1,5))
  if (as.character(substr(files[n],7,7)) == "O") {
    feedbackType = as.character(substr(files[n],7,11))
    if (as.character(substr(files[n],13,14)) == "Tr") {
      blockType = as.character(substr(files[n],13,17))
    } else {
      blockType = as.character(substr(files[n],13,16))
    }
  } else {
    feedbackType = as.character(substr(files[n],7,10))
    if (as.character(substr(files[n],12,13)) == "Tr") {
      blockType = as.character(substr(files[n],12,16))
    } else {
      blockType = as.character(substr(files[n],12,15))
    }
  }
  tmp=read.table(file=here(paste("data/evansetal-18/raw",files[n],sep="/")),header=TRUE)
  subjgroups1=c(subjgroups1,feedbackType)
  subjgroups2=c(subjgroups2,blockType)
  data=rbind(data,cbind(subject=snum,feedbackType=feedbackType,blockType=blockType,tmp))
}


nSubs = unique(data$subject)
S=length(nSubs)

data$RT=data$RT/1000

data=data[data$correct!=2,]
data=data[data$RT<10,]


participantsMeanAccuracy=tapply(data$correct,data$subject,mean)
len=length(participantsMeanAccuracy)



badSubs=NULL

for (i in 1:len) {
  partID = nSubs[i]
  if (participantsMeanAccuracy[paste(partID)] < 0.7) {
    data=data[data$subject!=partID,]
    badSubs=c(badSubs,i)
  }
}

nSubs=nSubs[-badSubs]
subjgroups1=subjgroups1[-badSubs]
subjgroups2=subjgroups2[-badSubs]
subjgroups3=subjgroups3[-badSubs]

S=length(nSubs)

use.subs=subjgroups1==parseFeedbackType & subjgroups2==parseBlockType
use.subs=grep("TRUE",use.subs,perl=TRUE)

parsedData=list()

for (i in 1:(length(use.subs))) {
  parsedData[[i]]=list(Cond=NULL,Time=NULL,Resp=NULL,Stim=NULL)
  isSub=data$subject==nSubs[use.subs[i]]
  #parsedData[[i]]$Cond=data$blkNum[isSub]
  parsedData[[i]]$Cond=1
  parsedData[[i]]$Time=data$RT[isSub]
  parsedData[[i]]$Resp=data$correct[isSub]==1
  parsedData[[i]]$Stim=data$winningDirection[isSub]
  parsedData[[i]]$Trial = 1:length(parsedData[[i]]$Time)
  parsedData[[i]]$TrialInBlock = data$trlNum[isSub]
  parsedData[[i]]$Block = data$blkNum[isSub]
}

#data=parsedData
S=length(parsedData)
conds=unique(parsedData[[1]]$Cond)
n.cond=length(conds)
stims=unique(parsedData[[1]]$Stim)
n.stim=length(stims)


save(parsedData,S,conds,n.cond,stims,n.stim,
     file=here(paste("data/evansetal-18/clean/parsedData-RDK-",parseFeedbackType,"-",parseBlockType,".Rdata",sep="")))

allData = data

for (i in 1:(length(use.subs))) {
  #data= as.data.frame(matrix(nrow = 0,ncol = 1))
    
  data = data.frame(Cond=NULL,Time=NULL,Resp=NULL,Stim=NULL,Trial = NULL, TrialInBlock = NULL, Block = NULL)
  #data = data.frame()
  isSub=allData$subject==nSubs[use.subs[i]]
  #parsedData[[i]]$Cond=data$blkNum[isSub]
  Time=allData$RT[isSub]
  Resp=allData$correct[isSub]+1
  Stim=allData$winningDirection[isSub]
  Trial = 1:length(parsedData[[i]]$Time)
  TrialInBlock = allData$trlNum[isSub]
  Block = allData$blkNum[isSub]
  Cond= rep(1,length(parsedData[[i]]$Time))
  PID = nSubs[use.subs[i]]
  
  data = as.data.frame(cbind(PID,Time,Resp,Stim,Trial,TrialInBlock,Block,Cond))
  
  # a bunch of columns get converted to numeric when switching to a data frame
  data$Trial = as.integer(data$Trial)
  data$Time = as.numeric(data$Time)
  data$Resp = as.numeric(data$Resp)
  data$Cond = as.numeric(data$Cond)
  data$Block = as.numeric(data$Block)
  
  
  save(data,file = here(paste("data/evansetal-18/clean/P",i,".Rdata",sep = "")))
}



