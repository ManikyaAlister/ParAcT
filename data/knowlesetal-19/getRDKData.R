## Some files have the same ID number which is why some "participants" have 320 trials. 
rm(list=ls())

files=dir("data/knowlesetal-19/raw/",pattern = ".txt")

data=NULL
for (n in files) {
  if (substr(n,1,4)=="PRAC") {
    
  } else {
    if (substr(n,1,1)=="c") {
      snum=as.numeric(substr(n,2,5))
    }
    else if (substr(n,1,1)=="C") {
      snum=as.numeric(substr(n,2,5))
    }
    else if (substr(n,1,1)=="K") {
      snum=as.numeric(substr(n,3,5))
    }
    else if (substr(n,1,1)=="E") {
      snum=99999
    }
    else if (substr(n,1,1)=="_") {
      snum=99998
    }
    else {
      snum=as.numeric(substr(n,1,5))
    }
    if (is.na(snum)) browser()
    tmp=read.table(file=paste("data/knowlesetal-19/raw/",n,sep=""),header=TRUE)
    data=rbind(data,cbind(subject=snum,tmp))
  }
}


## Options Menu for model fits

use.exclusions = TRUE     # Are we excluding?
minAcc = .6     # If we're excluding, what is the minimum accuracy level in the easiest condition that we'll allow


data=data[data$correct!=2,]     # Get rid of "tooFast" and "tooSlow" responses
#data=data[data$blkNum!=1,]     # Get rid of the first block


subs=unique(data$subject)     # Get the IDs of all of the subjects



badSubs=NULL
goodSubs=NULL


## If we're excluding, figure out who the good and the bad subjects are

if (use.exclusions) {
  for (s in subs) {
    isSub = data$subject == s
    subAcc = mean(data$correct[isSub])
    if (subAcc<minAcc) {
      badSubs=c(badSubs,s)
    } else {
      goodSubs=c(goodSubs,s)
    }
  }
}


## Get rid of the bad subjects

for (s in badSubs) {
  data=data[data$subject!=s,]
}



subs=unique(data$subject)     # Get the IDs of all of the subjects
S=length(subs)     # Get how many subjects there are

conds=unique(data$coherentDots)     # Get the names of all of the conditions
n.cond=length(conds)     # Get how many condiitons there are



## Put it into a format that suits my LBA code

parsedData=data
data=NULL


data=list()
for(s in 1:S) {
  data[[s]]=list(Cond=NULL,Time=NULL,Correct=NULL,Stim=NULL)
  isSub=parsedData$subject==subs[s]
  data[[s]]$Cond=parsedData$coherentDots[isSub]
  data[[s]]$Time=parsedData$RT[isSub]/1000
  data[[s]]$Correct=parsedData$correct[isSub]==1
  data[[s]]$trlNum= parsedData$trlNum[isSub]
  data[[s]]$blkNum = parsedData$blkNum[isSub]
  data[[s]]$winningDirection = parsedData$winningDirection[isSub]
}


allData = data
data = NULL
for (i in 1:(length(subs))) {
  #data= as.data.frame(matrix(nrow = 0,ncol = 1))

  data = data.frame(Cond=NULL,Time=NULL,Resp=NULL,Stim=NULL,Trial = NULL, TrialInBlock = NULL, Block = NULL)
  #data = data.frame()
  #isSub=allData$subject==nSubs[subs[i]]
  #parsedData[[i]]$Cond=data$blkNum[isSub]
  Time=allData[[i]]$Time
  Resp=allData[[i]]$Correct+1
  Stim=allData[[i]]$winningDirection
  Trial = 1:length(allData[[i]]$Time)
  TrialInBlock = allData[[i]]$trlNum
  Block = allData[[i]]$blkNum
  Cond= rep(1,length(allData[[i]]$Time))
  PID = i
  
  data = as.data.frame(cbind(PID,Time,Resp,Stim,Trial,TrialInBlock,Block,Cond))
  
  # a bunch of columns get converted to numeric when switching to a data frame
  data$Trial = as.integer(data$Trial)
  data$Time = as.numeric(data$Time)
  data$Resp = as.numeric(data$Resp)
  data$Cond = as.numeric(data$Cond)
  data$Block = as.numeric(data$Block)
  
  save(data,file = paste0("data/knowlesetal-19/clean/P",i
                         ,".Rdata"))
}
# 
# save.image("parsedData-RDK.Rdata")     # Save data to be used for LBA fit
# 
# 
# 
# RDKdata=NULL
# 
# for (s in 1:S) {
#   isSub = parsedData$subject == subs[s]
#   currSubData=subs[s]
#   meanRT=mean(parsedData$RT[isSub])
#   meanACC=mean(parsedData$correct[isSub])
#   
#   currSubData=c(currSubData,meanRT)
#   currSubData=c(currSubData,meanACC)
#   
#   RDKdata=rbind(RDKdata,currSubData)
# }
# 
# colnames(RDKdata)=c("Subject","MRT","ACC")
# 
# rownames(RDKdata)=NULL
# 
# 
# 
# write.csv(RDKdata,file="data/knowlesetal-19/clean/RDKdata.csv")
# 
# 
