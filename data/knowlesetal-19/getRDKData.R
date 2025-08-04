## 26/02 Realised that 4 participants did the experiment twice, which meant that they were recoded as having 320 trials. Made edits to remove the second session of these participants. 
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
    
    # Extract subject number
    parts = unlist(strsplit(n, "_"))
    subject_num = as.numeric(parts[1])
    
    # Extract date from filename
    date_str = gsub(".txt$", "", paste(parts[-1], collapse = "_"))
    
    remove_ordinal <- function(date_str) {
      gsub("([0-9]+)(st|nd|rd|th)", "\\1", date_str)
    }
    
    date_cleaned <- remove_ordinal(date_str)
    
    # Replace underscores with colons for proper time parsing
    date_cleaned <- gsub("_", ":", date_cleaned)
    
    # Correct format string
    date_parsed <- as.POSIXct(date_cleaned, format = "%A %d of %B %Y %I:%M:%S %p", tz = "UTC")
    
    
    tmp=read.table(file=paste("data/knowlesetal-19/raw/",n,sep=""),header=TRUE)
    data=rbind(data,cbind(subject=snum,tmp, date_collected = date_parsed))
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
  data[[s]]$date_collected = parsedData$date_collected[isSub]
}


allData = data
data = NULL
for (i in 1:length(subs)) {
  data = data.frame(
    PID = i,
    Time = as.numeric(allData[[i]]$Time),  
    Resp = as.numeric(allData[[i]]$Correct) + 1,
    Stim = allData[[i]]$winningDirection,
    Trial = as.integer(1:length(allData[[i]]$Time)),
    TrialInBlock = allData[[i]]$trlNum,
    Block = as.numeric(allData[[i]]$blkNum),
    Cond = rep(1, length(allData[[i]]$Time)),
    Date_Collected = allData[[i]]$date_collected  
    )
  
  # If max trial > 160, keep only trials from the earliest date
  if (max(data$Trial) > 160) {
    min_date <- min(data$Date_Collected, na.rm = TRUE)
    data <- data[data$Date_Collected == min_date, ]
  }

  # remove date collected column since we don't need it anymore
  data$Date_Collected <- NULL

  
  save(data, file = paste0("data/knowlesetal-19/clean/P", i, ".Rdata"))
}
