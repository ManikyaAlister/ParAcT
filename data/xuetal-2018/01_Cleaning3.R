
setwd("~/cloudstor/ACTC/RT-ACC-analyses/gaze-cueing-data")
rm(list=ls())
library(tidyverse)
library(readxl)
n = 80

#Create empty data-frame to combine all participants

#Participant 44 is missing. Participants 11, 32, and 80 were removed due to "errors during data collection". 

rm_error = c(11, 32, 80) #participants removed due to errors in data collection.
for (i in rm_error) {
  data = read.delim(file = paste("data/raw/AT_",i,"_RESULTS_FILE.txt", sep = ""))
  save(data, file = paste("data/clean/rm-error/P",i,".Rdata",sep=""))
}

#Make empty data frame 

all.data = NULL 

# The first 58 participants are saved in a .txt file, participants 59-63 are in .xlx, and 
# 64-80 are .txt so I have run separate loops. Participants 1-9 are saved as 01-09 in the original 
# files so I ran another loop for them too.

#Create an empty vector calculating the number of trials per participant pre exclusion criteria and post exclusion criteria (helpful for writing the method section). 
ntrials_pre = NULL 
ntrials_post = NULL 

# Clean data 
for (i in 1:9) { 
  if(i %in% c(11, 32, 44, 59, 60, 61, 62, 63, 80)) next #skip over missing participant (44), errors in data collection, and data saved as .xlsx
  data = read.delim(file = paste("data/raw/AT_0",i,"_RESULTS_FILE.txt", sep = ""))
  data$PID = i
  data = select(data, PID, name_trial__, RESPONSE_TIME, RESPONSE_ACCURACY, trial_type, gaze_direct)
  names(data) = c("PID", "trial", "Time", "Resp", "Validity", "Stim")
  data$Time = data$Time/1000
  data$Resp = case_when(
    data$Resp == "hit" ~ 2, # 2 = Correct
    data$Resp == "miss" ~ 1,
    data$Resp == "error" ~ 1) #1 = Incorrect
  data$Validity = case_when(
    data$Validity == "I" ~ "Invalid", # Keep  consistent with other clean data 
    data$Validity == "C" ~ "Valid")
  data$Cond = case_when(data$Validity == "Valid" ~ 1, 
                        data$Validity == "Invalid" ~ 2)  
  #Remove participants with <80% accuracy
  accurate_trials = filter(data, Resp == 2)
  n.accurate = length(accurate_trials$Resp)
  acc = n.accurate/length(data$Resp)
  if (acc < .8) {
    save(data, file = paste("data/clean/rm-accuracy/P",i,".Rdata",sep=""))
  } else {
    ntrials_pre[i] = length(data$trial)
    ntrials_post[i] = length(data$trial)
    save(data, file = paste("data/clean/gaps/P",i,".Rdata",sep="")) #Makes the modeling annoying if there are gaps in the 
    # order, (e.g., P42.Rdata, P43.Rdata, P45.Rdata) so saving these data into a different file and then re-saving without gaps in the order 
  }
}
  

for (i in 10:80) { 
  if(i %in% c(11, 32, 44, 59, 60, 61, 62, 63, 80)) next #skip over missing participant (44), errors in data collection, and data saved as .xlsx
  data = read.delim(file = paste("data/raw/AT_",i,"_RESULTS_FILE.txt", sep = ""))
  data$PID = i
  data = select(data, PID, name_trial__, RESPONSE_TIME, RESPONSE_ACCURACY, trial_type, gaze_direct)
  names(data) = c("PID", "trial", "Time", "Resp", "Validity", "Stim")
  data$Time = data$Time/1000
  data$Resp = case_when(
    data$Resp == "hit" ~ 2, # 2 = Correct
    data$Resp == "miss" ~ 1,
    data$Resp == "error" ~ 1) #1 = Incorrect
  data$Validity = case_when(
    data$Validity == "I" ~ "Invalid", # Keep  consistent with other clean data 
    data$Validity == "C" ~ "Valid")
  data$Cond = case_when(data$Validity == "Valid" ~ 1, 
                        data$Validity == "Invalid" ~ 2)  
  #Remove participants with <80% accuracy
  accurate_trials = filter(data, Resp == 2)
  n.accurate = length(accurate_trials$Resp)
  acc = n.accurate/length(data$Resp)
  if (acc < .8) {
    save(data, file = paste("data/clean/rm-accuracy/P",i,".Rdata",sep=""))
  } else {
    ntrials_pre[i] = length(data$trial)
    save(data, file = paste("data/clean/gaps/P",i,".Rdata",sep=""))
    ntrials_post[i] = length(data$trial)
  }
}

for (i in 59:63) { 
  data = read_xlsx(paste("data/raw/",i,"_loc.xlsx", sep = ""))
  data$PID = i
  data = select(data, PID, name_trial__, RESPONSE_TIME, RESPONSE_ACCURACY, trial_type, gaze_direct)
  names(data) = c("PID", "trial", "Time", "Resp", "Validity", "Stim")
  data$Time = data$Time/1000
  data$Resp = case_when(
    data$Resp == "hit" ~ 2, # 2 = Correct
    data$Resp == "miss" ~ 1,
    data$Resp == "error" ~ 1) #1 = Incorrect
  data$Validity = case_when(
    data$Validity == "I" ~ "Invalid", # Keep  consistent with other clean data 
    data$Validity == "C" ~ "Valid")
  data$Cond = case_when(data$Validity == "Valid" ~ 1, 
                        data$Validity == "Invalid" ~ 2)  
  #Remove participants with <80% accuracy
  accurate_trials = filter(data, Resp == 2)
  n.accurate = length(accurate_trials$Resp)
  acc = n.accurate/length(data$Resp)
  if (acc < .8) {
    save(data, file = paste("data/clean/rm-accuracy/P",i,".Rdata",sep=""))
  } else {
    ntrials_pre[i] = length(data$trial)
    ntrials_post[i] = length(data$trial)
    save(data, file = paste("data/clean/gaps/P",i,".Rdata",sep="")) 
  }
}

#Re-save so that the files are ordered 1-71

#Bring all the data into a big list
nSub = 80
#Bring all data together 
all.data = list()
for (useSub in 1:nSub) {
  if(useSub %in% c(11, 32, 29, 32, 34, 39, 44, 64, 75, 80)) next
  load(paste("data/clean/gaps/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  all.data[[useSub]]=data
  
}
all.data[sapply(all.data, is.null)] = NULL #remove null values 
#Re-save in order 1-71
for (i in 1:71) {
  data = as.data.frame(all.data[i])
  save(data, file = paste("data/clean/P",i,".Rdata",sep=""))
}

#Make a big data frame with all of the participants combined
PID = NULL 
trial = NULL
Time = NULL 
Resp = NULL 
Validity = NULL 
Stim = NULL 
Cond = NULL 
for (i in 1:71) {
  load(paste("data/clean/P",i,".Rdata",sep=""))
  PID = data$PID
  trial = data$trial
  Time = data$Time 
  Resp = data$Resp
  Validity = data$Validity
  Stim = data$Stim
  Cond = data$Cond
  p = cbind(PID, trial, Time, Resp, Validity, Stim, Cond)
  assign(paste("p",i,sep = ""), p)
}


P = rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30,
          p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, p49, p50, p51, p52, p53, p54, p55, p56, p57, p58, p59, p60,
          p61, p62, p63, p64, p65, p66, p67, p68, p69, p70, p71)
P = as.data.frame(P)

P$Time = as.numeric(P$Time)
P$trial = as.numeric(P$trial)


save(P, file = "data/clean/all-participants.Rdata")         








