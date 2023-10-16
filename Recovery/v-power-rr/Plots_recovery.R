
rm(list = ls())
library(here)
setwd(here())
#setwd("Recovery")


# Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)

allGenParams=NULL
allMeanTheta=NULL

#Define how many data sets to use
n = 100
model = "v-power-rr"

for (p in 1:n) { #Loop in each data set
  load(paste0("Recovery/",model,"/Fits_recovery/P",p,"_",model,".RData"))
  #load(paste0("Recovery/Datasets/RECOVERY_DATA-DIFF_LHS-",p,".Rdata"))
  #Rearrange and take out unnecessary values from the generated parameters 
  tmp = c(genParams[,1])
  
  #Create a large data set which combines the mean generated parameters from all data sets
  allGenParams=rbind(allGenParams,tmp)
  #Create a large data set which combines the mean estimated parameters from all data sets
  allMeanTheta=rbind(allMeanTheta,apply(theta,2,mean))
}


#Create vDiff columns 

allGenParams= as.data.frame(allGenParams)
allMeanTheta= as.data.frame(allMeanTheta)

cor(allGenParams$v.start, allMeanTheta$v.start)
cor(allGenParams$v.asym, allMeanTheta$v.asym)
cor(allGenParams$v.rate, allMeanTheta$v.rate)
cor(allGenParams$a, allMeanTheta$a)
cor(allGenParams$ter, allMeanTheta$t0)
cor(allGenParams$z, allMeanTheta$z)


plot(allGenParams$v.start, allMeanTheta$v.start)
plot(allGenParams$v.asym, allMeanTheta$v.asym)
plot(allGenParams$v.rate, allMeanTheta$v.rate)
plot(allGenParams$a, allMeanTheta$a)
plot(allGenParams$ter, allMeanTheta$t0)
plot(allGenParams$z, allMeanTheta$z)

