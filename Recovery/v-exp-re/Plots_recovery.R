
rm(list = ls())
library(here)
setwd(here())
#setwd("Recovery")


# Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)

allGenParams=NULL
allMeanTheta=NULL

#Define how many data sets to use
n = 100
model = "v-exp-re"

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

# Start a PDF device to save the plots to a PDF file
pdf(paste0("Recovery/figures/recovery-",model,".pdf"), width = 10, height = 10)

# Set the layout for multiple plots in a 2x4 grid (2 rows and 4 columns)
par(mfrow = c(3, 3))
cor = cor(allGenParams$v.start, allMeanTheta$v.start)
plot(allGenParams$v.start, allMeanTheta$v.start, xlab = "Generating", ylab = "Estimated", sub = paste0("r = ", round(cor, 2)), main = "v beta")
cor = cor(allGenParams$v.asym, allMeanTheta$v.asym)
plot(allGenParams$v.asym, allMeanTheta$v.asym, xlab = "Generating", ylab = "Estimated", sub = paste0("r = ", round(cor, 2)), main = "v alpha")
cor = cor(allGenParams$v.rate, allMeanTheta$v.rate)
plot(allGenParams$v.rate, allMeanTheta$v.rate, xlab = "Generating", ylab = "Estimated", sub = paste0("r = ", round(cor, 2)), main = "v rate")
plot(allGenParams$v.rate, allMeanTheta$v.rate, xlab = "Generating", ylab = "Estimated", ylim = c(0,0.05), main = "v rate (restricted y axis)")
cor = cor(allGenParams$ter, allMeanTheta$t0)
plot(allGenParams$ter, allMeanTheta$t0, xlab = "Generating", ylab = "Estimated", sub = paste0("r = ", round(cor, 2)), main = "t0")
cor = cor(allGenParams$z, allMeanTheta$z)
plot(allGenParams$z, allMeanTheta$z, xlab = "Generating", ylab = "Estimated", sub = paste0("r = ", round(cor, 2)), main = "z")
cor = cor(allGenParams$a, allMeanTheta$a)
plot(allGenParams$a, allMeanTheta$a, xlab = "Generating", ylab = "Estimated", sub = paste0("r = ", round(cor, 2)), main = "a")


dev.off()

