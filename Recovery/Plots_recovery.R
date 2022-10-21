
rm(list = ls())
setwd("~/Documents/2021/Gaze-Cueing/Recovery")

# Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)

allGenParams=NULL
allMeanTheta=NULL

#Define how many data sets to use
n = 100

for (p in 1:n) { #Loop in each data set
  load(paste("Fits_recovery/fits_recovery_P",p,".RData", sep = ""))
  
  #Rearrange and take out unnecessary values from the generated parameters 
  tmp = c(genParams[2,2], genParams[3,1], genParams[3,2], genParams[4,1], genParams[4,2], genParams[1,2])
  names(tmp) = c("a", "t0.-0.5","t0.0.5", "v.-0.5", "v.0.5", "z")
  
  #Create a large data set which combines the mean generated parameters from all data sets
  allGenParams=rbind(allGenParams,tmp)
  #Create a large data set which combines the mean estimated parameters from all data sets
  allMeanTheta=rbind(allMeanTheta,apply(theta,2,mean))
}
#Create vDiff columns 

allGenParams= as.data.frame(allGenParams)
allMeanTheta= as.data.frame(allMeanTheta)
allGenParams$vDiff = allGenParams$v.0.5 - allGenParams$`v.-0.5`
allMeanTheta$vDiff = allMeanTheta$v.0.5 - allMeanTheta$`v.-0.5`
allGenParams$t0Diff = allGenParams$t0.0.5 - allGenParams$`t0.-0.5`
allMeanTheta$t0Diff = allMeanTheta$t0.0.5 - allMeanTheta$`t0.-0.5`

#Save the generated parameters
save(allGenParams, file = "All_Fits/Generated_Paramaters.RData")
save(allMeanTheta, file = "All_Fits/Estimated_Paramaters.RData")

#If I want to reload
load("~/Documents/2021/Gaze-Cueing/Recovery/All_Fits/Estimated_Paramaters.RData")
load("~/Documents/2021/Gaze-Cueing/Recovery/All_Fits/Generated_Paramaters.RData")

cor_vDiff = cor(allGenParams[,"vDiff"],allMeanTheta[,"vDiff"])
cor_z = cor(allGenParams[,"z"],allMeanTheta[,"z"])
cor_vNeg = cor(allGenParams[,"v.-0.5"],allMeanTheta[,"v.-0.5"])
cor_vPos = cor(allGenParams[,"v.0.5"],allMeanTheta[,"v.0.5"])
cor_a = cor(allGenParams[,"a"],allMeanTheta[,"a"])
cor_t0Neg = cor(allGenParams[,"t0.-0.5"],allMeanTheta[,"t0.-0.5"])
cor_t0Pos = cor(allGenParams[,"t0.0.5"],allMeanTheta[,"t0.0.5"])
cor_t0Diff = cor(allGenParams[,"t0Diff"],allMeanTheta[,"t0Diff"])
res_vDiff = allGenParams[,"vDiff"] - allMeanTheta[,"vDiff"]
res_z = allGenParams[,"v.0.5"] - allMeanTheta[,"v.0.5"]
cor_res = cor(res_vDiff, res_z)


plot(allGenParams[,"z"],allMeanTheta[,"z"], 
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_z),
     main = "Z")

plot(allGenParams[,"v.-0.5"],allMeanTheta[,"v.-0.5"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_vNeg),
     main = "V.-0.5")

plot(allGenParams[,"v.0.5"],allMeanTheta[,"v.0.5"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_vPos),
     main = "V.0.5")

plot(allGenParams[,"vDiff"],allMeanTheta[,"vDiff"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_vDiff),
     main = "vDiff")


plot(allGenParams[,"t0.-0.5"],allMeanTheta[,"t0.-0.5"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_t0Neg),
     main = "t0.-0.5")

plot(allGenParams[,"t0.0.5"],allMeanTheta[,"t0.0.5"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_t0Pos),
     main = "t0.0.5")

plot(allGenParams[,"t0Diff"],allMeanTheta[,"t0Diff"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_t0Diff),
     main = "t0Diff")

plot(res_vDiff, res_z,
     sub = paste("r =",cor_res),
     main = "Residuals")





all_cor = cbind(cor_z, cor_vNeg, cor_vPos, cor_vDiff, cor_a, cor_t0Neg, cor_t0Pos, cor_t0Diff)
#         cor_z  cor_vNeg  cor_vPos cor_vDiff     cor_a cor_t0Neg cor_t0Pos cor_t0Diff
#       0.9909854 0.9788627 0.9755001 0.9400272 0.9649534 0.9811749 0.9818783  0.9559584


#MAYBE spearman's rho


rbind(allGenParams, allMeanTheta)
