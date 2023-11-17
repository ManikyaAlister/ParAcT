
rm(list = ls())
library(here)
setwd(here())
#setwd("Recovery")


# Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)

allGenParams=NULL
allMeanTheta=NULL

#Define how many data sets to use
n = 100
model = "a-exp-rr"

for (p in c(1:24,26:59,61:83,85:n)) { #Loop in each data set
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
cor = cor(allGenParams$a.start, allMeanTheta$a.start)
plot(
  allGenParams$a.start,
  allMeanTheta$a.start,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(cor, 2)),
  main = expression(paste("a ", beta, " (start)"))
)
abline(a = 0, b = 1, col = "red")
cor = cor(allGenParams$a.asym, allMeanTheta$a.asym)
plot(
  allGenParams$a.asym,
  allMeanTheta$a.asym,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(cor, 2)),
  main = expression(paste("a ", alpha, " (asymptote)"))
)
abline(a = 0, b = 1, col = "red")

cor = cor(allGenParams$a.rate, allMeanTheta$a.rate)
plot(
  allGenParams$a.rate,
  allMeanTheta$a.rate,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(cor, 2)),
  main = expression(paste("a ", eta, " (rate)"))
)
abline(a = 0, b = 1, col = "red")

cor = cor(allGenParams$ter, allMeanTheta$t0)
plot(
  allGenParams$ter,
  allMeanTheta$t0,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(cor, 2)),
  main = "t0"
)
abline(a = 0, b = 1, col = "red")

cor = cor(allGenParams$z, allMeanTheta$z)
plot(
  allGenParams$z,
  allMeanTheta$z,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(cor, 2)),
  main = "z"
)
abline(a = 0, b = 1, col = "red")

cor = cor(allGenParams$v, allMeanTheta$v)
plot(
  allGenParams$v,
  allMeanTheta$v,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(cor, 2)),
  main = "v"
)
abline(a = 0, b = 1, col = "red")



dev.off()

