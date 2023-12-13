
rm(list = ls())
library(here)
setwd(here())
#setwd("Recovery")


# Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)

allGenParams=NULL
allMeanTheta=NULL

#Define how many data sets to use
n = 100
model = "v-linear"

for (p in 1:n) { #Loop in each data set
  load(paste0("Recovery/",model,"/Fits_recovery/P",p,"_",model,".RData"))
  #Rearrange and take out unnecessary values from the generated parameters 
  tmp = c(genParams[,1])
  
  #Create a large data set which combines the mean generated parameters from all data sets
  allGenParams=rbind(allGenParams,tmp)
  #Create a large data set which combines the mean estimated parameters from all data sets
  allMeanTheta=rbind(allMeanTheta,apply(theta,2,mean))
}

allGenParams= as.data.frame(allGenParams)
allMeanTheta= as.data.frame(allMeanTheta)


# Start a PDF device to save the plots to a PDF file
pdf(paste0("Recovery/figures/recovery-",model,".pdf"), width = 10, height = 10)

# Set the layout for multiple plots in a 2x4 grid (2 rows and 4 columns)
par(mfrow = c(3, 3))
corr = cor(allGenParams$v.c, allMeanTheta$v.c, use = "complete")
plot(
  allGenParams$v.c,
  allMeanTheta$v.c,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(corr, 2)),
  main = expression(paste("v ", alpha, " (intercept)"))
)
abline(a = 0, b = 1, col = "red")  

corr = cor(allGenParams$v.b, allMeanTheta$v.b, use = "complete")
plot(
  allGenParams$v.b,
  allMeanTheta$v.b,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(corr, 2)),
  main = expression(paste("v ", beta, " (slope)"))
)
abline(a = 0, b = 1, col = "red")  

corr = cor(allGenParams$ter, allMeanTheta$t0)
plot(
  allGenParams$ter,
  allMeanTheta$t0,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(corr, 2)),
  main = "t0"
)
abline(a = 0, b = 1, col = "red")  

corr = cor(allGenParams$z, allMeanTheta$z)
plot(
  allGenParams$z,
  allMeanTheta$z,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(corr, 2)),
  main = "z"
)
abline(a = 0, b = 1, col = "red")  

corr = cor(allGenParams$a, allMeanTheta$a)
plot(
  allGenParams$a,
  allMeanTheta$a,
  xlab = "Generating",
  ylab = "Estimated",
  sub = paste0("r = ", round(corr, 2)),
  main = "a"
)
abline(a = 0, b = 1, col = "red")  



dev.off()

