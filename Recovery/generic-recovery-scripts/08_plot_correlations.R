
rm(list = ls())
library(here)

plot_recovery_correlations = function(model, n = 100){
  # Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)
  allGenParams=NULL
  allMeanTheta=NULL

  
  for (p in 1:n) { #Loop in each data set
    #load(paste0("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",p,".Rdata"))
    load(paste0("Recovery/",model,"/Fits_recovery/P",p,"-",model,".RData"))
    
    if (stims[1] == 2) {
      theta[,"z",] = 1 - theta[,"z",]
    }
    load(paste0("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",p,".Rdata"))
    
    
    #Rearrange and take out unnecessary values from the generated parameters 
    tmp = c(genParams[,1])
    
    #Create a large data set which combines the mean generated parameters from all data sets
    allGenParams=rbind(allGenParams,tmp)
    #Create a large data set which combines the mean estimated parameters from all data sets
    allMeanTheta=rbind(allMeanTheta,apply(theta,2,mean))
  }

  #Create vDiff columns 
  
  allGenParams= as.data.frame(allGenParams)
  allGenParams$t0 = allGenParams$ter
  allMeanTheta= as.data.frame(allMeanTheta)
  
  # Start a PDF device to save the plots to a PDF file
  pdf(paste0("Recovery/",model,"/recovery-",model,".pdf"), width = 10, height = 10)
  
  
  # Determine the layout based on the number of parameters
  numParams = min(ncol(allGenParams), ncol(allMeanTheta))
  par(mfrow = c(ceiling(numParams/3), 3))
  
  for (param in intersect(names(allGenParams), names(allMeanTheta))) {
    gen_param = allGenParams[[param]]
    est_param = allMeanTheta[[param]]
    
    if (param == "v.asym"){
      gen_param = gen_param + allGenParams[["v.start"]]
      est_param = est_param + allMeanTheta[["v.start"]]
    }
    
    corr = cor(gen_param, est_param, use = "complete")
    plot(gen_param, est_param, 
         xlab = "Generating", ylab = "Estimated", 
         sub = paste0("r = ", round(corr, 2)), 
         main = param)
    abline(a = 0, b = 1, col = "red")
  }
  dev.off()
}

plot_recovery_correlations("a-linear")


