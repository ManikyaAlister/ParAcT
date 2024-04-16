rm(list = ls())
library(here)
#Define how many data sets to use
n = 100

load_params = function(model, subs = 1:100){
  # Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)
  allGenParams=NULL
  allMeanTheta=NULL
  for (p in subs) { #Loop in each data set
    load(paste0("Recovery/",model,"/Fits_recovery/P",p,"-",model,".RData"))
    load(paste0("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",p,".Rdata"))
    #Rearrange and take out unnecessary values from the generated parameters 
    tmp = c(genParams[,1])
    
    #Create a large data set which combines the mean generated parameters from all data sets
    allGenParams=rbind(allGenParams,tmp)
    #Create a large data set which combines the mean estimated parameters from all data sets
    allMeanTheta=rbind(allMeanTheta,apply(theta,2,mean))
    
    
  }
  params <- list(allMeanTheta, allGenParams)
  params
}

plot_recovery = function(parameter, all_parameters, title){
  allMeanTheta <- all_parameters[[1]]
  allGenParams <- all_parameters[[2]]
  
  if (parameter == "v.asym"){
    gen <- allGenParams[,parameter]+allGenParams[,"v.start"]
    est <- allMeanTheta[,parameter]+allMeanTheta[,"v.start"]
  } else {
    gen <- allGenParams[,parameter]
    est <- allMeanTheta[,parameter]
  }
  

  
  cor = cor(gen, est)
  plot(
    gen,
    est,
    xlab = "Generating",
    ylab = "Estimated",
    sub = paste0("r = ", round(cor, 2)),
    main = title
  )
  abline(a = 0, b = 1, col = "red")
}

# Start a PDF device to save the plots to a PDF file
pdf(paste0("man-figures/all-param-change-recovery.pdf"), width = 14, height = 20)
par(mfrow = c(8, 4), oma = c(0, 0, 0, 0))  # Adjust the bottom margin (oma) to move titles closer

model <- "a-linear"

params <- load_params(model)

plot_recovery("a.b", params, expression(paste("a ", beta, " (slope)")))

plot_recovery("a.c", params, expression(paste("a ", alpha, " (intercept)")))
mtext("a Linear", side = 4, line = 1)


# blank plots to finish the row 
plot.new()

plot.new()

model <- "v-linear"
params <- load_params(model)

plot_recovery("v.b", params, expression(paste("v ", beta, " (slope)")))

plot_recovery("v.c", params, expression(paste("v ", alpha, " (intercept)")))
mtext("v Linear", side = 4, line = 1)

# blank plots to finish the row 
plot.new()

plot.new()


model <- "a-exp"
params <- load_params(model)
plot_recovery("a.start", params, expression(paste("a ", beta, " (start)")))

plot_recovery("a.asym", params, expression(paste("a ", alpha, " (asymptote)")))

plot_recovery("a.rate", params, expression(paste("a ", eta, " (rate)")))
mtext("a Exponential", side = 4, line = 1)

plot.new()

model <- "v-exp"
params <- load_params(model)
plot_recovery("v.start", params, expression(paste("v ", beta, " (start)")))

plot_recovery("v.asym", params, expression(paste("v ", alpha, " (asymptote)")))

plot_recovery("v.rate", params, expression(paste("v ", eta, " (rate)")))
mtext("v Exponential", side = 4, line = 1)

plot.new()


model <- "a-power"
params <- load_params(model)
plot_recovery("a.start", params, expression(paste("a ", beta, " (start)")))

plot_recovery("a.asym", params, expression(paste("a ", alpha, " (asymptote)")))
  
plot_recovery("a.rate", params, expression(paste("a ", eta, " (rate)")))
mtext("a Power", side = 4, line = 1)

plot.new()

model <- "v-power"
params <- load_params(model)
plot_recovery("v.start", params, expression(paste("v ", beta, " (start)")))

plot_recovery("v.asym", params, expression(paste("v ", alpha, " (asymptote)")))

plot_recovery("v.rate", params, expression(paste("v ", eta, " (rate)")))
mtext("v Power", side = 4, line = 1)

plot.new()

model <- "a-dExp"
params <- load_params(model)
plot_recovery("a.start", params, expression(paste("a ", beta, " (start)")))

plot_recovery("a.asym", params, expression(paste("a ", alpha, " (asymptote)")))

plot_recovery("a.rate", params, expression(paste("a ", eta, " (rate)")))

plot_recovery("a.delay", params, expression(paste("a ", gamma, " (delay)")))
mtext("a Delayed Exponential", side = 4, line = 1)


model <- "v-dExp"
params <- load_params(model, subs = c(1:100))
plot_recovery("v.start", params, expression(paste("v ", beta, " (start)")))

plot_recovery("v.asym", params, expression(paste("v ", alpha, " (asymptote)")))

plot_recovery("v.rate", params, expression(paste("v ", eta, " (rate)")))

plot_recovery("v.delay", params, expression(paste("v ", gamma, " (delay)")))
mtext("v Delayed Exponential", side = 4, line = 1)





dev.off()


