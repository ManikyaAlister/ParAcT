rm(list=ls())
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/optim/round-2/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/optim/round-2/02_deep-background.R"))

conds=c(1) # redundant because only one condition

nSub = 10 # number of subjects

####################################
#### Exponential Threshold Model ###
####################################

##### Simulate data Using Parameters ####

for (useSub in 1:nSub) {
  print(useSub)
  load(here(
    paste(
      "modelling/evansetal-17/optim/round-1/06_output/P",
      useSub,
      "_a-exp.Rdata",
      sep = ""
    )
  )) #Loads through the datasets of each participant in nSub
  
  
  simdata = list(Time = NULL,
                 Cond = NULL,
                 Resp = NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  tmp1 = apply(weight, 2, max)
  tmp2 = which.max(tmp1)
  tmp3 = which.max(weight[, tmp2])
  
best_values= theta[tmp2, , tmp3]
 x = best_values 
  for (cond in conds) {
    
    tmp = rdiffusion(
      n = 10000,
      a = x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial),
      v = x["v"],
      t0 = x["t0"],
      z = x["z"] * (x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial))
    ) # Runs diffusion model to generated data with estimated parameters
    simdata$Time = c(simdata$Time, tmp$rt) # Populates the RT column in the simulated data
    simdata$Resp = c(simdata$Resp, tmp$response) # Populates the Resp column in the simulated data
    simdata$Cond = c(simdata$Cond, rep(cond, length(tmp$rt)))
  } # Populates the Cond column in the simulated data
  
  sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format
  
  save(sim, file = here(
    paste(
      "modelling/evansetal-17/optim/round-1/08_model-predictions/P",
      useSub,
      "_a-exp.Rdata",
      sep = ""
    )
  ))
  print(paste0(useSub, " out of ", nSub))
}
