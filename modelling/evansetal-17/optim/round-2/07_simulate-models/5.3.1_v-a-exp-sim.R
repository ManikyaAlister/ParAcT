rm(list=ls())
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/optim/round-2/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/optim/round-2/02_deep-background.R"))

conds=c(1,2) # redundant because only one condition

nSub = 10 # number of subjects

####################################
#### Exponential Threshold Model ###
####################################

##### Simulate data Using Parameters ####

for (useSub in 1:nSub) {
  load(here(
    paste(
      "modelling/evansetal-17/optim/round-2/06_output/P",
      useSub,
      "_v-a-exp.Rdata",
      sep = ""
    )
  )) #Loads through the datasets of each participant in nSub
  
  
  simdata = list(Time = NULL,
                 Cond = NULL,
                 Resp = NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  tmp1 = apply(weight, 2, max)
  tmp2 = which.max(tmp1)
  tmp3 = which.max(weight[, tmp2])
  
  blah = theta[tmp2, , tmp3]
  
  for (cond in conds) {
    x = c(blah["v.start"], blah["v.asym"], blah["v.rate"], 0.5, blah["a.start"], blah["a.asym"], blah["a.rate"], blah["t0"]) # set parameters
    names(x) = c("v.start", "v.asym", "v.rate", "z", "a.start", "a.asym", "a.rate", "t0")  # set names of the parameters
    
    tmp = rdiffusion(
      n = 10000,
      a = x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial),
      v = (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial),
      t0 = x["t0"],
      z = x["z"] * x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
    ) # Runs diffusion model to generated data with estimated parameters
    simdata$Time = c(simdata$Time, tmp$rt) # Populates the RT column in the simulated data
    simdata$Resp = c(simdata$Resp, tmp$response) # Populates the Resp column in the simulated data
    simdata$Cond = c(simdata$Cond, rep(cond, length(tmp$rt)))
  } # Populates the Cond column in the simulated data
  
  sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format
  
  save(sim, file = here(
    paste(
      "modelling/evansetal-17/optim/round-2/08_model-predictions/P",
      useSub,
      "_v-a-exp.Rdata",
      sep = ""
    )
  ))
  
}
