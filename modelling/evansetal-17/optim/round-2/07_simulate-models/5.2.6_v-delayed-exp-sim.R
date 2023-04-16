rm(list=ls())

## Load Packages ## 
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(rtdists,lib.loc = lib)
library(msm, lib.loc = lib)
library(here, lib.loc = lib)

source(file = here("modelling/evansetal-17/optim/round-2/02_deep-background.R"))

conds=1 # redundant because only one condition


nSub = 10 # number of subjects

##### Simulate data Using Parameters ####

for (useSub in 1:nSub) {
  load(here(
    paste(
      "modelling/evansetal-17/optim/round-2/06_output/P",
      useSub,
      "_v-delayed-exp.Rdata",
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
    x = c(blah["v.start"], blah["v.asym"], blah["v.rate"], blah["v.delay"],0.5, blah["a"], blah["t0"]) # set parameters
    names(x) = c("v.start", "v.asym", "v.rate","v.delay", "z", "a", "t0")  # set names of the parameters
    
    tmp = rdiffusion(
      n = 10000,
      a = x["a"],
      v = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial))),
      t0 = x["t0"],
      z = x["z"] * x["a"]
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
      "_v-delayed-exp.Rdata",
      sep = ""
    )
  ))
  
}

