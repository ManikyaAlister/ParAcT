rm(list=ls())
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/optim/round-1/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/optim/round-1/02_deep-background.R"))
model = "v-blocked-simple"
conds=c(1,2) # redundant because only one condition
blocks = 1:24
nSub = 10 # number of subjects

####################################
#### Exponential Threshold Model ###
####################################

##### Simulate data Using Parameters ####

for (useSub in 1:nSub) {
  load(here(
    paste(
      "modelling/evansetal-17/optim/round-1/06_output/P",
      useSub,
      "_",model,".Rdata",
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
    for (block in blocks)
    x=c(blah["a"],0.5,blah["v"],blah["t0"],blah["step"]) # Sets the value of parameters.
    names(x)=c("a","z","v","t0","step")  # Sets the names of the parameters
      d = (-x["step"] * (block-1) )
    tmp = rdiffusion(
      n = 10000/blocks,
      a = x["a"],
      v = x["v"]-d,
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
      "modelling/evansetal-17/optim/round-1/08_model-predictions/P",
      useSub,
      "_",model,".Rdata",
      sep = ""
    )
  ))
  
}
