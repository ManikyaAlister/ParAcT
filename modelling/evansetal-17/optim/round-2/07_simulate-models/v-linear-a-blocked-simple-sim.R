rm(list=ls())
## Load Packages ## 
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(rtdists,lib.loc = lib)
library(msm, lib.loc = lib)
library(here, lib.loc = lib)

source(file = here("modelling/evansetal-17/optim/round-2/02_deep-background.R"))

model = "v-linear-a-blocked-simple"
conds=1

nSub = 10

####################
#### Linear Model###
####################

for (useSub in 1:nSub) {
  load(here(
    paste(
      "modelling/evansetal-17/optim/round-2/06_output/P",
      useSub,
      "_",model,".Rdata",
      sep = ""
    )
  )) #Loads through the datasets of each participant in nSub
  
  blocks = unique(data$Block)
  
  simdata = list(Time = NULL,
                 Cond = NULL,
                 Resp = NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  # get the best fitting parameters
  tmp1 = apply(weight, 2, max)
  tmp2 = which.max(tmp1)
  tmp3 = which.max(weight[, tmp2])
  
  best_values = theta[tmp2, , tmp3]
  
  for (cond in conds) {
    for (block in blocks){
    x = best_values
    d = (-x["step"] * (block-1) )
    tmp = rdiffusion(
      n = 10000/max(blocks),
      a = x["a"]-d,
      v = (x["v.b"]*data$Trial)+x["v.c"],
      t0 = x["t0"],
      z = x["z"] * (x["a"]-d)
    ) # Runs diffusion model to generated data with estimated parameters
    simdata$Time = c(simdata$Time, tmp$rt) # Populates the RT column in the simulated data
    simdata$Resp = c(simdata$Resp, tmp$response) # Populates the Resp column in the simulated data
    simdata$Cond = c(simdata$Cond, rep(cond, length(tmp$rt)))
  } # Populates the Cond column in the simulated data
  }
  sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format
  
  save(sim, file = here(
    paste(
      "modelling/evansetal-17/optim/round-2/08_model-predictions/P",
      useSub,
      "_",model,".Rdata",
      sep = ""
    )
  ))
  print(paste0(useSub, " out of ", nSub))
}



