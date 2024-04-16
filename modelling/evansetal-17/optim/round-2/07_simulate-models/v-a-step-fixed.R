rm(list=ls())
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/optim/round-2/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/optim/round-2/02_deep-background.R"))

conds=c(1) # redundant because only one condition

nSub = 10 # number of subjects

model = "v-a-step-fixed"

####################################
#### Exponential Threshold Model ###
####################################

##### Simulate data Using Parameters ####

for (useSub in 1:nSub) {
  print(paste0("Starting participant ",useSub))
  load(here(
    paste(
      "modelling/evansetal-17/optim/round-2/06_output/P",
      useSub,
      "_",model,".Rdata",
      sep = ""
    )
  )) #Loads through the datasets of each participant in nSub
  
  
  simdata = list(Time = NULL,
                 Cond = NULL,
                 Resp = NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  # get the best fitting parameters
  tmp1 = apply(weight, 2, max)
  tmp2 = which.max(tmp1)
  tmp3 = which.max(weight[, tmp2])
  
  best_values = theta[tmp2, , tmp3]
  blocks <- data$Block
  noFeedbackBlocks = 1:4
  for(block in blocks){
  for (cond in conds) {
    x =  best_values    
    tmp = rdiffusion(
      n = 10000/length(blocks),
      a=ifelse(block %in% noFeedbackBlocks, x["initial"], x["initial"]-x["step"]),
      v=ifelse(block %in% noFeedbackBlocks, x["initial.v"], x["initial.v"]+x["step.v"]),
      t0 = x["t0"],
      z = x["z"]*ifelse(block %in% noFeedbackBlocks, x["initial"], x["initial"]-x["step"])
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
  
}

  