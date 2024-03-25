library(dplyr)
library(here)
library(modelProb)

# source plotting function
source(here("functions/plot-fits-across-trials.R"))

dataset <- "evansetal-17"
subset <- "/optim"

# load the 2 parameter models for the data set so we know what file path to call later
models_2p <- load(here("data/",dataset,"/derived",subset,"/2-param-models.Rdata"))

# load the AIC/BIC data so that we can determine what models we want to generate fits for
load(here(paste0("data/",dataset,"/derived",subset,"/allBIC.Rdata")))
load(here(paste0("data/",dataset,"/derived/optim/best_BIC.Rdata")))

# get the relative model weights
weights <- modelProb::weightedICs(allBIC)

# aggregate across participants
average_weights <- apply(weights, 2, mean)

# find which models have an average weight > .1 
fit_models <- names(average_weights[average_weights > .1])

for (i in 1:length(fit_models)){
  model <- fit_models[i]
  # determine which participants were best fit by model 
  subjects <- which(model == best_BIC)
  
  # determine whether it is a 1-parameter model (round 1) or 2-parameter model (round-2)
  round <- ifelse(model %in% models_2p, 2, 1) 
  
  # plot the fits of participants who were best fit by that model
  fitRTAcrossTime(model, subjects,dataset, subset, round)
}
