rm(list = ls())
library(dplyr)
library(here)
library(modelProb)
library(ggpubr)

# source plotting function
source(here("functions/plot-fits-across-trials.R"))
# source data set details with file paths 
source(here("modelling/define-dataset-details.R"))

dataset_id <- "dutilh" #commandArgs(trailingOnly = TRUE)

dataset_index <- which(dataset_details$dataset_id == dataset_id)
subjects <- dataset_details$n_subjects[dataset_index]
derived_data <- dataset_details$save_IC_path[dataset_index]
output_path <- dataset_details$save_output_path[dataset_index][[1]]
data_path <- dataset_details$load_data_path[dataset_index][[1]]

# load 2 param models 
load(here(paste0(derived_data,"round-2-models.Rdata")))

# load the AIC/BIC data so that we can determine what models we want to generate fits for
load(here(paste0(derived_data,"allBIC.Rdata")))
load(here(paste0(derived_data,"best_BIC.Rdata")))


#load(here(paste0("data/",dataset,"/derived",subset,"/allBIC.Rdata")))
#load(here(paste0("data/",dataset,"/derived/optim/best_BIC.Rdata")))

# get the relative model weights
weights <- modelProb::weightedICs(allBIC)

# aggregate across participants
average_weights <- apply(weights, 2, mean)

# get the top 4 models per their average weight
best_models <- sort(average_weights, decreasing = TRUE)[1:4]

# find which models have an average weight > .1 
#fit_models <- names(average_weights[average_weights > .1])
fit_models <- names(best_models)

# set up empty plot list
plot_list <- list()

for (i in 1:length(fit_models)){
  model <- fit_models[i]
  # determine which participants were best fit by model 
  subjects <- which(best_BIC == model)
  #print(subjects)
  
  # # determine whether it is a 1-parameter model (round 1) or 2-parameter model (round-2)
  # round <- ifelse(model %in% unique_2p_best, 2, 1) 
  
  # plot the fits of participants who were best fit by that model
  plot <- fitRTAcrossTime(model, subjects,output_path = output_path, data_path = data_path, dataset_id = dataset_id)
  
  # save in plot list 
  plot_list[[i]] <- plot
}


ggarrange(plotlist = plot_list, common.legend = TRUE, nrow=2, ncol = 2, legend = "bottom")
ggsave(filename = paste0("man-figures/fits-across-trials-",dataset_id,".png"), width = 12, height = 6)

