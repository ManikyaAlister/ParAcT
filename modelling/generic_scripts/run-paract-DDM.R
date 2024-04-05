rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/generic_scripts/load-packages.R"))

# it's currently set up to run from the terminal/shell script/HPC. 
# To run from R, change args to a vector of each subject, id, and model
args = commandArgs(trailingOnly = TRUE)

# # data set id 
dataset_id <- args[2]

# # subject (define full vector of subjects if running serially/locally)
subj <- args[1]

# load time varying functions
source(here("modelling/generic_scripts/model-functions.R"))

# model name
model <- args[3]

# get the time-varying functions that correspond to the model. 
paract_functions <- all_functions[[model]]

# load data structure with data set details
load(here("data/dataset-details.Rdata"))

# source fitting script
source(here("modelling/generic_scripts/fit-paract-DDM.R"))

