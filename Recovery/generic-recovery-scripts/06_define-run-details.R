rm(list = ls())
library(here)

# it's currently set up to run from the terminal/shell script/HPC. 
# To run from R, change args to a vector of each subject, id, and model
args = commandArgs(trailingOnly = TRUE)

# # data set id 
dataset_id <- args[2]

# subject (define full vector of subjects if running serially/locally)
subj <- args[1]

# model name
model <- args[3]

# load time varying functions
source(here("modelling/generic_scripts/model-functions.R"))

# whether or not this is a model recovery 
recovery = TRUE

# get the time-varying functions that correspond to the model. 
paract_functions <- all_functions[[model]]

# load data set details  (that provides info on file paths, etc)
source(here("Recovery/generic-recovery-scripts/05_define-dataset-details-recovery.R"))


