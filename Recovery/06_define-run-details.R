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
source(here("modelling/model-functions.R"))

# whether or not this is a model recovery 
recovery = TRUE

# whether we want to save a plot 
plot = FALSE

# whether the likelihood is blocked
blocked_likelihood = FALSE

# get the time-varying functions that correspond to the model. 
paract_functions <- all_functions[[model]]

# load data set details  (that provides info on file paths, etc)
source(here("Recovery/05_define-dataset-details-recovery.R"))


