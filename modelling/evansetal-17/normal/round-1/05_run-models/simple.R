rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/generic_scripts/load-packages.R"))

args = commandArgs(trailingOnly = TRUE)
print(args)

# # data set id 
dataset_id <- args[2]#"evans-normal" # commandArgs(trailingOnly = TRUE)[2]
# 
# # subject (define full vector of subjects if running serially/locally)
subj <- args[1] # commandArgs(trailingOnly = TRUE)[1]

# load time varying functions
source(here("modelling/generic_scripts/model-functions.R"))

# # model name
model <- args[3]

paract_functions <- all_functions[[model]]
# 
# # define paract functions (how/if any parameters vary across time)
# paract_functions <- list(
#   a = a_standard,
#   v = v_standard,
#   z = z_standard,
#   t0 = t0_standard
# )

# load data structure with data set details
load(here("data/dataset-details.Rdata"))

# source fitting script
source(here("modelling/generic_scripts/fit-paract-DDM.R"))

