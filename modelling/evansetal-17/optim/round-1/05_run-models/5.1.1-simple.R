rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/generic_scripts/load-packages.R"))

conds <- 1

# data set id 
dataset_id <- "evans-optim"

# model name
model <- "simple"

# subject (define full vector of subjects if running serially/locally)
subj <- 1 #commandArgs(trailingOnly = TRUE)

# load time varying functions
source(here("modelling/generic_scripts/time-varying-functions.R"))

# define paract functions (how/if any parameters vary across time)
paract_functions <- list(
  a = a_standard,
  v = v_standard,
  z = z_standard,
  t0 = t_standard
)

source(here("modelling/generic_scripts/run-paract-DDM.R"))