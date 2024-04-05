rm(list = ls())
library(here)

# model being recovered
model <- "simple"

# load time-varying functions
source(here("modelling/generic_scripts/time-varying-functions.R"))

# define paract functions (how/if any parameters vary across time)
paract_functions <- list(a = a_standard,
                         v = v_standard,
                         z = z_standard,
                         t0 = t0_standard)

# define subjects to fit
subj <- 1:100

# define data set id
dataset_id <- model

# load data set details  (that provides info on file paths, etc)
source(here("Recovery/generic-recovery-scripts/define-dataset-details-recovery.R"))
