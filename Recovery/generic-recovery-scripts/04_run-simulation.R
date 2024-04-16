library(here)
# define all models that will be recovered
recovery_models <- c("v-power")
  #c("simple", "a-linear", "a-exp", "a-power","a-dExp", "v-linear","v-exp", "v-dExp", "v-power")

# define how many "participants" you want to simulate
n_participants <- 1

# define how many trials for each simulated participant
n_trials = 1000

# source the paract functions
source(here("modelling/generic_scripts/model-functions.R"))

# loop through all models and simulate their data sets
for (model in recovery_models){
  
  # get the time-varying functions that correspond to the model. 
  paract_functions <- all_functions[[model]]
  
  # simulate the data sets
  source(here("Recovery/generic-recovery-scripts/03_sim-recovery.R"))
}



