library(here)
# source run details
source(here("Recovery/generic-recovery-scripts/01_define-run-details.R"))

# define whether it's a recovery
recovery = TRUE

# run fitting script
source(here("modelling/generic_scripts/fit-paract-DDM.R"))

# if the data set is the same as the model, plot the correlation between generating and estimating parameters. 
if (dataset_id == model){
  source(here("Recovery/generic-recovery-scripts/plot_correlations.R"))
  # correlation plots
  plot_recovery_correlations(model = model)
}

