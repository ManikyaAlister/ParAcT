# source run details
source(here("Recovery/simple/01_define-run-details.R"))

# run fitting script
source(here("modelling/generic_scripts/fit-paract-DDM.R"))

# plots
plot_recovery_correlations(model = model)
