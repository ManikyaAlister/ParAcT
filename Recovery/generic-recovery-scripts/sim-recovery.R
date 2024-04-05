library(lhs)

# define how many "participants" you want to simulate
n_participants <- 100

# define how many trials for each simulated participant
n_trials = 1000

# list parameter names so we knows what to call from priors scripts
theta.z = get_function_variables(paract_functions$z) # function that extracts the parameters from the time-varying (or standard DDM) functions.
theta.t = get_function_variables(paract_functions$t0)
theta.v = get_function_variables(paract_functions$v)
theta.a = get_function_variables(paract_functions$a)

theta.names = c(theta.z, theta.a, theta.v, theta.t, "stoch.s", "sz", "sv", "ster")

# source parameter ranges
source(here("Recovery/generic-recovery-scripts/parameter-ranges.R"))

# get the number of parameters
nParams <- length(theta.names)

# only use parameter ranges for parameters being recovered
use.range <- use.range[theta.names, ]
colnames(use.range) = c("Min", "Max")


# set up response stimuli
stims <- c(1, 2)

# create a vector of uniqe stims of length trials to sample from
# note: for this recovery we are not interested in time varying z, but if you were, you might want to make this these uneven (e.g., 2/3rds one stimulus)
all_stims <- c(rep(stims[1], n_trials / 2), rep(stims[2], n_trials / 2))

# set up hyper cube
use.LHS <- randomLHS(n = n_participants, k = nParams)
colnames(use.LHS) <- theta.names

# initiate Latin hypercube sampling with defined ranges for each parameter
for (useParam in theta.names) {
  use.LHS[, useParam] = use.range[useParam, "Min"] +
    use.LHS[, useParam] * (use.range[useParam, "Max"] - use.range[useParam, "Min"])
}

conds = 1

source("Recovery/generic-recovery-scripts/02_simulate-DIFF.R")

# Simulate all data sets with defined parameters

for (i in 1:nrow(use.LHS)) {
  cat("\n", i, "/", nrow(use.LHS))
  
  # Set up simulated data storage
  data = list(
    Time = NULL,
    Resp = NULL,
    Stim = NULL,
    Trial = NULL
  )
  
  # set up vector of trials
  all_trials <- 1:n_trials
  
  # sample form all stims randomly so that they are not time dependent on trial
  sample_stim <- sample(all_stims, n_trials, replace = FALSE)
  
  # set up empty genParams. Single column data frame as this means you can add other columns if simulating different conditions.
  genParams = array(NA, c(nParams, conds), dimnames = list(theta.names, conds))
  
  # Set up generating parameters storage
  genParams[, 1] = as.numeric(use.LHS[i, theta.names])
  
  # Loop over conditions
  for (stim in stims) {
    # filter stim and trial vectors based on stim (each trial needs to link to its corresponding stim)
    stim_loop <- sample_stim[sample_stim == stim]
    stim_trials <- all_trials[sample_stim == stim]
    
    # MA: Z is conditionally updated from stim in simulate.DIFF function (within the z time varying function) now
    
    # Simulate
    tmp = simulate.DIFF(
      trials = stim_trials,
      params = genParams[, 1],
      maxCounter = 50000,
      stepSize = 0.001,
      use.table = use.table,
      n.table.options = n.table.options
    )
    
    # Store sim data
    data$Time = c(data$Time, tmp$rt)
    data$Resp = c(data$Resp, tmp$resp)
    data$Stim = c(data$Stim, stim_loop)
    data$Trial = c(data$Trial, tmp$trial)
    
  }
  
  # convert to data frame
  data <- do.call(data.frame, data)
  
  # re-order trials
  data <- data[order(data$Trial),]
  
  #Save sim data
  save(file=paste("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata",sep=""),data,genParams,stims)
}