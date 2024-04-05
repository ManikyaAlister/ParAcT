
# Script for fitting paract-DDM (should be sourced from run_model  --------

# if a DDM parameter function has more than one parameter it is time varying
time_varying <- unlist(lapply(paract_functions, function(x)
  length(x) > 1))

# check how many time varying parameters there are
n_time_varying <- sum(time_varying)

#  calculate  whether this is a round 1 model (only 1 time-varying parameter) or round 2 model (not relevant for recovery)
if (n_time_varying > 1) {
  analysis_round <- 2
} else {
  analysis_round <- 1
}

# get data set details (for file paths) for specific data set
selected_index <- which(dataset_details$dataset_id == dataset_id)
dataset_details <- lapply(dataset_details, function(x) x[selected_index])

# extract key info
dataset <- dataset_details$dataset
load_data_path <- dataset_details$load_data_path[[1]]
save_output_path <- dataset_details$save_output_path[[1]]
save_IC_path <- dataset_details$save_IC_path
simulate_fits <- dataset_details$simulate_fits

# source background code for MCMC 
source(file = here("modelling/generic_scripts/deep-background.R"))

# Run DDM for each subject or a specific subject if running in parallel
for (useSub in subj) {
  
  # So I can see the data set 
  print(paste0("Data set id: ",dataset_id))
  
  # So I can see which model is running
  print(paste0("Model: ", model))
  
  # so I can see what subject is running
  print(paste0("Participant: ", useSub))
  
  # load data
  load(here(load_data_path()))
  
  # identify unique response stimuli in data (eg., left/right)
  stims <- unique(data$Stim)
  if (length(stims) > 2 ) stop("More than two response stimuli detected in the data set.")
  
  # generate and set seed
  newSeed = Sys.time()
  set.seed(as.numeric(newSeed))
  
  # log likelihood function
  log.dens.like = function (x, data, par.names, functions = paract_functions) {
    out = 0
    names(x) = par.names
    
    for (stim in stims) {
      a = functions$a(x)
      t0 = functions$t0(x)
      v = functions$v(x)
      z = functions$z(x, stimulus = stim)
      sv = 0
      sz = 0
      st0 = 0
      s = 1
      tmp = ddiffusion(
        rt = data$Time[data$Stim == stim],
        response = data$Resp[data$Stim == stim],
        z = z * a,
        a = a,
        v = v,
        t0 = t0 - (st0 / 2),
        s = s,
        sv = sv,
        sz = sz,
        st0 = st0
      )
      out = out + sum(log(pmax(tmp, 1e-10)))
    }
    out
  }
  
  # list parameter names so we knows what to call from priors scripts
  theta.z = get_function_variables(paract_functions$z) # function that extracts the parameters from the time-varying (or standard DDM) functions.
  theta.t = get_function_variables(paract_functions$t0)
  theta.v = get_function_variables(paract_functions$v)
  theta.a = get_function_variables(paract_functions$a)
  
  theta.names = c(theta.z, theta.a, theta.v, theta.t)
  
  # define file paths for output
  savefile = here(
      save_output_path()
  )

  
  # source priors
  source(here("modelling/generic_scripts/priors.R"))
  
  # run MCMC process
  source(here("modelling/generic_scripts/iterative-process.R"))
  
  # calculate the number of parameters (for AIC and BIC)
  n.pars = length(theta.names)
  
  # calculate AIC and BIC
  AIC = -2 * max(weight) + 2 * n.pars
  BIC = log(length(data$Time)) * n.pars - 2 * max(weight)
  
  # save model output
  save(AIC,
       BIC,
       theta,
       weight,
       data,
       burnin,
       nmc,
       n.chains,
       theta.names,
       stims,
       file = savefile)
  
  
  # full output file is quite big, so sometimes you might want to save model comparison data separately
  if(save_IC_path != FALSE){
  saveIC = here(
    paste(
      save_IC_path,
      "/P",
      useSub,
      "_",
      model,
      "-IC.Rdata",
      sep = ""
    )
  )
  
  save(AIC, BIC, file = saveIC)
  }
  
  if(simulate_fits){
    # simulate predictions from estimated parameters (for model fits)
    source(here("modelling/generic_scripts/simulate-paract-DDM.R"))
  }
  
}



