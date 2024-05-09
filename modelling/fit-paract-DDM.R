
# Script for fitting paract-DDM (should be sourced from run_model)  --------
library(msm)
library(rtdists)


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
save_IC_path <- dataset_details$save_IC_path[[1]]
simulate_fits <- dataset_details$simulate_fits
if (plot) plot_path <- dataset_details$save_plot_paths[[1]]

# source background code for MCMC 
source(file = here("modelling/deep-background.R"))

# Run DDM for each subject or a specific subject if running in parallel
for (useSub in subj) {
  print(save_output_path())
  
  # So I can see the data set 
  print(paste0("Data set id: ",dataset_id))
  
  # So I can see which model is running
  print(paste0("Model: ", model))
  
  # so I can see what subject is running
  print(paste0("Participant: ", useSub))
  
  # check if the model is a blocked model
  #blocked_model <- grepl("blocked", model)
  
  # print to make sure blocked model has been updated properly
  #print(paste0("Blocked model: ", blocked_model))
  
  # load data
  load(here(load_data_path()))
  
  # The data generating script for the recovery has correct and incorrect switched around
  if(recovery){
    data$Resp = 3-data$Resp
  }
  
  # identify unique response stimuli in data (eg., left/right)
  stims <- sort(unique(data$Stim))
  if (length(stims) > 2 ) stop("More than two response stimuli detected in the data.")
  
  # generate and set seed
  newSeed = Sys.time()
  set.seed(as.numeric(newSeed))
  
  # source log likelihood functions
  source(here("modelling/likelihood-function.R"))
  
  # some models require the likelihood function to iterate over blocks
  if (blocked_likelihood){
    log.dens.like <- log.dens.like.blocked
  } else{
    log.dens.like <- log.dens.like.normal
  }
  
  # Extract the full name of the model
  full_name = paract_functions$full_name
  
  # list parameter names so we knows what to call from priors scripts
  theta.z = get_function_variables(paract_functions$z) # function that extracts the parameters from the time-varying (or standard DDM) functions.
  theta.t = get_function_variables(paract_functions$t0)
  theta.v = get_function_variables(paract_functions$v)
  theta.a = get_function_variables(paract_functions$a)
  # to extract the parameter names for the complex block function is a little different
  getComplexBlockParams = function(parameter, blocks){
    # name block parameters
    block.theta.names = NULL
    for (block in blocks) {
      block.theta.names[block] =  paste(parameter,block,sep=".")
    }
    block.theta.names
  }

  # if a model is the complex block model, the parameter should come up as, e.g., "a."
  if("a." %in% theta.a){
    # add to theta names
    theta.a = getComplexBlockParams("a", blocks = unique(data$Block))
  }
  if ("v." %in% theta.v){
    theta.v = getComplexBlockParams("v", blocks = unique(data$Block))
  }
  
  theta.names = c(theta.z, theta.a, theta.v, theta.t)
  print(cat("Parameters to be estimated: ", theta.names))
  # define file paths for output
  savefile = here(
      save_output_path()
  )

  
  # source priors
  source(here("modelling/priors.R"))
  
  print("Starting sampling")
  
  # run MCMC process
  source(here("modelling/iterative-process.R"))
  
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
       blocked_likelihood,
       paract_functions,
       analysis_round,
       dataset_details,
       model,
       file = savefile)
  
  
  # full output file is quite big, so sometimes you might want to save model comparison data separately
  if(save_IC_path != FALSE){
    
  saveIC = here(
    paste(
      save_IC_path,
      "P",
      useSub,
      "_",
      model,
      "-IC.Rdata",
      sep = ""
    )
  )
  
  save(AIC, BIC, file = saveIC)
  }
  
  if (plot) {
    # source parameter plotting script
    source(here("modelling/plot-parameters.R"))
    
  png(filename = paste0(plot_path(), "P", subj, "-",model,"-parameter-plot.png"), width = 1000, height = 500)
    # define parameters of interest
    parameters_of_interest <- c("a", "v")
    
    # Set up the plotting layout
    par(mfrow = c(1, length(parameters_of_interest)), cex = 1.3)

    for (par in parameters_of_interest){
      # plot parameters across time for parameters of interest
      plotParamsIndividual(
        parameter = par,
        functions = paract_functions,
        theta = theta,
        subject = subj,
        blocked_likelihood = blocked_likelihood,
        data = data
      )
      if (par == parameters_of_interest[1]){
        mtext(full_name, line = 2, cex = 2, adj = 0)
      }
    }
  
  dev.off()
  print(paste0("Plot saved for model", model))
  }
  
  if(simulate_fits){
    # simulate predictions from estimated parameters (for model fits)
    source(here("modelling/simulate-paract-DDM.R"))
  }
  
}



