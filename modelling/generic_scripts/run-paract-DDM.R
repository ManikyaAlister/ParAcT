
# load data structure with data set details
load(here("data/dataset-details.Rdata"))

# get data set details (for file paths) for specific data set
dataset_details <-
  dataset_details[dataset_details$dataset_id == dataset_id, ]

# extract key info
dataset <- dataset_details$dataset
subvariant <- dataset_details$subvariant
parsed_data <- dataset_details$parsed_data_file

# if a DDM parameter function has more than one parameter it is time varying
time_varying <- unlist(lapply(paract_functions, function(x)
  length(x) > 1))

# check how many time varying parameters there are
n_time_varying <- sum(time_varying)

#  calculate  whether this is a round 1 model (only 1 time-varying parameter) or round 2 model
if (n_time_varying > 1) {
  round <- 2
} else {
  round <- 1
}

# source background code for MCMC 
source(file = here("modelling/generic_scripts/deep-background.R"))

# Run DDM for each subject or a specific subject if running in parallel
for (useSub in subj) {
  
  # So I can see the data set 
  print(dataset_id)
  
  # So I can see which model is running
  print(model)
  
  # so I can see what subject is running
  print(paste0("Participant ", useSub))
  
  # load data
  load(here(
    paste0(
      "data/",
      dataset,
      "/clean/P",
      useSub,
      "",
      parsed_data,
      ".Rdata",
      sep = ""
    )
  ))
  
  # generate and set seed
  newSeed = Sys.time()
  set.seed(as.numeric(newSeed))
  
  # log likelihood function
  log.dens.like = function (x, data, par.names, functions = paract_functions) {
    out = 0
    names(x) = par.names
    
    for (cond in conds) {
      a = functions$a(x)
      t0 = functions$t0(x)
      v = functions$v(x)
      z = functions$z(x)
      sv = 0
      sz = 0
      st0 = 0
      s = 1
      tmp = ddiffusion(
        rt = data$Time[data$Cond == cond],
        response = data$Resp[data$Cond == cond],
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
    paste(
      "modelling/",
      dataset,
      "",
      subvariant,
      "/round-",
      round,
      "/06_output/P",
      useSub,
      "_",
      model,
      ".Rdata",
      sep = ""
    )
  )
  
  saveIC = here(
    paste(
      "data/",
      dataset,
      "/derived",
      subvariant,
      "/P",
      useSub,
      "_",
      model,
      "-IC.Rdata",
      sep = ""
    )
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
  save(AIC, BIC, file = saveIC)
  save(AIC,
       BIC,
       theta,
       weight,
       data,
       burnin,
       nmc,
       n.chains,
       theta.names,
       conds,
       file = savefile)
}

