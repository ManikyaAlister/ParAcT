# Simulate paract-DDM using estimated parameters -------------------------------

# This should be sourced from inside the fitting script.

print("Simulating model from estimated parameters")

# Sets up a list with the correct headings in preparation for the simulation
simdata = list(Time = NULL,
               Stim = NULL,
               Resp = NULL,
               Trial = NULL,
               Block = NULL
               )

# find the estimated parameters
tmp1 = apply(weight, 2, max)
tmp2 = which.max(tmp1)
tmp3 = which.max(weight[, tmp2])

best_values = theta[tmp2, , tmp3]
x = best_values
for (stim in stims) {
  if (blocked_likelihood) {
    blocks = unique(data$Block)
    for (block in blocks) {
      
      data_stim <-
        data[data$Stim == stim & data$Block == block,]
      
      # function to get the arguments of another function
      funArgs = function(fun) {
        names(formals(fun))
      }
      
      # get estimates
      
      # check to see if there is a "block" (b) argument in the function, and if there is, add it.
      if ("b" %in% funArgs(paract_functions$a)) {
        a = paract_functions$a(x, data = data_stim, b = block)
      } else {
        a = paract_functions$a(x, data = data_stim)
      }
      
      if ("b" %in% funArgs(paract_functions$v)) {
        v = paract_functions$v(x, data = data_stim, b = block)
      } else {
        v = paract_functions$v(x, data = data_stim)
      }
      
      if ("b" %in% funArgs(paract_functions$t0)) {
        t0 = paract_functions$t0(x, data = data_stim, b = block)
      } else {
        t0 = paract_functions$t0(x, data = data_stim)
      }
      
      if ("b" %in% funArgs(paract_functions$z)) {
        z = paract_functions$z(x,
                        data = data_stim,
                        stimulus = stim,
                        b = block)
      } else {
        z = paract_functions$z(x, data = data_stim, stimulus = stim)
      }
      
      tmp = rdiffusion(
        # n is the number of trials in a given loop cell
        n = length(data$Time[data$Stim == stim & data$Block == block]),
        a = a,
        v = v,
        t0 = t0,
        z = z * a
      )
      
      # get the actual trials that this corresponds to 
      
      
      simdata$Time = c(simdata$Time, tmp$rt) # Populates the RT column in the simulated data
      simdata$Resp = c(simdata$Resp, tmp$response) # Populates the Resp column in the simulated data
      simdata$Stim = c(simdata$Stim, rep(stim, length(tmp$rt))) 
      simdata$Trial = c(simdata$Trial,data$Trial[data$Stim == stim & data$Block == block])
      simdata$Block = c(simdata$Block, data$Block[data$Stim == stim & data$Block == block])
    }
  } else {
    data_stim <-
      data[data$Stim == stim,]
    
    # Runs diffusion model to generate data with estimated parameters
    tmp = rdiffusion(
      n = length(data$Time[data$Stim == stim]),
      a = paract_functions$a(x, data = data_stim),
      v = paract_functions$v(x, data = data_stim),
      t0 = paract_functions$t0(x, data = data_stim),
      z = paract_functions$z(x, stimulus = stim, data = data_stim) * paract_functions$a(x, data = data_stim)
    )
    simdata$Time = c(simdata$Time, tmp$rt) # Populates the RT column in the simulated data
    simdata$Resp = c(simdata$Resp, tmp$response) # Populates the Resp column in the simulated data
    simdata$Stim = c(simdata$Stim, rep(stim, length(tmp$rt)))
    simdata$Trial = c(simdata$Trial,data$Trial[data$Stim == stim])
    simdata$Block = c(simdata$Block, data$Block[data$Stim == stim])
  }
}
sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format

# save
save(sim, file = here(
  paste0(
save_output_path(fit = FALSE)
  )))