# Simulate paract-DDM using estimated parameters -------------------------------

# This should be sourced from inside the fitting script.

print("Simulating model from estimated parameters")

# Sets up a list with the correct headings in preparation for the simulation
simdata = list(Time = NULL,
               Stim = NULL,
               Resp = NULL)

# find the estimated parameters
tmp1 = apply(weight, 2, max)
tmp2 = which.max(tmp1)
tmp3 = which.max(weight[, tmp2])

best_values = theta[tmp2, , tmp3]
x = best_values

for (stim in stims) {
  # Runs diffusion model to generate data with estimated parameters
  tmp = rdiffusion(
    n = 10000,
    a = paract_functions$a(x),
    v = paract_functions$v(x),
    t0 = paract_functions$t0(x),
    z = paract_functions$z(x, stimulus = stim) * paract_functions$a(x)
  )
  simdata$Time = c(simdata$Time, tmp$rt) # Populates the RT column in the simulated data
  simdata$Resp = c(simdata$Resp, tmp$response) # Populates the Resp column in the simulated data
  simdata$Stim = c(simdata$Stim, rep(stim, length(tmp$rt)))
} 

sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format

# save
save(sim, file = here(
  paste0(
save_output_path(fit = FALSE)
  )))