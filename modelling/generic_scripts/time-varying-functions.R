
# Standard DDM parameters (that don't vary over time)---------------------------
# standard_DDM_parameter = function(x, parameter, time, recovery = FALSE){
#   param <- x[parameter]
#   # due to the way the recovery data is simulated, need to change this slightly.  
#   if (recovery){ 
#     param = rep(param, length(time))
#   }
#   param
# }

a_standard = function(x, data){
  param<- x["a"]
  param
}

v_standard = function(x, data){
  param <- x["v"]
  param
}

t0_standard = function(x, data){
  param <- x["t0"]
  param
}

# z has slightly different rules as it changes based on the response stimulus
z_standard = function(x, stimulus = stim, all_stimuli = stims, data) {
  if (stimulus == all_stimuli[1]) {
    z <- x["z"]
  } else if (stimulus == all_stimuli[2]) {
    z <- (1 - x["z"])
  } 
  z
}


# Trial-varying functions -------------------------------------------------

# linear
a_linear = function(x, data) {
  a <- ((-x["a.b"])*data$Trial)+x["a.c"]
  a
}

v_linear <-  function(x, data){
  v <- (x["v.b"]*data$Trial)+x["v.c"]
  v
}

# exponential
a_exp = function(x, data){
  a <- x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
  a
}

v_exp = function(x, data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial)
  v
}

# delayed exponential
a_dExp = function(x, data){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))))
  a
}

v_dExp = function(x, data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial)))
  v
}

a_power = function(x, data){
  a <- x["a.asym"]+x["a.start"]*(data$Trial^-x["a.rate"])
  a
}

v_power = function(x, data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*(data$Trial^-x["v.rate"])
  v
}

# # Block-varying functions -------------------------------------------------


# linear block (re-parameterisation of constant block)
a_linear_blocked = function(x, data) {
  a <- ((-x["a.b"])*data$Block)+x["a.c"]
  a
}

v_linear_blocked = function(x, data){
  v <- (x["v.b"]*data$Block)+x["v.c"]
  v
}

# exponential blocked
a_exp_blocked = function(x, data){
  a <- x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Block)
  a
}

v_exp_blocked = function(x, data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Block)
  v
}

# delayed exp blocked
a_dExp_blocked = function(x, data){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Block))))
  a
}

v_dExp_blocked = function(x, data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Block)))
  v
}

# step function (for optim data)
a_step_fixed = function(x, data, b){
  noFeedbackBlocks = c(1:4) # blocks where participants did not get detailed feedback in optim data
  a=ifelse(b %in% noFeedbackBlocks, x["initial"], x["initial"]-x["step"])
  a
}

v_step_fixed = function(x, data, b){
  noFeedbackBlocks = c(1:4)
  v=ifelse(b %in% noFeedbackBlocks, x["initial"], x["initial"]+x["step"])
  v
}

# block + exp trial 
a_block_trial_exp= function(x, data, b){
  b <- x["b.bump"]*(b-1)
  a <- x["a.asym"]+(b+x["a.start"])*exp(-x["a.rate"]*data$Trial)
  a
}

v_block_trial_exp = function(x, data, b){
  b <- x["b.bump"]*(b-1)
  v <- (x["v.asym"]+x["v.start"])-(b+x["v.start"])*exp(-x["v.rate"]*data$Trial)
  v
}

# complex (different estimate in every block)
a_blocked_complex = function(x, data, b){
  a = x[paste0("a.",b)]
  if (any(a) < 0) return(-Inf)
  a
}

v_blocked_complex = function(x, data, b){
  v = x[paste0("v.",b)]
  if (any(v) < 0) return(-Inf)
  v
}


# Get time-varying parameters from the functions -------------------------------

# in the old fitting script, you needed to define the parameter names manually, but they are already in the time-varying functions.,
# so we can save a lot of work if we just extract them from the functions.
get_function_variables <- function(func) {
  body_text <- capture.output(print(body(func)))  # Capture the body of the function as text
  body_text <- paste(body_text, collapse = "\n")  # Collapse into a single string
  # Use regex to find all matches of string literals surrounded by quotes, capturing only inside the quotes
  matches <- stringr::str_extract_all(body_text, '"([^"]*)"')
  parameters <- unique(unlist(matches))  # Unlist and get unique matches
  # Remove the surrounding quotes from each parameter
  parameters <- gsub('^"|"$', '', parameters)
  parameters
}

# test_fun = function(time){
#   time(x)
# }
# 
# test_fun(time = a_blocked_simple(x))


