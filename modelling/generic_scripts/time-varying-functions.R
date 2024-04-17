
# Standard DDM parameters (that don't vary over time)---------------------------
# standard_DDM_parameter = function(x, parameter, time, recovery = FALSE){
#   param <- x[parameter]
#   # due to the way the recovery data is simulated, need to change this slightly.  
#   if (recovery){ 
#     param = rep(param, length(time))
#   }
#   param
# }

a_standard = function(x, time){
  param<- x["a"]
  param
}

v_standard = function(x, time){
  param <- x["v"]
  param
}

t0_standard = function(x, time){
  param <- x["t0"]
  param
}

# z has slightly different rules as it changes based on the response stimulus
z_standard = function(x, stimulus = stim, all_stimuli = stims, time) {
  if (stimulus == all_stimuli[1]) {
    z <- x["z"]
  } else if (stimulus == all_stimuli[2]) {
    z <- (1 - x["z"])
  } 
  z
}


# Trial-varying functions -------------------------------------------------

# linear
a_linear = function(x, time) {
  a <- ((-x["a.b"])*time)+x["a.c"]
  a
}

v_linear <-  function(x, time){
  v <- (x["v.b"]*time)+x["v.c"]
  v
}

# exponential
a_exp = function(x, time){
  a <- x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*time)
  a
}

v_exp = function(x, time){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*time)
  v
}

# delayed exponential
a_dExp = function(x, time){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*time))))
  a
}

v_dExp = function(x, time){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*time)))
  v
}

a_power = function(x, time){
  a <- x["a.asym"]+x["a.start"]*(time^-x["a.rate"])
  a
}

v_power = function(x, time){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*(time^-x["v.rate"])
  v
}

# # Block-varying functions -------------------------------------------------
# 
# constant block
# a_blocked_simple = function(x, b = block, time){
#   d <- (-x["step"] * (b-1) )
#   a <- x["a"]-d
#   a
# }
# 
# v_blocked_simple = function(x, b = block, time){
#   d <- -x["step"] * (block-1) # negative step because it needs to increase drift rate over blocks
#   a <- x["v"]-d
#   a
# }

# linear block (re-parameterisation of constant block)
a_linear_blocked = function(x, time) {
  a <- ((-x["a.b"])*time)+x["a.c"]
  a
}

v_linear_blocked = function(x, time){
  v <- (x["v.b"]*time)+x["v.c"]
  v
}

# exponential blocked
a_exp_blocked = function(x, time){
  a <- x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*time)
  a
}

v_exp = function(x, time){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*time)
  v
}

# delayed exp blocked
a_dExp_blocked = function(x, time){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*time))))
  a
}

v_dExp_blocked = function(x, time){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*time)))
  v
}

# step function (for optim data)
a_step_fixed = function(x, time, b){
  noFeedbackBlocks = c(1:4) # blocks where participants did not get detailed feedback in optim data
  a=ifelse(b %in% noFeedbackBlocks, x["initial"], x["initial"]-x["step"])
  a
}

v_step_fixed = function(x, time, b){
  noFeedbackBlocks = c(1:4)
  v=ifelse(b %in% noFeedbackBlocks, x["initial"], x["initial"]+x["step"])
  v
}

# block + exp trial 
a_block_trial_exp= function(x, time, b){
  b <- x["b.bump"]*(b-1)
  a <- x["a.asym"]+(b+x["a.start"])*exp(-x["a.rate"]*time)
  a
}

v_block_trial_exp = function(x, time, b){
  b <- x["b.bump"]*(b-1)
  v <- (x["v.asym"]+x["v.start"])-(b+x["v.start"])*exp(-x["v.rate"]*time)
  v
}

# complex (different estimate in every block)
a_blocked_complex = function(x, time, b){
  a = x[paste0("a.",b)]
  a
}

v_blocked_complex = function(x, time, b){
  v = x[paste0("v.",b)]
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


