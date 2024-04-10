
# Standard DDM parameters (that don't vary over time)---------------------------
# standard_DDM_parameter = function(x, parameter, trials =  data$Trial, recovery = FALSE){
#   param <- x[parameter]
#   # due to the way the recovery data is simulated, need to change this slightly.  
#   if (recovery){ 
#     param = rep(param, length(trials))
#   }
#   param
# }

a_standard = function(x, trials =  data$Trial){
  param<- x["a"]
  param
}

v_standard = function(x, trials =  data$Trial){
  param <- x["v"]
  param
}

t0_standard = function(x, trials =  data$Trial){
  param <- x["t0"]
  param
}

# z has slightly different rules as it changes based on the response stimulus

z_standard = function(x, stimulus = stim, all_stimuli = stims, trials =  data$Trial) {
  if (stimulus == all_stimuli[1]) {
    z <- x["z"]
  } else if (stimulus == all_stimuli[2]) {
    z <- (1 - x["z"])
  } else {
    stop("WTF?!? Additional stim in stims")
  }
  z
}


# Trial-varying functions -------------------------------------------------

# linear
a_linear = function(x, trials =  data$Trial) {
  a <- ((-x["a.b"])*trials)+x["a.c"]
  a
}

v_linear <-  function(x, trials =  data$Trial){
  v <- (x["v.b"]*trials)+x["v.c"]
  v
}

# exponential
a_exp = function(x, trials =  data$Trial){
  a <- x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*trials)
  a
}

v_exp = function(x, trials =  data$Trial){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*trials)
  v
}

# delayed exponential
a_dExp = function(x, trials =  data$Trial){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*trials))))
  a
}

v_dExp = function(x, trials =  data$Trial){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*trials)))
  v
}

a_power = function(x, trials =  data$Trial){
  a <- x["a.asym"]+x["a.start"]*(trials^-x["a.rate"])
  a
}

v_power = function(x, trials =  data$Trial){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*(trials^-x["v.rate"])
  v
}

# Block-varying functions -------------------------------------------------

# constant block 
a_blocked_simple = function(x, b = block, trials =  data$Trial){
  d <- (-x["step"] * (b-1) )
  a <- x["a"]-d
  a
}

v_blocked_simple = function(x, b = block, trials =  data$Trial){
  d <- -x["step"] * (block-1) # negative step because it needs to increase drift rate over blocks
  a <- x["v"]-d
  a
}

# delayed exp blocked 
a_dExp_blocked = function(x, trials =  data$Trial, blocks = data$Block){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*blocks))))
  a
}

v_dExp_blocked = function(x, trials =  data$Trial, blocks = data$Block){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*blocks)))
  v
}

# block + exp trial 
a_blocked_sb_exp = function(x, trials =  data$Trial, b = block){
  b <- x["b.bump"]*(block-1)
  a <- x["a.asym"]+(b+x["a.start"])*exp(-x["a.rate"]*trials)
  a
}

v_blocked_sb_exp = function(x, trials =  data$Trial, b = block){
  b <- x["b.bump"]*(block-1)
  v <- (x["v.asym"]+x["v.start"])-(b+x["v.start"])*exp(-x["v.rate"]*trials)
  v
}

# Get time-varying parameters from the functions -------------------------------

# in the old fitting script, you needed to define the parameter names manually, but they are already in the time-varying functions.,
# so we can save a lot of work if we just extract them from the functions.
get_function_variables <- function(func) {
  body_text <- capture.output(print(body(func)))
  body_text <- paste(body_text, collapse = "\n")
  matches <- stringr::str_extract_all(body_text, '(?<=x\\[\\\")[^\\"]+(?=\\\"\\])')
  parameters <- unique(matches[[1]])
  parameters
}

# test_fun = function(time){
#   time(x)
# }
# 
# test_fun(time = a_blocked_simple(x))


