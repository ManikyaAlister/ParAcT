

# Standard DDM parameters -------------------------------------------------

a_standard = function(x, d = data){
  a = x["a"]
  a
}

v_standard = function(x, d = data){
  v = x["v"]
  v
}

t_standard = function(x, d = data){
  t = x["t0"]
  t
}

z_standard = function(x, d = data){
  z = x["z"]
  z
}


# Trial-varying functions -------------------------------------------------

# linear
a_linear = function(x, d = data) {
  a <- ((-x["a.b"])*d$Trial)+x["a.c"]
  a
}

v_linear <-  function(x, d = data){
  v <- (x["v.b"]*d$Trial)+x["v.c"]
  v
}

# exponential
a_exp = function(x, d = data){
  a <- x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*d$Trial)
  a
}

v_exp = function(x, d = data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*d$Trial)
  v
}

# delayed exponential
a_dExp = function(x, d = data){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*d$Trial))))
  a
}

v_dExp = function(x, d = data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*d$Trial)))
  v
}


# Block-varying functions -------------------------------------------------

# constant block 
a_blocked_simple = function(x, b = block, d = data){
  d = (-x["step"] * (b-1) )
  a = x["a"]-d
  a
}

v_blocked_simple = function(x, b = block, d = data){
  d = -x["step"] * (block-1) # negative step because it needs to increase drift rate over blocks
  a = x["v"]-d
  a
}

# delayed exp blocked 
a_dExp = function(x, d = data){
  a <- x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*d$Block))))
  a
}

v_dExp = function(x, d = data){
  v <- (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*d$Block)))
  v
}

# block + exp trial 
a_blocked_sb_exp = function(x, d = data, b = block){
  b = x["b.bump"]*(block-1)
  a=x["a.asym"]+(b+x["a.start"])*exp(-x["a.rate"]*data$Trial)
  a
}

v_blocked_sb_exp = function(x, d = data, b = block){
  b = x["b.bump"]*(block-1)
  v=(x["v.asym"]+x["v.start"])-(b+x["v.start"])*exp(-x["v.rate"]*data$Trial)
  v
}

# Get time-varying parameters from the functions -------------------------------

# in the old fitting script, you needed to define the parameter names manually, but they are already in the time-varying functions.,
# so we can save a lot of work if we just extract them from the functions.
get_function_variables <- function(func) {
  body_text <- capture.output(print(body(func)))
  body_text <- paste(body_text, collapse = "\n")
  matches <- stringr::str_extract_all(body_text, '(?<=x\\[\\\")[^\\"]+(?=\\\"\\])')
  parameters <- matches[[1]]
  parameters
}

# test_fun = function(time){
#   time(x)
# }
# 
# test_fun(time = a_blocked_simple(x))

