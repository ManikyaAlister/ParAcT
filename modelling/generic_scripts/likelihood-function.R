# log likelihood function
log.dens.like.normal = function (x, data, par.names, functions = paract_functions) {
  out = 0
  names(x) = par.names
  
  for (stim in stims) {
    #if it's a blocked model, time = block, if trial model time = trial
    if (blocked_model){
      # filter trials for a given stimulus 
      stim_time <- data$Block[data$Stim == stim]
    } else{
      stim_time <- data$Trial[data$Stim == stim]
    }
    
    # get estimates 
    a = functions$a(x, time = stim_time)
    t0 = functions$t0(x, time = stim_time)
    v = functions$v(x, time = stim_time)
    z = functions$z(x, time = stim_time, stimulus = stim)
    sv = 0
    sz = 0
    st0 = 0
    s = 1
    tmp = ddiffusion(
      rt = data$Time[stim_time],
      response = data$Resp[stim_time],
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

# log likelihood function for models that require iteration over blocks
log.dens.like.blocked = function (x, data, par.names, functions = paract_functions) {
  out = 0
  names(x) = par.names
  blocks = unique(data$Block)
  for (stim in stims) {
    for (block in blocks){
      
    
    #if it's a blocked model, time = block, if trial model time = trial
    if (blocked_model){
      # filter trials for a given stimulus 
      stim_time <- data$Block[data$Stim == stim & data$Block == block]
    } else{
      stim_time <- data$Trial[data$Stim == stim & data$Block == block]
    }
      
    # function to get the arguments of another function
    funArgs = function(fun) {
      names(formals(fun))
    }

    # get estimates 
    
    # check to see if there is a "block" (b) argument in the function, and if there is, add it. 
    if ("b" %in% funArgs(functions$a)){
      a = functions$a(x, time = stim_time, b = block)
    } else {
      a = functions$a(x, time = stim_time)
    } 
    
    if ("b" %in% funArgs(functions$v)){
      v = functions$v(x, time = stim_time, b = block)
    } else {
      v = functions$v(x, time = stim_time)
    }
    
    if ("b" %in% funArgs(functions$t0)){
      t0 = functions$t0(x, time = stim_time, b = block)
    } else {
      t0 = functions$t0(x, time = stim_time)
    }
    
    if ("b" %in% funArgs(functions$z)){
      z = functions$z(x, time = stim_time, stimulus = stim, b = block)
    } else {
      z = functions$z(x, time = stim_time, stimulus = stim)
    }
    
    sv = 0
    sz = 0
    st0 = 0
    s = 1
    tmp = ddiffusion(
      rt = data$Time[stim_time],
      response = data$Resp[stim_time],
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
  }
  out
}