# log likelihood function
log.dens.like.normal = function (x, data, par.names, functions = paract_functions) {
  out = 0
  names(x) = par.names
  
  for (stim in stims) {
    data_stim = data[data$Stim == stim,]
    # get the trials that correspond to the stimulus in the loop
    stim_time = data$Stim == stim
    
    # figure out whether the model has across trial variability for v/z
    if("sz" %in% par.names) {
      sz = functions$sz(x,data = data_stim)
    } else {
      sz = 0
    }
    
    if("sv" %in% par.names) {
      sv = functions$sv(x,data = data_stim)
    } else {
      sv = 0
    }
    
    
    # get estimates 
    a = functions$a(x, data = data_stim)
    t0 = functions$t0(x, data = data_stim)
    v = functions$v(x, data = data_stim)
    z = functions$z(x, data = data_stim, stimulus = stim)
    sv = sv
    sz = sz
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
    data_stim = data[data$Stim == stim & data$Block == block,]
    stim_time = data$Stim == stim & data$Block == block
    
    # function to get the arguments of another function
    funArgs = function(fun) {
      names(formals(fun))
    }

    # get estimates 
    
    # check to see if there is a "block" (b) argument in the function, and if there is, add it. 
    if ("b" %in% funArgs(functions$a)){
      a = functions$a(x, data = data_stim, b = block)
    } else {
      a = functions$a(x, data = data_stim)
    } 
    
    if ("b" %in% funArgs(functions$v)){
      v = functions$v(x, data = data_stim, b = block)
    } else {
      v = functions$v(x, data = data_stim)
    }
    
    if ("b" %in% funArgs(functions$t0)){
      t0 = functions$t0(x, data = data_stim, b = block)
    } else {
      t0 = functions$t0(x, data = data_stim)
    }
    
    if ("b" %in% funArgs(functions$z)){
      z = functions$z(x, data = data_stim, stimulus = stim, b = block)
    } else {
      z = functions$z(x, data = data_stim, stimulus = stim)
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