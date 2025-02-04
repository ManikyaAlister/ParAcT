getParameterRanges = function(model) {
  a.start <- if(model == "a-power") {
    c(2.5, 7)
  } else {
    c(1.5, 3)
  }
  
  a.rate <- if(model == "a-power") {
    c(0.001, 0.8)
  } else {
    c(0.001, 0.05)
  }
  
  a.asym <- if(model == "a-power") {
    c(0.5, 2)
  } else {
    c(0.45, 2)
  }
  
  
  v.start <- if(model == "v-power") {
    c(2.5, 5)
  } else {
    c(1.5, 3)
  }
  
  v.rate <- if(model == "v-power") {
    c(0.001, 0.8)
  } else {
    c(0.001, 0.05)
  }
  v.asym <- if(model == "v-power") {
    c(0.01, 1.5)
  } else {
    c(0.45, 2)
  }

  sv <- if(model == "full-ddm" | model == "v-var"){
    c(1.5,2.5)
  } else {
    c(0,0)
  }
  
  sz <- if(model == "full-ddm"| model == "z-var"){
    c(0.1,0.4)
  } else {
    c(0,0)
  }
  
  use.range <- rbind(
    # standard DDM parameters
    a = c(0.45, 1.75),
    v = c(0.1, 4),
    z = c(0.3, 0.7),
    t0 = c(0.1, 0.6),
    # linear change
    a.b = c(.0001, .003),
    v.b = c(.0001, .005),
    a.c = c(0.5, 5),
    v.c = c(0.001, 3),
    # exponential change
    a.start = a.start,
    # power and exp models need different parameter ranges
    v.start = v.start,
    a.rate = a.rate,
    v.rate = v.rate,
    a.asym = a.asym,
    v.asym = v.asym,
    # delayed change
    a.delay = c(0.1, 30),
    v.delay = c(0.1, 30),
    # between trial variability parameters
    stoch.s = c(1, 1),
    sz = sz,
    sv = sv,
    ster = c(0, 0)
  )
  use.range
}
