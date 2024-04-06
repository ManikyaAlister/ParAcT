# set up large data frame with parameters and their ranges for all models we want to recover
use.range <- rbind(
  # standard DDM parameters
  a = c(0.45,1.75),
  v = c(0.1,4),
  z = c(0.3, 0.7),
  t0 = c(0.1, 0.6),
  # linear change
  a.b = c(.0001,.003),
  v.b = c(.0001,.005),
  a.c = c(0.5,5),
  v.c = c(0.001, 3),
  # exponential change
  a.start = c(1.5, 3), # should this be wider? 
  v.start = c(1.5, 3),
  a.rate = c(0.001,0.05),
  v.rate = c(0.001,0.05),
  a.asym = c(0.45,2),
  v.asym = c(0.45,2),
  # delayed change
  a.delay = c(0.1,30),
  v.delay = c(0.1,30),
  # between trial variability parameters
  stoch.s = c(1,1),
  sz = c(0,0),
  sv = c(0,0),
  ster = c(0,0)
)
