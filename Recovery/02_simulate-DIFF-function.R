
dyn.load("Recovery/DIFF_trialVarying.so")

use.interval=0.0001
use.table=qnorm(seq(use.interval,1-use.interval,use.interval))
n.table.options=length(use.table)


simulate.DIFF=function(time,params,maxCounter,stepSize,use.table,n.table.options, parameter_functions = paract_functions, stimulus = stim, all_stimuli = stims) {
  
  N = length(time)
  
  rts=rep(0,N)
  resps=rep(0,N)
  
  # the time variable needs to be a data frame for the paract functions
  df_time = data.frame(Trial = time)
  
  v = parameter_functions$v(params, data = df_time)
  # if the parameter is not time varying (which will output as length == 1), you still need it to be length(trials)
  if (length(v) == 1)
    v = rep(v, N)
  
  a = parameter_functions$a(params, data = df_time)
  if (length(a) == 1)
    a = rep(a, N)
  
  t0 = parameter_functions$t0(params, data = df_time)
  if (length(t0) == 1)
    t0 = rep(t0, N)
  
  z = parameter_functions$z(params, data = df_time, stimulus = stimulus, all_stimuli = all_stimuli)
  if (length(z) == 1)
    z = rep(z, N)
  
  useZ=z*a
  
  aU=a
  aL=rep(0,N)

    tmp=.C("DIFF",z=useZ,v=v,
         aU=aU,aL=aL,ter=t0,
         sv=params["sv"],sz=params["sz"],ster=params["ster"],
         s=params["stoch.s"],h=stepSize,resp=resps,rt=rts,
         n=as.numeric(N),maxiter=as.double(round(maxCounter)),
         rangeLow=as.integer(0),rangeHigh=as.integer(n.table.options-1),
         randomTable=as.double(use.table)) 
  
  out=list(rt=tmp$rt,resp=tmp$resp, trial = time)
}


