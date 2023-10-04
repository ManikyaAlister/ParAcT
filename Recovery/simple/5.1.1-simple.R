rm(list=ls())
#renv::deactivate() 
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
#renv::install(c("msm"))
#library(here)
#library(rtdists)
#library(msm)
source(file = here("modelling/knowlesetal-19/round-1/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/knowlesetal-19/round-1/02_deep-background.R"))

conds=1 # number of conditions to loop over
model = "simple"
print(model)
subj = commandArgs(trailingOnly = TRUE)
print(subj)
nSub = 100 # number of subjects to run (if looping instead of parallel)

####################
#### Simple Model###
####################


for (useSub in 1) {
  # Run DDM for each subject in n Subjects
  
  load(here(
    paste("Recovery/simple/Datasets/RECOVERY_DATA-DIFF_LHS-",useSub, ".Rdata", sep = "")
  ))
  newSeed = Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x, data, par.names) {
    out = 0
    names(x) = par.names
    
    for (cond in conds) {
      a = x["a"]
      t0 = x["t0"]
      v = x["v"]
      z = x["z"]
      sv = 0
      sz = 0
      st0 = 0
      s = 1
      tmp = ddiffusion(
        rt = data$time[data$Cond == cond],
        response = data$Resp[data$Cond == cond],
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
  
  theta.names = c("z", "a", "t0",
                  "v")
  
  savefile=here(paste("Recovery/simple/Fits_recovery/P",useSub,"_",model,".Rdata",sep=""))
  
  source(here("modelling/knowlesetal-19/round-1/03_priors.R"))
  source(here("modelling/knowlesetal-19/round-1/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2 * max(weight) + 2 * n.pars
  BIC = log(length(data$time)) * n.pars - 2 * max(weight)
  
  save(AIC,
       BIC,
       theta,
       weight,
       data,
       burnin,
       nmc,
       n.chains,
       theta.names,
       conds,
       file = savefile)
}


