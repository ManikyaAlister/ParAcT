rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("Recovery/5.0.0_load-packages.R"))
source(file = here("Recovery/02_deep-background.R"))

conds= 1 # number of experimental conditions to loop over
model = "complex-blocked" 
nSub = 100 # number of subjects to run 
subj = commandArgs(trailingOnly = TRUE)
generating_data = "v-exp-generated"
blocks = 1:24
####################################
#### Exponential Threshold Model ###
####################################

for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  load(paste("Recovery/v-exp/Datasets/RECOVERY_DATA-DIFF_LHS-",useSub,".Rdata",sep=""))
  data$Block = rep(1:24, each = 1000/24+1, length.out = 1000)
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (block in blocks) {
      a = x["a"]
      t0 = x["t0"]
      v = x[paste("v",block,sep=".")]
      z = x["z"]
      sv = 0
      sz = 0
      st0 = 0
      s = 1
      tmp = ddiffusion(
        rt = data$time[data$Block == block],
        response = (3-data$Resp[data$Block == block]),
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
  
  theta.names = c("z", "a", "t0")
  
  # name block parameters
  block.theta.names = NULL
  for (block in blocks) {
    block.theta.names[block] =  paste("v",block,sep=".")
  }
  
  # add to theta names
  theta.names = c(theta.names,block.theta.names)
  
  savefile=here(paste("Recovery/model-recovery/",generating_data,"/fits/P",useSub,"_",model,".Rdata",sep=""))

  source(here("modelling/evansetal-17/optim/round-1/03_priors.R"))
  source(here("Recovery/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$time))*n.pars-2*max(weight)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
