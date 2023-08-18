rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("Recovery/5.0.0_load-packages.R"))
source(file = here("Recovery/02_deep-background.R"))

conds= 1 # number of experimental conditions to loop over
model = "simple" 
nSub = 100 # number of subjects to run 
subj = commandArgs(trailingOnly = TRUE)
generating_data = "v-power-generated"

####################################
#### Exponential Threshold Model ###
####################################

for (useSub in 1:nSub) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  load(paste("Recovery/v-power/Datasets/RECOVERY_DATA-DIFF_LHS-",useSub,".Rdata",sep=""))
  #data =  lapply(data, head, n = 10)

  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x["v"]
      z=x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$time[data$Cond==cond],response=(3-data$Resp[data$Cond==cond]),z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0) #if I want to do it over multiple conditions
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names = c("z", "a","t0","z",
                "v")
  
  savefile=here(paste("Recovery/model-recovery/",generating_data,"/fits/P",useSub,"_",model,".Rdata",sep=""))

  source(here("modelling/evansetal-17/optim/round-1/03_priors.R"))
  source(here("Recovery/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$time))*n.pars-2*max(weight)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
