rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("Recovery/5.0.0_load-packages.R"))
source(file = here("Recovery/02_deep-background.R"))

conds= 1 # number of experimental conditions to loop over
model = "a-exp" 
nSub = 1 # number of subjects to run 
subj = commandArgs(trailingOnly = TRUE)
generating_data = "a-power-rr-generated"

####################################
#### Exponential Threshold Model ###
####################################

for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  # load data generated from the a exponential model
  load(paste("Recovery/a-power-rr/Datasets/RECOVERY_DATA-DIFF_LHS-",useSub,".Rdata",sep=""))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
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
  
  theta.names = c("v","t0","z",
                "a.start","a.asym","a.rate")
  
  savefile=here(paste("Recovery/model-recovery/",generating_data,"/fits/P",useSub,"_",model,".Rdata",sep=""))
  #saveIC = here(paste("data/evansetal-17/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  
  source(here("Recovery/03_priors/03.1.3_a-priors-pow-exp.R"))
  source(here("Recovery/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Trial))*n.pars-2*max(weight)
  #save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
