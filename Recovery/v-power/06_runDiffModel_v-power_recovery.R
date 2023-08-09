rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("Recovery/5.0.0_load-packages.R"))
source(file = here("Recovery/02_deep-background.R"))

conds= 1 # number of experimental conditions to loop over
model = "v-power" 
nSub = 100 # number of subjects to run 
subj = commandArgs(trailingOnly = TRUE)

####################################
#### Exponential Threshold Model ###
####################################

for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  load(paste("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",useSub,".Rdata",sep=""))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=(x["v.asym"]+x["v.start"])-x["v.start"]*data$Trial^(-x["v.rate"])
      z = x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$time[data$Cond==cond],response=(3-data$Resp[data$Cond==cond]),z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0) #if I want to do it over multiple conditions
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names = c("z", "a","t0",
                "v.start","v.asym","v.rate")
  
  savefile=here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep=""))
  #saveIC = here(paste("data/evansetal-17/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/evansetal-17/optim/round-1/03_priors.R"))
  source(here("Recovery/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  #save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
