rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-18/round-1/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-18/round-1/02_deep-background.R"))

conds=1 # number of experimental conditions to loop over
model = "v-a-exp-blocked" 
nSub = 1 # number of subjects to run 
block = # number of trials in a block

####################################
#### Exponential Threshold Model ###
####################################

for (useSub in 1:nSub) { # Run DDM for each subject in n Subjects
  
  load(here(paste("data/evansetal-18/clean/P",useSub,".Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
      t0=x["t0"]
      v=(x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial)
      z=0.5
      sv=0
      sz=0
      st0=0
      s=1
      #browser()
      tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0) #if I want to do it over multiple conditions
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names=c("a.asym","a.start","a.rate","t0",
                "v.asym","v.start","v.rate")
  
  savefile=here(paste("modelling/evansetal-18/round-1/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/evansetal-18/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/evansetal-18/round-1/03_priors/03.3.1_v-a-priors.R"))
  source(here("modelling/evansetal-18/round-1/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}
