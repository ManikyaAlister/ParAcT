rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/chenetal-18/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/chenetal-18/02_deep-background.R"))

conds=1 # number of experimental conditions to loop over
model = "v-a-exp-mir" 
nSub = 71 # number of subjects to run 

####################################
#### Exponential Threshold Model ###
####################################

for (useSub in 1:nSub) { # Run DDM for each subject in n Subjects
  
  load(here(paste("data/chenetal-18/clean/P",useSub,".Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=(x["a.asym"]+x["a.start"])-x["a.start"]*exp(x["a.rate"]*data$trial)
      t0=x["t0"]
      v=(x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$trial)
      z = x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0) #if I want to do it over multiple conditions
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names = c("z", "a.start","a.asym","a.rate","t0",
                "v.start","v.asym","v.rate")

  savefile=here(paste("modelling/chenetal-18/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/chenetal-18/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/chenetal-18/03_priors/03.3.1_v-a-priors.R"))
  source(here("modelling/chenetal-18/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  #save(AIC,BIC,file = saveIC)
  #save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
  #     file=savefile)
}
