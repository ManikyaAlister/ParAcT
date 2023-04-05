rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-18/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-18/02_deep-background.R"))

conds=1 # number of conditions to loop over
model = "v-linear"
nSub = 9 # number of subjects to run 

####################
#### Linear Model###
####################


for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  load(here(paste("data/evansetal-18/clean/P",useSub,".Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a"] # bx + c = linear function
      t0=x["t0"]
      v=(x["v.b"]*data$Trial)+x["v.c"] 
      z=0.5
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names=c("a","t0",
                "v.b","v.c")
  
  savefile=here(paste("modelling/evansetal-18/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/evansetal-18/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/evansetal-18/03_priors/03.2.2_v-priors-linear.R"))
  source(here("modelling/evansetal-18/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  
  save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}