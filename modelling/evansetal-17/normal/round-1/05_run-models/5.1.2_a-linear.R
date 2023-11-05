rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/normal/round-1/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/normal/round-1/02_deep-background.R"))

conds=1 # number of conditions to loop over
model = "a-linear"
print(model)
subj = commandArgs(trailingOnly = TRUE)
# nSub = 1 # number of subjects to run 

####################
#### Linear Model###
####################


for (useSub in subj) { 
  
  load(here(paste("data/evansetal-17/clean/P",useSub,"-Norm-Trial.Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=((-x["a.b"])*data$Trial)+x["a.c"] # -bx + c = linear function
      # params should never be negative
      if (any(a < 0)) {
        return(-Inf)
      }
      t0=x["t0"]
      v=x["v"]
      z = x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names = c("z", "a.b","a.c","t0",
                "v")
  
  savefile=here(paste("modelling/evansetal-17/normal/round-1/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/evansetal-17/derived/normal/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/evansetal-17/normal/round-1/03_priors.R"))
  source(here("modelling/evansetal-17/normal/round-1/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  
  save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}