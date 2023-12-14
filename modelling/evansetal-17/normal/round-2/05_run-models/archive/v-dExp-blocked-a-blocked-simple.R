rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/normal/round-2/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/normal/round-2/02_deep-background.R"))

conds=1 # number of experimental conditions to loop over
model = "v-dExp-blocked-a-blocked-simple" 
nSub = 7 # number of subjects to run 
subj = commandArgs(trailingOnly = TRUE)
print(model)


for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  load(here(paste("data/evansetal-17/clean/P",useSub,"-Norm-Trial.Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    blocks <- unique(data$Block)
    for (block in blocks) {
      d = x["step"] * (block-1)
      a = x["a"]-d
      if (a < 0 ) {
        return(-Inf)
      }
      t0=x["t0"]
      v=(x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Block)))
      z = x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$Time[data$Block==block],response=data$Resp[data$Block==block],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0) #if I want to do it over multiple conditions
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }
  
  theta.names = c("z", "v.start","v.asym","v.rate","v.delay", "t0",
                  "a", "step")
  
  savefile=here(paste("modelling/evansetal-17/normal/round-2/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/evansetal-17/derived/normal/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/evansetal-17/normal/round-2/03_priors.R"))
  source(here("modelling/evansetal-17/normal/round-2/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
      file=savefile)
}
