rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/evansetal-17/optim/round-2/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/evansetal-17/optim/round-2/02_deep-background.R"))

conds=1 # number of experimental conditions to loop over
model = "v-dExp-a-dExp" 
nSub = 9 # number of subjects to run 
subj = commandArgs(trailingOnly = TRUE)
print(model)

for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel

  load(here(paste("data/evansetal-17/clean/P",useSub,"-Optim-Trial.Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))))
      t0=x["t0"]
      v=(x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial)))
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
  
  theta.names = c("z", "a.start","a.asym","a.rate","a.delay","t0",
                "v.start","v.asym","v.rate","v.delay")

  savefile=here(paste("modelling/evansetal-17/optim/round-2/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/evansetal-17/derived/optim/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/evansetal-17/optim/round-2/03_priors.R"))
  source(here("modelling/evansetal-17/optim/round-2/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}
