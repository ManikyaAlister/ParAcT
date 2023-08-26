rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(file = here("modelling/knowlesetal-19/round-1/05_run-models/5.0.0_load-packages.R"))
source(file = here("modelling/knowlesetal-19/round-1/02_deep-background.R"))

blocks = 1:4 # blocks to loop over 
model = "v-blocked-exp-ul" 
print(model) # so I can see what model is running in the output
nSub = 7 # number of subjects to run (only used if looping instead of parallel)
subj = commandArgs(trailingOnly = TRUE) # If parallel, this will be the subject number taken from the sbatch or shell array
print(subj)

# number of trials in a block
####################################
#### Exponential Threshold Model ###
####################################

for (useSub in subj) { # Run DDM for each subject in nSubj, or a specific subject if running in parallel
  
  load(here(paste("data/knowlesetal-19/clean/P",useSub,".Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (block in blocks) {
      u = x["trialUnlearn"]*(block-1)
      a=x["a"]
      t0=x["t0"]
      v=(x["v.asym"]+x["v.start"])-(x["v.start"])*exp(-x["v.rate"]*(data$Trial-u))
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
  
  theta.names = c("z", "a","t0","trialUnlearn",
                "v.start","v.asym","v.rate")

  savefile=here(paste("modelling/knowlesetal-19/round-1/06_output/P",useSub,"_",model,".Rdata",sep=""))
  saveIC = here(paste("data/knowlesetal-19/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
  
  source(here("modelling/knowlesetal-19/round-1/03_priors.R"))
  source(here("modelling/knowlesetal-19/round-1/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  save(AIC,BIC,file = saveIC)
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}
