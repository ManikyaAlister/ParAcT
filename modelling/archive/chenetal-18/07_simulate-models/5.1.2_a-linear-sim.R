rm(list=ls())
source(file = "modelling/evansetal-18/05_run-models/5.0.0_load-packages.R")
source(file = here("modelling/evansetal-18/02_deep-background.R"))

conds=1



nSub = 9

####################
#### Linear Model###
####################


for (useSub in 1:nSub) { # Run DDM for each subject in n Subjects
  
  load(here(paste("data/evansetal-18/clean/P",useSub,".Rdata",sep="")))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=(-x["a.b"]*data$Trial)+x["a.c"] # -bx + c = linear function
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
  
  theta.names = c("z", "a.c","a.b","t0",
                "v")
  
  savefile=here(paste("modelling/evansetal-18/06_output/P",useSub,"_a-linear.Rdata",sep=""))
  
  source(here("modelling/evansetal-18/03_background.R"))
  source(here("modelling/evansetal-18/04_iterative-process.R"))
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}

##### Simulate data Using Parameters ####

for(useSub in 1:nSub) {
  
  
  load(here(paste("modelling/evansetal-18/06_output/P",useSub,"_a-linear.Rdata", sep = ""))) #Loads through the datasets of each participant in nSub
  
  
  simdata=list(Time=NULL,Cond=NULL,Resp=NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  tmp1=apply(weight,2,max)
  tmp2=which.max(tmp1)
  tmp3=which.max(weight[,tmp2])
  
  blah=theta[tmp2,,tmp3]
  
  for (cond in conds) {
    currParams=c(blah["a"],0.5,blah["v"],blah["t0"]) # Sets the value of parameters. 
    names(currParams)=c("a","z","v","t0")  # Sets the names of the parameters
    
    
    tmp=rdiffusion(n=10000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"]) # Runs diffusion model to generated data with estimated parameters
    simdata$Time=c(simdata$Time,tmp$rt) # Populates the RT column in the simulated data
    simdata$Resp=c(simdata$Resp,tmp$response) # Populates the Resp column in the simulated data 
    simdata$Cond=c(simdata$Cond,rep(cond,length(tmp$rt)))} # Populates the Cond column in the simulated data
  
  sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format
  
  save(sim, file = here(paste("data/Model-Predictions/P",useSub,"_a-linear.Rdata", sep = "")))
  
}



