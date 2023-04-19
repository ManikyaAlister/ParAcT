## Load Packages ## 
.libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()
library(rtdists,lib.loc = lib)
library(msm, lib.loc = lib)
library(here, lib.loc = lib)

conds = 1
nSub = 9

##### Simulate data Using Parameters ####

for(useSub in 1:nSub) {


  load(here(paste("modelling/evansetal-17/normal/round-2/06_output/P",useSub,"_simple.Rdata", sep = ""))) #Loads through the datasets of each participant in nSub


  simdata=list(Time=NULL,Cond=NULL,Resp=NULL) #Sets up a list with the correct headings in preparation for the simulation

  tmp1=apply(weight,2,max)
  tmp2=which.max(tmp1)
  tmp3=which.max(weight[,tmp2])

  blah=theta[tmp2,,tmp3]

  for (cond in conds) { # Loops through each cue condition (congruent and incongruent)
    currParams=c(blah["a"],0.5,blah["v"],blah["t0"]) # Sets the value of parameters.
    names(currParams)=c("a","z","v","t0")  # Sets the names of the parameters


    tmp=rdiffusion(n=10000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"]) # Runs diffusion model to generated data with estimated parameters
    simdata$Time=c(simdata$Time,tmp$rt) # Populates the RT column in the simulated data
    simdata$Resp=c(simdata$Resp,tmp$response) # Populates the Resp column in the simulated data
    simdata$Cond=c(simdata$Cond,rep(cond,length(tmp$rt)))} # Populates the Cond column in the simulated data

  sim = as.data.frame(simdata) # Convert the simulated data from List format to data frame format

  save(sim, file = here(paste("modelling/evansetal-17/normal/round-2/08_model-predictions/P",useSub,"_simple.Rdata", sep = "")))

}



