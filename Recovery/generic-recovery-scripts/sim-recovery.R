rm(list = ls())
library(lhs)
library(here)

# model being recovered
model <-"simple"

# load time-varying functions
source(here("modelling/generic_scripts/time-varying-functions.R"))

# define paract functions (how/if any parameters vary across time)
paract_functions <- list(
  a = a_standard,
  v = v_standard,
  z = z_standard,
  t0 = t0_standard
)

vary_v = FALSE
vary_a = FALSE
vary_t0 = FALSE
vary_z = FALSE

vary_type = "exp" 


# define how many "participants" you want to simulate
n_participants <- 100

# list parameter names so we knows what to call from priors scripts
theta.z = get_function_variables(paract_functions$z) # function that extracts the parameters from the time-varying (or standard DDM) functions.
theta.t = get_function_variables(paract_functions$t0)
theta.v = get_function_variables(paract_functions$v)
theta.a = get_function_variables(paract_functions$a)

theta.names = c(theta.z, theta.a, theta.v, theta.t, "stoch.s","sz","sv","ster")

# source parameter ranges
source(here("Recovery/generic-recovery-scripts/parameter-ranges.R"))

# get the number of parameters
nParams <- length(theta.names)

# only use parameter ranges for parameters being recovered
use.range <- use.range[theta.names,]
colnames(use.range)=c("Min","Max") 


# set up response stimuli 
stims <- c(1,2)

# set up hyper cube
use.LHS <- randomLHS(n=n_participants, k=nParams)
colnames(use.LHS) <- theta.names

# initiate Latin hpercube sampling with defined ranges for each parameter
for (useParam in theta.names) {
  use.LHS[,useParam]=use.range[useParam,"Min"]+
    use.LHS[,useParam]*(use.range[useParam,"Max"]-use.range[useParam,"Min"])
}


stims=c(1,2)     ##### NJE: I changed this
conds = 1

source("Recovery/02_simulate-DIFF.R")

# Simulate all data sets with defined parameters

for (i in 1:nrow(use.LHS)) {
  cat("\n",i,"/",nrow(use.LHS))
  
  # Set up simulated data storage
  data=list(Time=NULL,Resp=NULL,Stim=NULL,Trial=NULL)   
  
  # set up empty genParams. Single column data frame as this means you can add other columns if simulating different conditions. 
  genParams=array(NA,c(nParams,conds),dimnames=list(theta.names,conds))
  
  # Set up generating parameters storage
  genParams[,1] = as.numeric(use.LHS[i,theta.names])
  
  # Loop over conditions     ##### NJE: I've changed this to now loop over stimuli, which kills two birds with one stone (fixes z issue, gets rid of redundant conditions bit)
  for (stim in stims) {
    # Define generating parameters for each stimulus 
    
    # MA: Z is conditionally updated from stim in simulate.DIFF function now
    
    n_trials=1000/length(stims) # generate a different data set for each stim then divide. 
    # Actually simulate
    tmp=simulate.DIFF(N=n_trials,params=genParams[,1],maxCounter=50000,stepSize=0.001,     ##### NJE: Changed "cond" to "stim" here
                      use.table=use.table,n.table.options=n.table.options)
    
    # Store sim data
    data$Time=c(data$Time,tmp$rt)
    data$Resp=c(data$Resp,tmp$resp)
    data$Stim=c(data$Stim,rep(stim,length(tmp$rt)))     ##### NJE: Changed "cond" to stim" here in several places
    data$Trial=c(data$Trial,1:n_trials)
  }
  
  # Save sim data
  #save(file=paste("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata",sep=""),data,genParams,stims)     ##### NJE: Changed "conds" to "stims" 
}