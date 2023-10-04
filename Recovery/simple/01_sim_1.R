rm(list=ls())

#library(here)
#setwd("Recovery") 
library(msm)


source("Recovery/simple/02_simulate-DIFF.R")
library(lhs)


model = "simple"

#When in the same working directory as the C code, this line will compile the code (first time usage, like installing a package)
#system("R CMD SHLIB Recovery/DIFF.c") # <--- Make sure to run this first before trying to source

# Set up hypercube

parameters = c("v", "ter","a",
                "z")

nParams = length(parameters)

use.LHS=randomLHS(n=100, k=nParams) # = n params

colnames(use.LHS)=parameters

use.range=array(NA,c(nParams,2)) #c(nparams, 2)

rownames(use.range)=parameters
colnames(use.range)=c("Min","Max")



# Define ranges for parameters for hypercube sampling

use.range["v",]=c(0.1,4)
use.range["ter",]=c(0.1,0.6)
use.range["z",]=c(0.3,0.7)
use.range["a",]=c(0.45,1.75)

for (useParam in colnames(use.LHS)) {
  use.LHS[,useParam]=use.range[useParam,"Min"]+
    use.LHS[,useParam]*(use.range[useParam,"Max"]-use.range[useParam,"Min"])
}


conds=c(1)



# Simulate all data sets

for (i in 1:nrow(use.LHS)) {
  cat("\n",i,"/",nrow(use.LHS))
  
  # Set up simulated data storage
  data=list(time=NULL,Resp=NULL,Cond=NULL)
  
  
  # Set up generating parameters storage
  genParams=array(NA,c(nParams + 4,length(conds)),dimnames=list(c(parameters,"stoch.s","sz","sv","ster"),conds))
  
  # Loop over conditions
  for (cond in conds) {
    
    for (j in 1:nParams){
      genParams[j,paste(cond)] = as.numeric(use.LHS[i,j])
    }
      
    
    # Define generating parameters for this condition
    genParams[(nParams+1):length(genParams[,1]),paste(cond)] = c(
                                1,0,0,0)
    
    # Actually simulate
    tmp=simulate.DIFF(N=100,params=genParams[,paste(cond)],maxCounter=10000,stepSize=0.001,use.table=use.table,n.table.options=n.table.options)
    
    # Store sim data
    data$time=c(data$Time,tmp$rt)
    data$Resp=c(data$Resp,tmp$resp)
    data$Cond=c(data$Cond,rep(cond,length(tmp$rt)))
  }
  
  # Save sim data
  save(file=paste("Recovery/simple/Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata",sep=""),data,genParams,conds)
  
  
}
