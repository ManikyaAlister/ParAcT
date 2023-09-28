rm(list=ls())

#library(here)
#setwd("Recovery") 
library(msm)


source("Recovery/simple/02_simulate-DIFF.R")
library(lhs)


nParams = 4
model = "simple"

#When in the same working directory as the C code, this line will compile the code (first time usage, like installing a package)
#system("R CMD SHLIB Recovery/DIFF.c") # <--- Make sure to run this first before trying to source

# Set up hypercube

use.LHS=randomLHS(n=100, k=nParams) # = n params

colnames(use.LHS)=c("v", "t0","a",
                    "z")

use.range=array(NA,c(nParams,2)) #c(nparams, 2)

rownames(use.range)=c("v", "t0","a",
                    "z")
colnames(use.range)=c("Min","Max")



# Define ranges for parameters for hypercube sampling

use.range["v",]=c(0.1,4)
use.range["t0",]=c(0.1,0.6)
use.range["z",]=c(0.1,0.9)
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
  genParams=array(NA,c(8,length(conds)),dimnames=list(c("z","a","ter","v","stoch.s","sz","sv","ster"),conds))
  
  # Loop over conditions
  for (cond in conds) {
    
    
    # Define generating parameters for this condition
    genParams[,paste(cond)] = c(
                                as.numeric(use.LHS[i,"v"]),
                                as.numeric(use.LHS[i,"t0"]),
                                as.numeric(use.LHS[i,"a"]),
                                as.numeric(use.LHS[i,"z"]),
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
