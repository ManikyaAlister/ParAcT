library(here)
rm(list = ls())
model = "a-power"
for ( useSub in 69:100) {
  load(here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep="")))
  n.pars = length(theta.names)
  BIC = log(length(data$time))*n.pars-2*max(weight)
  savefile=here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep=""))
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
