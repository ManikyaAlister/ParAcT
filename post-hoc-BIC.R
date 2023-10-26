library(here)
rm(list = ls())
model = "a-exp-rr"
bad_datasets = c(25, 60, 84)
for ( useSub in 1:100) {
  if(useSub %in% bad_datasets){
    next
  }
  load(here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep="")))
  n.pars = length(theta.names)
  BIC = log(length(data$time))*n.pars-2*max(weight)
  savefile=here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep=""))
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
