library(here)
rm(list = ls())
model = "v-delayed-exp"
bad_datasets = c(44)
for ( useSub in 1:100) {
  if(useSub %in% bad_datasets){
    next
  }
  load(here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep="")))
  n.pars = length(theta.names)
  BIC = log(length(data$time))*n.pars-2*max(weight)
  #AIC = -2*max(weight)+ 2*n.pars 
  savefile=here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep=""))
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}
