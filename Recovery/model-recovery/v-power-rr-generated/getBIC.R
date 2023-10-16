# calculate BIC
model = "v-power-rr"
n = 100
for (useSub in 1:n) {
    load(here(
      paste(
        "Recovery/",
        model,
        "/Fits_recovery/P",
        useSub,
        "_",
        model,
        ".Rdata",
        sep = ""
      )
    ))
  n.pars = length(theta.names)
  BIC = log(length(data$time))*n.pars-2*max(weight)
  savefile=here(paste("Recovery/",model,"/Fits_recovery/P",useSub,"_",model,".Rdata",sep=""))
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds, genParams,
       file=savefile)
}



