library(here)
rm(list = ls())
#model = "v-exp-a-step-fixed"
n = 147

v_models <- c(
  # "simple",
  "v-linear",
  # "v-power",
  "v-exp",
  # "v-delayed-pow",
  "v-delayed-exp",
  "v-blocked-simple",
  # "v-blocked-complex",  # only including complex blocked models as a sanity check, not in model compariso
  # "v-blocked-exp-ul",
  "v-delayed-exp-blocked",
  "v-blocked-exp-sb"
)

a_models <- c(  "a-linear",
                # "a-power",
                "a-exp",
                # "a-delayed-power",
                "a-delayed-exp",
                "a-blocked-simple",
                # "a-blocked-complex", # only including complex blocked models as a sanity check, not in model comparisons
                "a-delayed-exp-blocked",
                "a-blocked-exp-sb"
                # "a-blocked-exp-ul",
                #"a-step"
)

models_2p <- c(
  "v-a-exp",
  "v-linear-a-exp",
  "v-linear-a-blocked-simple",
  "v-dExp-a-Exp",
  "v-exp-a-step-fixed",
  "v-exp-a-dExp-blocked",
  "v-linear-a-dExp",
  "v-dExp-blocked-a-blocked-simple",
  "v-dExp-blocked+a-dExp" 
)

#models <- c("simple", a_models, v_models, models_2p)
models <- "v-linear-a-blocked-simple"

for ( model in models){
  for ( useSub in 1:n) {
    # if(useSub %in% bad_datasets){
    #   next
    # }
    AIC = NULL 
    BIC = NULL 
    if (model %in% models_2p){
      load(here(paste("modelling/knowlesetal-19/round-2/06_output/P",useSub,"_",model,".Rdata",sep="")))
    } else {
      load(here(paste("modelling/knowlesetal-19/round-1/06_output/P",useSub,"_",model,".Rdata",sep="")))
    }
    n.pars = length(theta.names)
    if (is.null(BIC) | BIC == -Inf){
      BIC = log(length(data$Resp))*n.pars-2*max(weight)
    }
    
    if (is.null(AIC)){
      AIC = -2*max(weight)+ 2*n.pars
      
    }
    savefile=here(paste("modelling/knowlesetal-19/round-2/06_output/P",useSub,"_",model,".Rdata",sep=""))
    save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
         file=savefile)
    saveIC = here(paste("data/knowlesetal-19/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
    save(AIC,BIC,file = saveIC)
  }
}
  
#   for ( useSub in 1:n) {
#     load(here(paste("modelling/knowlesetal-19/round-2/06_output/P",useSub,"_",model,".Rdata",sep="")))
#     saveIC = here(paste("data/knowlesetal-19/derived/P",useSub,"_",model,"-IC.Rdata",sep=""))
#     save(AIC,BIC,file = saveIC)
#   }
#   
# }
