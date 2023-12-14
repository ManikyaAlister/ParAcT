# Some model scripts would not save AIC and BIC, but rather than running the models 
# again from scratch, we can calculate the AIC and BIC based on other savefd information. 

library(here)
rm(list = ls())

n_sub = 10

v_models <- c(
              #"v-linear",
              #"v-power",
             # "v-exp",
              #"v-delayed-pow",
              #"v-delayed-exp",
              #"blocked-simple",
              #"v-blocked-complex",  # only including complex blocked models as a sanity check, not in model compariso
              #"v-blocked-exp-sb",
              #"v-blocked-exp-ul",
              #"v-delayed-exp-blocked",
              "v-step-fixed")

a_models <- c(
              "a-linear",
              #"a-power",
              #"a-exp",
              #"a-delayed-power",
              "a-delayed-exp")
              #"a-blocked-simple",
              #"a-blocked-complex", # only including complex blocked models as a sanity check, not in model comparisons
              #"a-blocked-exp-sb",
              #"a-blocked-exp-ul",
              #"a-delayed-exp-blocked")
              #"a-step-fixed")

models <- c("simple", a_models, v_models)

# models_2p <- c(
#   "v-a-exp",
#   "v-linear-a-exp",
#   "v-linear-a-blocked-simple",
#   "v-dExp-a-Exp",
#   "v-exp-a-step-fixed",
#   "v-exp-a-dExp-blocked",
#   "v-linear-a-dExp",
#   "v-dExp-blocked-a-blocked-simple",
#   "v-dExp-blocked+a-dExp" 
# )
# 
# models_2p = c(
#   "v-a-exp",
#   #"v-linear-a-blocked-complex",
#   "v-linear-a-exp",
#   #"v-linear-a-dExp",
#   "v-linear-a-blocked-simple",
#   #"v-dExp-blocked-a-exp"
#   "v-dExp-blocked-a-Exp",
#   "v-exp-a-step-fixed"
# )

#models <- c("simple", a_models, v_models, models_2p)

for ( model in models){
  for ( subject in 1:n_sub) {
    # if(subject %in% bad_datasets){
    #   next
    # }
    AIC = NULL 
    BIC = NULL 
    if (model %in% models){
      load(here(paste("modelling/evansetal-17/optim/round-1/06_output/P",subject,"_",model,".Rdata",sep="")))
      savefile=here(paste("data/evansetal-17/derived/optim/P",subject,"_",model,"-IC.Rdata",sep=""))
    } else {
      load(here(paste("modelling/evansetal-17/optim/round-1/06_output/P",subject,"_",model,".Rdata",sep="")))
      savefile=here(paste("data/evansetal-17/derived/optim/P",subject,"_",model,"-IC.Rdata",sep=""))
      
    }
    print(savefile)
    if (!is.null(BIC)){
      #next
    }
    
    if (!is.null(AIC)){
      #next
    }
    
    
    # n.pars = length(theta.names)
    #   BIC = log(length(data$Resp))*n.pars-2*max(weight)
    #   AIC = -2*max(weight)+ 2*n.pars
    
    
    #save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
    #     file=savefile)
    #saveIC = here(paste("data/knowlesetal-19/derived/P",subject,"_",model,"-IC.Rdata",sep=""))
    save(AIC,BIC,file = savefile)
  }
}
  
#   for ( subject in 1:n) {
#     load(here(paste("modelling/knowlesetal-19/round-2/06_output/P",subject,"_",model,".Rdata",sep="")))
#     saveIC = here(paste("data/knowlesetal-19/derived/P",subject,"_",model,"-IC.Rdata",sep=""))
#     save(AIC,BIC,file = saveIC)
#   }
#   
# }
