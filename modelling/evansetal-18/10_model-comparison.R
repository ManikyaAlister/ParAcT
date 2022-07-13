rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)

n = 9
models = c("simple","a-linear","a-power","a-exp","a-exp-mir","v-linear","v-power","v-exp")
allAIC = as.data.frame(matrix(ncol = 1+length(models)))
allBIC = as.data.frame(matrix(ncol = 1+length(models)))

colnames(allAIC) = c("Participant",models)
colnames(allBIC) = c("Participant",models)

for (model in models){
    for (i in 1:n){
      
load(here(paste("data/evansetal-18/derived/P",i,"_",model,"-IC.Rdata",sep = "")))
allAIC[i,"Participant"] = i
allAIC[i,model] = AIC

allBIC[i,model] = BIC
allBIC[i,"Participant"] = i
}
}
save(allAIC, file = here("data/evansetal-18/derived/allAIC.Rdata"))
save(allBIC, file = here("data/evansetal-18/derived/allBIC.Rdata"))
