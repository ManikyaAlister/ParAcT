rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)

n = 9
models = c("simple","a-linear","a-power","a-exp","a-delayed-power","a-delayed-exp","v-linear","v-power","v-exp","v-delayed-pow","v-delayed-exp","v-a-exp")
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

apply(allBIC[,2:9],2,which.min)


rankBIC = aapply(allBIC[,2:length(allBIC)],1,which.min)

rankAIC = apply(allAIC[,2:length(allAIC)],1,which.min)
