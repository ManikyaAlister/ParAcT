rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)


n = 1
generating_data ="v-exp-generated"

models = c(
"v-power",
"v-exp")


IC_array = function(models, criterion) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  
  for (model in models) {
    for (i in 1:n) {
      load(here(
        paste(
          "Recovery/model-recovery/",generating_data,"/fits/P",i,"_",model,".Rdata",
          sep = ""
        )
      ))
      if (criterion == "AIC") {
        IC <- AIC
      } else if (criterion == "BIC") {
        IC <- BIC
      }
      allIC[i, model] = IC
      
    }
  }
  allIC
}

allAIC <- IC_array(models,"AIC")
allBIC <- IC_array(models,"BIC")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

modelProb::plotWeightedICs(weightedAIC)
modelProb::plotWeightedICs(weightedBIC)
