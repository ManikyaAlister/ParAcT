rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)


n = 100
generating_model <- "a-power-rr"
recovering_model <- "a-exp"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)



IC_array = function(models, criterion, generating) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    
    for (i in c(1:67, 69:n)) {
      if (!gen) {
        generating_data <- paste0(model, "-generated")
        load(here(
          paste(
            "Recovery/model-recovery/",
            generating_data,
            "/fits/P",
            i,
            "_",
            model,
            ".Rdata",
            sep = ""
          )
        ))
      } else {
        load(here(
          paste(
            "Recovery/",
            model,
            "/Fits_recovery/P",
            i,
            "_",
            model,
            ".Rdata",
            sep = ""
          )
        ))
      }
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

allAIC <- IC_array(models, "AIC", generating)
allBIC <- IC_array(models, "BIC", generating)

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC[-68,],2, mean)
apply(weightedBIC[-68,],2, mean)


-modelProb::plotWeightedICs(weightedAIC, main = "AIC a-power-rr generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC a-power-rr generating data", seed = 9)
