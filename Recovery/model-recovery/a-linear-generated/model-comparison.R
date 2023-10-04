library(modelProb)
library(here)

IC_array = function(models, criterion, generating, grouping_param) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  gen_param <- c()
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    generating_data <- paste0(models[generating == TRUE], "-generated")
    for (i in 1:n) {
      if (!gen) {
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
        
      gen_param[i] <- genParams[grouping_param, 1]
      
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
  
  
  #allIC <- cbind(allIC, gen_param)
  
  allIC <- allIC[order(gen_param),]
}


n = 100


recovering_model <- c("a-exp", "a-delayed-exp", "simple")
generating_model <- "a-linear"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "a.b")
allBIC <- IC_array(models,"BIC", generating, grouping_param = "a.b")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC a-exp generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC a-exp generating data", seed = 9)

# linear - exp comparison

recovering_model <- "a-linear"
generating_model <- "a-exp"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "a.rate")
allBIC <- IC_array(models,"BIC", generating, grouping_param = "a.rate")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))

modelProb::plotWeightedICs(weightedAIC, main = "AIC a-power generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC a-power generating data", seed = 9)
