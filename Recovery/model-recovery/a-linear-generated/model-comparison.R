library(modelProb)
library(here)

IC_array = function(models, criterion, generating, grouping_param, bad_datasets = "") {
  # Set up an empty data frame with named columns for models
  allIC <- data.frame(matrix(ncol = length(models)))
  colnames(allIC) <- models
  gen_param <- numeric(n_sub)  # Initialize gen_param as a numeric vector
  
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    generating_data <- paste0(models[generating == TRUE], "-generated")
    
    for (i in 1:n_sub) {
      if (i %in% bad_datasets) {
        next
      }
      
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
  
  # Clean the data frame by removing rows with NAs
  allIC$gen_param <- gen_param
  allIC <- allIC[order(gen_param),]
  allIC <- allIC[complete.cases(allIC),]
  

  return(allIC)  # Return the cleaned data frame
}

#bad_datasets = c(16)

n_sub= 100
bad_datasets = c()

recovering_model <- c("a-exp", "a-delayed-exp", "simple")
generating_model <- "a-linear"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, FALSE, TRUE)

allAIC <- IC_array(models,"AIC", generating, grouping_param = "a.b", bad_datasets)
allBIC <- IC_array(models,"BIC", generating, grouping_param = "a.b", bad_datasets)

gen_param <- allBIC$gen_param

allAIC <- allAIC[,1:4]
allBIC <- allBIC[,1:4]

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC a-linear generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC a-linear generating data", seed = 9)
