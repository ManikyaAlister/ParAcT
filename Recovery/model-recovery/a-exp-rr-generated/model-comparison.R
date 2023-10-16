library(here)
IC_array = function(models, criterion, generating, grouping_param, bad_datasets = "") {
  # Set up an empty data frame with named columns for models
  allIC <- data.frame(matrix(ncol = length(models)))
  colnames(allIC) <- models
  gen_param <- numeric(n)  # Initialize gen_param as a numeric vector
  
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    generating_data <- paste0(models[generating == TRUE], "-generated")
    
    for (i in 1:n) {
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
  allIC <- allIC[order(gen_param),]
  allIC <- allIC[complete.cases(allIC),]
  
  
  return(allIC)  # Return the cleaned data frame
}


n = 100
bad_datasets = c(25,60,84) # there was am error generating these data sets
# power - exp comparison

recovering_model <- "a-power"
generating_model <- "a-exp-rr"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "a.asym", bad_datasets = bad_datasets)
allBIC <- IC_array(models,"BIC", generating, grouping_param = "a.asym", bad_datasets = bad_datasets)

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC a-exp-rr generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC a-exp-rr generating data", seed = 9)

# simple - linear - exp comparison

recovering_model <- c("simple", "a-linear", "a-delayed-exp")
generating_model <- "a-exp-rr"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "a.rate", bad_datasets)
allBIC <- IC_array(models,"BIC", generating, grouping_param = "a.rate", bad_datasets)

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))

modelProb::plotWeightedICs(weightedAIC, main = "AIC a-power generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC a-power generating data", seed = 9)
