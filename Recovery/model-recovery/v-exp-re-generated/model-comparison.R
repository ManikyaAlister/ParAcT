library(here)
IC_array = function(models, criterion, generating, grouping_param, bad_datasets = "") {
  # Set up an empty data frame with named columns for models
  allIC <- data.frame(matrix(ncol = length(models)))
  colnames(allIC) <- models
  gen_param <- numeric(nSub)  # Initialize gen_param as a numeric vector
  generating_dataset <- paste0(models[generating == TRUE], "-generated")
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    
    for (i in 1:nSub) {
      if (i %in% bad_datasets) {
        next
      }
      
      if (!gen) {
        load(here(
          paste(
            "Recovery/model-recovery/",
            generating_dataset,
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


nSub = 100
bad_datasets = c() # there was am error generating these data sets
# power - exp comparison

recovering_model <- "v-power"
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.asym", bad_datasets = bad_datasets)
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.asym", bad_datasets = bad_datasets)


n_AIC <- table(apply(allAIC, 1, which.min))
n_BIC <-table(apply(allBIC, 1, which.min))

names(n_AIC) <- models
names(n_BIC) <- models

n_AIC
n_BIC

perc_AIC <- n_AIC/sum(n_AIC)
perc_BIC <- n_BIC/sum(n_BIC)

perc_AIC
perc_BIC

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp-re generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp-re generating data", seed = 9)

# simple - linear - exp comparison

recovering_model <- c("simple", "v-delayed-exp", "v-linear")
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.rate", bad_datasets)
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.rate", bad_datasets)

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))

get_n = function(allIC){
  n_IC <- table(apply(allIC, 1, which.min))
  if (length(n_IC) == length(colnames(allIC))){
    names(n_IC) = colnames(allIC)
  }
  n_IC
}

n_AIC <- get_n(allAIC) 
n_BIC <- get_n(allBIC)

n_AIC
n_BIC

modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp generating data", seed = 9)
