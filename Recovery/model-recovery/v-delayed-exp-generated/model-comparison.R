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
  allIC$param = gen_param
  allIC <- allIC[order(gen_param),]
  allIC <- allIC[complete.cases(allIC),]

  
  return(allIC)  # Return the cleaned data frame
}


bad_datasets = c() # there was am error generating these data sets
# power - exp comparison

n = 100

# power - exp comparison

recovering_model <- c("simple", "v-linear", "v-exp")
generating_model <- "v-delayed-exp"

models <- c(recovering_model,
            generating_model)

generating <- c(rep(FALSE, length(recovering_model)), TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.delay", bad_datasets)
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.delay", bad_datasets)

gen_param <- allAIC$param

allAIC <- allAIC[,1:length(models)]
allBIC <- allBIC[,1:length(models)]


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

cor(weightedBIC[,"v-delayed-exp"], gen_param)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC a-exp generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC a-exp generating data", seed = 9)
