

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

# power - exp comparison

recovering_model <- "v-exp"
generating_model <- "v-power-rr"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.asym")
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.asym")

# number of participants best fit to each model
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

# relative probability of each model collapsed across participants
weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC v-power-rr generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC v-power-rr generating data", seed = 9)
