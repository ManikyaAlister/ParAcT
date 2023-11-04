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

desired_order <- c("simple", models[grep("linear", models)], models[models == models[grep("exp", models)] & models != models[grep("delayed", models)]], models[grep("delayed", models)])

printLatexRow = function(meanBIC, meanAIC, desired_order){
  # Reorder the vector
  rv_AIC <- meanAIC[desired_order]
  rv_AIC[rv_AIC == 0] <- "$<$ .01"
  rv_BIC <- meanBIC[desired_order]
  rv_BIC[rv_BIC == 0] <- "$<$ .01"
  
  meanAIC = round(apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum)),2)
  meanBIC = round(apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum)),2)
  
  # Format for LaTeX table row
  latex_table_row <- sprintf("%s (%s) & %s (%s) & %s (%s) & %s (%s)", rv_BIC[1], rv_AIC[1], rv_BIC[2], rv_AIC[2],rv_BIC[3], rv_AIC[3],rv_BIC[4], rv_AIC[4])
  
  # Print the formatted row
  cat(latex_table_row)
}

printLatexRow(meanBIC,meanAIC, desired_order)


modelProb::plotWeightedICs(weightedBIC, main = "BIC a-linear generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC a-linear generating data", seed = 9)
