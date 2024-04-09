rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)


IC_array = function(estimating_models, criterion, generating_data, n_sub = 100, grouping_param) {
  n_models <- length(estimating_models)
  # Set up an empty data frame
  allIC <- matrix(NA, ncol = n_models, nrow = n_sub)  # Initialize with NAs
  colnames(allIC) <- estimating_models
  gen_param = c()
  for (j in 1:n_models) {
    model <- estimating_models[j]
    for (i in 1:n_sub) {
      # Reset IC for each iteration
      IC <- NA  # Use NA as a default value
      
      # Construct file path
      file_path <- here(paste0("Recovery/", generating_data, "/Fits_recovery/P", i, "-", model, ".Rdata"))
      load(paste0("Recovery/",generating_data,"/Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata"))
      
      # Check if the file exists before trying to load it
      if (file.exists(file_path)) {
        load(file_path)
        # Determine which criterion to use
        if (criterion == "AIC") {
          IC <- AIC
        } else if (criterion == "BIC") {
          IC <- BIC
        }
      }
      gen_param[i] <- genParams[grouping_param, 1]
      # Assign the IC value to the appropriate place in the matrix
      allIC[i, j] <- IC  # Use matrix indexing [row, column]
    }
  }
  
  print(gen_param)
  
  allIC <- allIC[order(gen_param),]
  
  # Convert the matrix to a data frame for the final output
  allIC_df <- as.data.frame(allIC)
  colnames(allIC_df) <- estimating_models
  return(allIC_df)
}

# define model groups 
a_models <- c("a-linear", "a-exp", "a-dExp")
v_models <- c("c-linear", "v-exp", "v-dExp")
a_exp_power <- c("a-exp", "a-power")
v_exp_power <- c("v-exp", "v-power")

# define details of each comparsion 
model_comparisons <- list(
  # can we distinguish between power and exponential changes? 
  list(data = "a-exp",
      models = a_exp_power),
  list(data = "a-power",
       models = a_exp_power),
  list(data = "v-exp",
      models = v_exp_power),
  list(data = "v-power",
       models = v_exp_power),
  # can we recover the simple model compared to all time-varying models? 
  list(data = "simple",
       models = c("simple", a_models, v_models)),
  # can we distinguish between a time-varying functions? 
  list(data = "a-linear",
       models = a_models),
  list(data = "a-exp",
        models = a_models),
  list(data = "a-dExp",
        models = a_models),
  # can we distinguish between v time-varying functions?
  list(data = "v-linear",
       models = v_models),
  list(data = "v-exp",
        models = v_models),
  list(data = "v-dExp",
        models = v_models),
  # can we distinguish between a and v exponential functions?
  list(data = "a-exp",
       models = c("a-exp", "v-exp")),
  list(data = "v-exp",
       models = c("a-exp", "v-exp"))
)




# Example usage
 IC_array(
  estimating_models = c("simple","a-linear", "a-exp", "a-dExp", "v-linear", "v-exp", "v-dExp"),
  criterion = "BIC",
  generating_data = "simple",
  grouping_param = "a"
)

 mc <- IC_array(
  estimating_models = c("simple","a-linear", "a-exp", "v-exp", "a-dExp"),
  criterion = "BIC",
  generating_data = "a-exp"
)

 mc <- IC_array(
  estimating_models = c("simple","v-linear", "v-exp", "v-dExp", "a-exp"),
  criterion = "BIC",
  generating_data = "v-exp",
  n_sub = 100,
  grouping_param = "v.asym"
  
)

 weights <- modelProb::weightedICs(mc)
 
 modelProb::plotWeightedICs(weights, colours = c("green", "red", "black", "blue", "purple"))
 


IC_array(
  estimating_models = c("a-linear", "a-exp","a-dExp", "v-linear", "v-exp", "v-dExp"),
  criterion = "BIC",
  generating_data = "simple"
)


n = 100

# power - exp comparison

recovering_model <- "v-power"
generating_model <- "v-exp"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <-
  IC_array(models, "AIC", generating, grouping_param = "v.rate")
allBIC <-
  IC_array(models, "BIC", generating, grouping_param = "v.rate")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum) / sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum) / sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp generating data", seed = 9)

# linear - exp comparison

recovering_model <- "v-linear"
generating_model <- "v-exp"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <-
  IC_array(models, "AIC", generating, grouping_param = "v.rate")
allBIC <-
  IC_array(models, "BIC", generating, grouping_param = "v.rate")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum) / sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum) / sum(apply(weightedBIC, 2, sum))

modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp generating data", seed = 9)

# simple - exp comparison

recovering_model <- "simple"
generating_model <- "v-exp"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <-
  IC_array(models, "AIC", generating, grouping_param = "v.start")
allBIC <-
  IC_array(models, "BIC", generating, grouping_param = "v.start")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum) / sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum) / sum(apply(weightedBIC, 2, sum))

modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp generating data", seed = 9)
