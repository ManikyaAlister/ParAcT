

rm(list = ls())
lib <-
  .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)

n <- 147
v_models <- c(
  # "simple",
  "v-linear",
  # "v-power",
  "v-exp",
  # "v-delayed-pow",
  "v-delayed-exp",
  "v-blocked-simple",
  # "v-blocked-complex",  # only including complex blocked models as a sanity check, not in model compariso
  "v-blocked-exp-sb",
  # "v-blocked-exp-ul",
  "v-delayed-exp-blocked"
)

a_models <- c(
  "simple",
  "a-linear",
  # "a-power",
  "a-exp",
  # "a-delayed-power",
  "a-delayed-exp",
  "a-blocked-simple",
  # "a-blocked-complex", # only including complex blocked models as a sanity check, not in model comparisons
  "a-blocked-exp-sb",
  # "a-blocked-exp-ul",
  "a-delayed-exp-blocked",
  "a-step"
)

models_2p <- c(
  "v-a-exp",
  # "v-linear-a-blocked-complex",
  "v-linear-a-exp",
  "v-linear-a-blocked-simple",
  "v-a-linear",
  "v-dExp-a-dExp",
  "v-dExp-blocked-a-exp",
  "v-linear-a-dExp"
)

models <- c(a_models, v_models, models_2p)

IC_array <- function(models, criterion) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) <- c(models)
  
  for (model in models) {
    for (i in 1:n) {
      load(here(
        paste(
          "data/knowlesetal-19/derived/P",
          i,
          "_",
          model,
          "-IC.Rdata",
          sep = ""
        )
      ))
      if (criterion == "AIC") {
        IC <- AIC
      } else if (criterion == "BIC") {
        IC <- BIC
      }
      allIC[i, model] <- IC
    }
  }
  allIC
}

allAIC <- IC_array(models, "AIC")
allBIC <- IC_array(models, "BIC")

save(allAIC, file = here("data/knowlesetal-19/derived/allAIC.Rdata"))
save(allBIC, file = here("data/knowlesetal-19/derived/allBIC.Rdata"))
#
# allAIC_v <- IC_array(v_models, "AIC")
# allAIC_a <- IC_array(a_models, "AIC")
#
# allBIC_v <- IC_array(v_models,"BIC")
# allBIC_a <- IC_array(a_models, "BIC")

rank_models <- function(scores_array) {
  # apply the ranking function to each row of the array
  ranked_array <- t(apply(scores_array, 1, rank))
  
  # create a new array to store the model names
  model_names <-
    array(dim = dim(ranked_array),
          dimnames = dimnames(ranked_array))
  
  # loop over each row of the ranked array
  for (i in 1:nrow(ranked_array)) {
    # sort the row by the rank and get the corresponding column names
    model_names[i,] <-
      colnames(ranked_array)[order(ranked_array[i,])]
  }
  
  colnames(model_names) <- 1:length(scores_array[1,])
  return(model_names)
}

# make a function that gets the weightet IC from a vector models
weighted_IC_from_models <- function(models, IC_array) {
  # set up empty vector to fill with IC values
  ICs <- c()
  for (i in 1:length(IC_array[, 1])) {
    ICs[i] <- IC_array[i, models[i]]
  }
  ICs
}

# get a vector of just the single parameter models
models_1p <- models[!(models %in% models_2p)]

# make a function that returns the best IC for a given set of models and IC
best_IC_from_models = function(models, ICs = c("AIC", "BIC")) {
  # set up empt array to fill with best models and their ICs
  best_IC <- as.data.frame(matrix(ncol = length(ICs) * 2, nrow = n))
  colnames(best_IC) <- c(ICs, paste0("best_model_", ICs))
  
  for (IC in ICs) {
    IC_array_name <- paste0("all", IC)
    # turn string into object
    IC_array <- get(IC_array_name)
    IC_array <- IC_array[, models]
    # get column name for best model loop iteration
    best_model_colname <- paste0("best_model_", IC)
    #get the best model for each participant according to the IC
    best_models <- rank_models(IC_array)
    # add rthhe model names to the data frame
    best_IC[, best_model_colname] <- best_models[, 1]
    # get the actual  IC for the best model
    best_IC[, IC] <- weighted_IC_from_models(best_models, IC_array)
  }
  best_IC
}

# get IC weights
BIC_weights <- modelProb::weightedICs(allBIC)
AIC_weights <- modelProb::weightedICs(allAIC)

# Rank the best models for each participant
rankBIC <- rank_models(allBIC)
rankAIC <- rank_models(allAIC)

# Get the best model for each participant
best_BIC <- rankBIC[, 1]
save(best_BIC, file = here("data/knowlesetal-19/derived/best_BIC"))
best_AIC <- rankAIC[, 1]
save(best_AIC, file = here("data/knowlesetal-19/derived/best_AIC"))

# get the best single parameter models
best_IC_1p <- best_IC_from_models(models_1p)

# get the best 2 parameter models
best_IC_2p <- best_IC_from_models(models_2p)

# plot best 1p v best 2p
n_param_comparison_BIC <-
  cbind(best_IC_1p[, "BIC"], best_IC_2p[, "BIC"])
n_param_comparison_weights_BIC <-
  weightedICs(n_param_comparison_BIC)
colnames(n_param_comparison_weights_BIC) <-
  c("1 parameter models", "2 parameter models")
plotWeightedICs(n_param_comparison_weights_BIC, main = "BIC")

n_param_comparison_AIC <-
  cbind(best_IC_1p[, "AIC"], best_IC_2p[, "AIC"])
n_param_comparison_weights_AIC <-
  weightedICs(n_param_comparison_AIC)
colnames(n_param_comparison_weights_AIC) <-
  c("1 parameter models", "2 parameter models")
plotWeightedICs(n_param_comparison_weights_AIC, main = "AIC")

best_models <- rankBIC[, 1]


modelProb::MMComparisonPlot(
  BIC_weights,
  models_1p,
  models_2p,
  groupNames = c("Single Parameter Models", "2 Parameter Models"),
  main = "BIC"
)
modelProb::MMComparisonPlot(
  AIC_weights,
  models_1p,
  models_2p,
  groupNames = c("Single Parameter Models", "2 Parameter Models"),
  main = "AIC"
)

a_exp_models <- models[grep("a-exp", models)]
non_a_exp_models <- models[!(models %in% a_exp_models)]
modelProb::MMComparisonPlot(
  BIC_weights,
  a_exp_models,
  non_a_exp_models,
  groupNames = c("a-exp Models", "Other Models")
)

a_dExp_models <- models[grep("a-dExp", models)]
non_a_dExp_models <- models[!(models %in% a_dExp_models)]
modelProb::MMComparisonPlot(
  BIC_weights,
  a_dExp_models,
  non_a_dExp_models,
  groupNames = c("a-delayed exp Models", "Other Models")
)

v_exp_models <-
  models[c(grep("v-exp", models), grep("v-a-exp", models))]
non_v_exp_models <- models[!(models %in% v_exp_models)]
modelProb::MMComparisonPlot(
  BIC_weights,
  v_exp_models,
  non_v_exp_models,
  groupNames = c("v-exp Models", "Other Models")
)

v_linear_models <- models[c(grep("v-linear", models))]
non_v_linear_models <- models[!(models %in% v_linear_models)]
modelProb::MMComparisonPlot(
  BIC_weights,
  v_linear_models,
  non_v_linear_models,
  groupNames = c("v-linear Models", "Other Models")
)

v_dExp_models <- models[grep("v-dExp", models)]
non_v_dExp_models <- models[!(models %in% v_dExp_models)]
modelProb::MMComparisonPlot(
  BIC_weights,
  v_dExp_models,
  non_v_dExp_models,
  groupNames = c("v-delayed exp Models", "Other Models")
)


# v-linear versus v exp models
modelProb::MMComparisonPlot(
  BIC_weights,
  v_linear_models,
  v_exp_models,
  groupNames = c("v-linear Models", "v-exp Models")
)


# which participants were best fit by exponential models?
best_BICs <- rankBIC[, 1]
p_a_exp <- grep("a-exp", best_BICs)

# Best model vesus simple model
rank_no_simple <- rank_models(dplyr::select(allBIC,-simple))
best_models <- rank_no_simple[, 1]

best_BICs <- c()
for (i in 1:length(allBIC[, 1])) {
  best_BICs[i] <- allBIC[i, best_models[i]]
}

simple <- allBIC[, "simple"]
simpleVbest <- cbind(best_BICs, simple)
weighted_simpleVbest <- weightedICs(simpleVbest)
modelProb::MMComparisonPlot(
  weighted_simpleVbest,
  "simple",
  "best_BICs",
  groupNames = c("simple", "best alternative")
)

barplot(apply(BIC_weights, 2, mean), las=2)
