rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)

n = 147
# round 1 models 

v_models <- c("simple", 
              "v-linear",
              "v-exp",
              "v-delayed-exp",
              "v-blocked-simple",
              #"v-blocked-complex",  # only including complex blocked models as a sanity check, not in model comparison
              "v-blocked-exp-sb",
              "v-delayed-exp-blocked")

a_models <- c(
              "a-linear",
              "a-exp",
              "a-delayed-exp",
              "a-blocked-simple",
              #"a-blocked-complex", # only including complex blocked models as a sanity check, not in model comparisons
              "a-blocked-exp-sb",
              #"a-delayed-exp-blocked",
              "a-step")

models <- c(a_models, v_models)

models_2p <- c(
  "v-a-exp", 
  "v-dExp-a-exp",
  "v-dExp-a-pow",
  "v-dPow-a-exp",
  "v-dPow-a-Pow"
)

IC_array = function(models, criterion) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  
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
      allIC[i, model] = IC
      
    }
  }
  allIC
}

allAIC <- IC_array(models,"AIC")
allBIC <- IC_array(models,"BIC")

#save(allAIC, file = here("data/knowlesetal-19/derived/allAIC.Rdata"))
#save(allBIC, file = here("data/knowlesetal-19/derived/allBIC.Rdata"))

allAIC_v <- IC_array(v_models, "AIC")
allAIC_a <- IC_array(a_models, "AIC")

allBIC_v <- IC_array(v_models,"BIC")
allBIC_a <- IC_array(a_models, "BIC")

rank_models <- function(scores_array) {
  # apply the ranking function to each row of the array
  ranked_array <- t(apply(scores_array, 1, rank))
  
  # create a new array to store the model names
  model_names <- array(dim = dim(ranked_array), dimnames = dimnames(ranked_array))
  
  # loop over each row of the ranked array
  for (i in 1:nrow(ranked_array)) {
    # sort the row by the rank and get the corresponding column names
    model_names[i,] <- colnames(ranked_array)[order(ranked_array[i,])]
  }
  
  colnames(model_names) = 1:length(scores_array[1,])
  return(model_names)
}

# Rank the best models for each participant
rankBIC <- rank_models(allBIC)
rankAIC <- rank_models(allAIC)

rankBIC_a <- rank_models(allBIC_a)
rankBIC_v <- rank_models(allBIC_v)

# Narrow that down to the best two models per participant
best2_v <- rankBIC_v[,1:2]
best2_a <- rankBIC_a[,1:2]

best2 <- cbind(best2_a, best2_v)

# Figure out all of the two parameter models to run based on the best 2
models_2p <- array(dim = c(n, 4))

for (i in 1:length(best2[,1])){
  model <- c()
  model[1] <- paste0(best2[i,1],"+",best2[i,3])
  model[2] <- paste0(best2[i,1],"+",best2[i,4])
  model[3] <- paste0(best2[i,2],"+",best2[i,4])
  model[4] <- paste0(best2[i,2],"+",best2[i,3])
  
  models_2p[i,] <- model
}

unique_2p <- unique(models_2p)

# Narrow that down to the best model because best two results in a lot of models
best_v <- rankBIC_v[,1]
save(best_v, file = here("data/knowlesetal-19/derived/best_v_1p_BIC"))
best_a <- rankBIC_a[,1]
save(best_a, file = here("data/knowlesetal-19/derived/best_a_1p_BIC"))


best <- cbind(best_a, best_v)

# Figure out all of the two parameter models to run for each participant, based on their best single parameterm models
models_2p_best <- array(dim = c(n, 1))

for (i in 1:length(best[,1])){
  models_2p_best
  models_2p_best[i] <- paste0(best[i,2],"+",best[i,1])
}

# what are the 2-parameter models that need to be made? 
unique_2p_best <- unique(models_2p_best)

BIC_weights <- modelProb::weightedICs(allBIC)
BICmeans <- apply(BIC_weights, 2, mean)
barplot(BICmeans)
AIC_weights <- modelProb::weightedICs(allAIC)
AICmeans <- apply(AIC_weights, 2, mean)
barplot(AICmeans)


modelProb::plotWeightedICs(weightedAICs)

# Best model vesus simple model
rank_no_simple <- rank_models(dplyr::select(allBIC, -simple) )
best_models <- rank_no_simple[,1]

best_BICs <- c()
for ( i in 1:length(allBIC[,1])){
  best_BICs[i] <- allBIC[i,best_models[i]]
}

simple = allBIC[,"simple"]
simpleVbest = cbind(best_BICs, simple)
weighted_simpleVbest <- weightedICs(simpleVbest)
modelProb::MMComparisonPlot(weighted_simpleVbest, "simple", "best_BICs", groupNames = c("simple", "best alternative"))

a_exp_models <- models[grep("a-exp", models)]
non_a_exp_models <- models[!(models %in% a_exp_models)]
modelProb::MMComparisonPlot(BIC_weights, a_exp_models, non_a_exp_models, groupNames = c("a-exp Models", "Other Models"))

a_dExp_models <- models[grep("a-dExp", models)]
non_a_dExp_models <- models[!(models %in% a_dExp_models)]
modelProb::MMComparisonPlot(BIC_weights, a_dExp_models, non_a_dExp_models, groupNames = c("a-delayed exp Models", "Other Models"))

v_exp_models <- models[c(grep("v-exp", models), grep("v-a-exp", models))]
non_v_exp_models <- models[!(models %in% v_exp_models)]
modelProb::MMComparisonPlot(BIC_weights, v_exp_models, non_v_exp_models, groupNames = c("v-exp Models", "Other Models"))

v_linear_models <- models[c(grep("v-linear", models))]
non_v_linear_models <- models[!(models %in% v_linear_models)]
modelProb::MMComparisonPlot(BIC_weights, v_linear_models, non_v_linear_models, groupNames = c("v-linear Models", "Other Models"))

v_dExp_models <- models[grep("v-dExp", models)]
non_v_dExp_models <- models[!(models %in% v_dExp_models)]
modelProb::MMComparisonPlot(BIC_weights, v_dExp_models, non_v_dExp_models, groupNames = c("v-delayed exp Models", "Other Models"))


## FIT PLOT

# DOTS IN GREEN OR RED FOR CORRECT OR INCORRECT 
# HOW TO CAPTURE UNCERTAINTY? 
# dots plotted as usual. Color based on correct or incorrect. If correct, get model predictions and error bar
# Number at the top saying % of times where model made correction prediction about accuracy. 


# Relative probability of single param modles to simple model
# 
# 
# # Get names of best model for each participant
# nSub = 7
# library(modelProb)
# best_mod_names <- rankBIC[, 1]
# BIC_best_mod <- NULL
# for (i in 1:nSub) {
#   BIC_best_mod[i] <- allBIC[i, best_mod_names[i]]
# }
# allBIC$simple
# 
# IC_array_MM <- cbind(BIC_best_mod, allBIC$simple)
# rownames(IC_array_MM) <- c("Best Model", "Standard DDM")
# 
# weights_simple_comp <- modelProb::weightedICs(IC_array_MM)
# colnames(weights_simple_comp) <- c("Best Model", "Standard DDM")
# 
# 
# MMComparisonPlot(
#   ICweights = weights_simple_comp,
#   models1 = "Best Model",
#   models2 = "Standard DDM",
#   main = "Best Model v Standard DDM"
# )
