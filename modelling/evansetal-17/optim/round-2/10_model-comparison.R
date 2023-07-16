rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)

n = 9
v_models <- c("simple", 
              "v-linear",
              #"v-power",
              "v-exp",
              #"v-delayed-pow",
              "v-delayed-exp",
              "v-blocked-simple",
              #"v-blocked-complex",  # only including complex blocked models as a sanity check, not in model compariso
              "v-blocked-exp-sb",
              #"v-blocked-exp-ul",
              "v-delayed-exp-blocked")

a_models <- c(#"simple",
              "a-linear",
              #"a-power",
              "a-exp",
              #"a-delayed-power",
              "a-delayed-exp",
              "a-blocked-simple",
              #"a-blocked-complex", # only including complex blocked models as a sanity check, not in model comparisons
              "a-blocked-exp-sb",
              #"a-blocked-exp-ul",
              "a-delayed-exp-blocked",
              "a-step-fixed")

models_2p = c(
  "v-a-exp",
  #"v-linear-a-blocked-complex",
  "v-linear-a-exp",
  "v-linear-a-dExp",
  "v-linear-a-blocked-simple",
  "v-dExp-blocked-a-exp",
  "v-dExp-a-dExp"
)

models <- c(a_models, v_models, models_2p)


IC_array = function(models, criterion) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  
  for (model in models) {
    for (i in 1:n) {
      print(model)
      load(here(
        paste(
          "data/evansetal-17/derived/optim/P",
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

save(allAIC, file = here("data/evansetal-17/derived/optim/allAIC.Rdata"))
save(allBIC, file = here("data/evansetal-17/derived/optim/allBIC.Rdata"))
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

# Get only models that were at least in the top 3 models
rankBIC_top <- unique(as.vector(rankBIC[,1:3]))

# filter the BIC array so that only these models are included
allBIC_top <- allBIC[,rankBIC_top]


BIC_weights_top <- modelProb::weightedICs(allBIC_top)
plotWeightedICs(BIC_weights_top, cex = 0.5)

BIC_weights <- modelProb::weightedICs(allBIC)
models = colnames(allAIC)

models_1p <- models[!(models %in% models_2p)]
modelProb::MMComparisonPlot(BIC_weights, models_1p, models_2p, groupNames = c("Single Parameter Models", "2 Parameter Models"))

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

exp_models <- models[grep("-exp", models)]
exp_models <- exp_models[!exp_models %in% c("a-blocked-exp-sb", "v-delayed-exp-blocked", "v-blocked-exp-sb", "a-delayed-exp-blocked", "v-delayed-exp")]
non_exp_models <- models[!models %in% exp_models]
modelProb::MMComparisonPlot(BIC_weights, exp_models, non_exp_models, groupNames = c("Exp Models", "Other Models"), main = "Exponential versus non-exponential models")

# which participants were best fit by exponential models? 
best_BICs = rankBIC[,1]
p_a_exp = grep("a-exp", best_BICs)
p_v_exp = c(grep("v-exp", best_BICs), best_BICs[best_BICs["v-a-exp"]])

