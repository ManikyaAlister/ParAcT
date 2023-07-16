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

models_2p_optim = c(
  "v-a-exp",
  #"v-linear-a-blocked-complex",
  "v-linear-a-exp",
  "v-linear-a-dExp",
  "v-linear-a-blocked-simple",
  "v-dExp-blocked-a-exp",
  "v-dExp-a-dExp"
)

models_2p_normal = c(
  "v-a-exp",
  #"v-linear-a-blocked-complex",
  "v-linear-a-exp",
  "v-linear-a-blocked-simple",
  "v-a-linear"
  
)

models_optim <- c(a_models, v_models, models_2p_optim)
models_normal <- c(a_models[!a_models %in% "a-step-fixed"], v_models, models_2p_normal)



IC_array = function(models, criterion, condition) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  
  for (model in models) {
    for (i in 1:n) {
      print(model)
      load(here(
        paste(
          "data/evansetal-17/derived/",condition,"/P",
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

allBIC_optim <- IC_array(models_optim,"BIC", "optim")
n = 7
allBIC_normal <- IC_array(models_normal, "BIC", "normal")

allBIC <- rbind(allBIC_normal, allBIC_optim)

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
