rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)

n = 9
models = c("simple",
"a-linear",
"a-power",
"a-exp",
"a-delayed-power",
"a-delayed-exp",
"a-blocked-simple",
"a-blocked-complex",
"a-blocked-exp-sb",
"a-blocked-exp-ul",
"v-linear",
"v-power",
"v-exp",
"v-delayed-pow",
"v-delayed-exp",
"blocked-simple",
"v-blocked-complex",
"v-blocked-exp-sb",
"v-blocked-exp-ul")

v_models <- c("simple", 
"v-linear",
"v-power",
"v-exp",
"v-delayed-pow",
"v-delayed-exp",
"blocked-simple",
"v-blocked-complex",
"v-blocked-exp-sb",
"v-blocked-exp-ul")

a_models <- c("simple",
"a-linear",
"a-power",
"a-exp",
"a-delayed-power",
"a-delayed-exp",
"a-blocked-simple",
"a-blocked-complex",
"a-blocked-exp-sb",
"a-blocked-exp-ul")

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
          "data/evansetal-18/derived/P",
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

save(allAIC, file = here("data/evansetal-18/derived/allAIC.Rdata"))
save(allBIC, file = here("data/evansetal-18/derived/allBIC.Rdata"))

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
best_a <- rankBIC_a[,1]

best <- cbind(best_a, best_v)

# Figure out all of the two parameter models to run for each participant, based on their best single parameterm models
models_2p_best <- array(dim = c(n, 1))

for (i in 1:length(best[,1])){
  models_2p_best
  models_2p_best[i] <- paste0(best[i,1],"+",best[i,2])
}

# what are the 2-parameter models that need to be made? 
unique_2p_best <- unique(models_2p_best)



## FIT PLOT

# DOTS IN GREEN OR RED FOR CORRECT OR INCORRECT 
# HOW TO CAPTURE UNCERTAINTY? 
# dots plotted as usual. Color based on correct or incorrect. If correct, get model predictions and error bar
# Number at the top saying % of times where model made correction prediction about accuracy. 

