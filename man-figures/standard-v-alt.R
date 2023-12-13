library(here)
library(modelProb)
dataset <- 
  
load(here("data/evansetal-17/derived/normal/allAIC.Rdata"))
load(here("data/evansetal-17/derived/normal/allBIC.Rdata"))
load(here("data/evansetal-17/derived/normal/rank_BIC.Rdata"))
load(here("data/evansetal-17/derived/normal/rank_AIC.Rdata"))

# Best model versus simple model

# get IC for models



simple_AIC <- allAIC[,"simple"]
simple_BIC <- allBIC[,"simple"]
best_no_simple <- best_IC_from_models(models[models!="simple"])

# get weighted IC
weighted_simpleVbest_AIC <- weightedICs(cbind(simple_AIC, best_no_simple$AIC))
colnames(weighted_simpleVbest_AIC) = c("simple", "best alternative")
weighted_simpleVbest_BIC <- weightedICs(cbind(simple_BIC, best_no_simple$BIC))
colnames(weighted_simpleVbest_BIC) = c("simple", "best alternative")

# plot 
plotWeightedICs(weighted_simpleVbest_AIC, main = "AIC")
plotWeightedICs(weighted_simpleVbest_BIC, main = "BIC")


apply(weighted_simpleVbest_AIC,2, mean)
apply(weighted_simpleVbest_BIC,2, mean)