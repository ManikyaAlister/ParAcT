rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)


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
  allIC
}


n = 100

# power - exp comparison

recovering_model <- "v-power"
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.start")
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.start")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))


modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp-re generating data", seed = 9)
modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp-re generating data", seed = 9)

# linear - exp comparison

recovering_model <- "v-linear"
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.start")
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.start")

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))

modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp-re generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp-re generating data", seed = 9)

# simple - exp comparison

recovering_model <- "simple"
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


allAIC <- IC_array(models,"AIC", generating, grouping_param = "v.start")
allBIC <- IC_array(models,"BIC", generating, grouping_param = "v.start")

apply(weightedAIC, 2, sum)/sum(apply(weightedAIC, 2, sum))
apply(weightedBIC, 2, sum)/sum(apply(weightedBIC, 2, sum))

weightedAIC <- modelProb::weightedICs(allAIC, bySubject = TRUE)
weightedBIC <- modelProb::weightedICs(allBIC, bySubject = TRUE)

modelProb::plotWeightedICs(weightedAIC, main = "AIC v-exp-re generating data", seed = 9)
modelProb::plotWeightedICs(weightedBIC, main = "BIC v-exp-re generating data", seed = 9)

# see if there are any gen param correlations

gen_param_array = function(model, generating) {
  # set up empty array
  gen <- NULL  
  est <- NULL
    for (i in 1:n) {
      if (!generating) {
        generating_data <- paste0(model, "-generated")
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
      gen <- rbind(gen, as.vector(genParams))
      
      est_params <- apply(theta,2, mean)
      est <- rbind(est, est_params)
    }
    colnames(gen) <- paste0("gen_", rownames(genParams))
    colnames(est) <- names(est_params)
    params <- cbind(gen, est)
    
    params
    
}

params <- gen_param_array(model = "v-exp-re", TRUE)
BIC_ordered <- allBIC[order(as.numeric(rownames(allBIC))),] 
BIC_ordered$best <- apply(BIC_ordered, 1, which.min)
params_BIC <- cbind(params, BIC_ordered)
rownames(params_BIC) <- NULL

params_simple <- params_BIC[params_BIC[,"best"]==1,]
params_exp <- params_BIC[params_BIC[,"best"]==2,]

cor(params_simple$gen_v.rate, params_simple$v.rate)
cor(params_exp$gen_v.rate, params_exp$v.rate)

c(range(params_exp$gen_v.rate), range(params_simple$gen_v.rate))

c(range(params_exp$v.rate), range(params_simple$v.rate))

barplot(c(mean(params_exp$gen_v.start), mean(params_simple$gen_v.start)))
barplot(c(mean(params_exp$v.start), mean(params_simple$v.start)))

c(range(params_exp$gen_v.start), range(params_simple$gen_v.start))

params_simple$v.rate-params_simple$gen_v.rate

params_simple$v.start-params_simple$gen_v.start

params_exp$v.rate-params_exp$gen_v.rate

plot(params_BIC$v.start, params_BIC$`v-exp-re`)
cor(params_BIC$v.start, params_BIC$`v-exp-re`)


plot(params_BIC$gen_v.start, params_BIC$`v-exp-re`)
cor(params_BIC$gen_v.start, params_BIC$`v-exp-re`)

plot(params_BIC$v.rate, params_BIC$`v-exp-re`)
plot(params_BIC$gen_v.rate, params_BIC$`v-exp-re`)



  