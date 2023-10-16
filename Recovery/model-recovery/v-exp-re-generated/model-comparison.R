rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)


IC_array = function(models, criterion, generating, grouping_param) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  gen_param <- c()
  #generating_data <- paste0(models[generating == TRUE], "-generated")
  generating_data <- models[generating == TRUE]
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    for (i in 1:n) {
      if (!gen) {
        load(here(
          paste(
            "Recovery/model-recovery/",
            generating_data,
            "-generated/fits/P",
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

plotComparison = function(models, generating, criterion, grouping_param = "v.rate"){
  allIC <- IC_array(models,criterion, generating, grouping_param = grouping_param)
  weighted <- modelProb::weightedICs(allIC, bySubject = TRUE)
  colours <- c("red", "blue", "darkseagreen", "yellow")
  print(apply(weighted, 2, mean))
  modelProb::plotWeightedICs(weighted, main = paste0(criterion, " v-exp-re generating data"), colours = colours)
}

# power - exp comparison

recovering_model <- "v-power"
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)

plotComparison(models, generating, "AIC")
plotComparison(models, generating, "BIC")


# simple - linear - exp comparison

recovering_model <- c("simple", "v-linear")
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, TRUE)

plotComparison(models, generating, "AIC", grouping_param = "v.start")
plotComparison(models, generating, "BIC" , grouping_param = "v.start")

# simple - exp comparison

recovering_model <- "simple"
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, TRUE)


plotComparison(models, generating, "AIC", grouping_param = "v.start")
plotComparison(models, generating, "BIC", grouping_param = "v.start")

# simple + linear

recovering_model <- c("simple", "v-linear")
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, TRUE)

plotComparison(models, generating, "AIC")
plotComparison(models, generating, "BIC")

# simple + linear

recovering_model <- c("simple", "v-linear")
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, TRUE)

plotComparison(models, generating, "AIC")
plotComparison(models, generating, "BIC")

# all

recovering_model <- c("simple", "v-linear", "v-power")
generating_model <- "v-exp-re"

models <- c(recovering_model,
            generating_model)

generating <- c(FALSE, FALSE, FALSE, TRUE)

plotComparison(models, generating, "AIC")
plotComparison(models, generating, "BIC")

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

allBIC <- IC_array(models, criterion = "BIC", generating)
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



  