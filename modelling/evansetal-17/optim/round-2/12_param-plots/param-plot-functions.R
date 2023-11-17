#rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(ggplot2, lib.loc = lib)
library(dplyr)

# Make a function that gets the parameter changes across trials given the function
getThresholdChange = function(model, parameters, data) {
  x = parameters
  if (grepl("a-power", model)) {
    threshold = x["a.asym"] + x["a.start"] * data$Trial^(-x["a.rate"])
  } else if (grepl("a-exp|v-a-exp", model)) {
    threshold = x["a.asym"] + x["a.start"] * exp(-x["a.rate"] * data$Trial)
  } else if (grepl("a-linear", model)) {
    threshold = ((-x["a.b"]) * data$Trial) + x["a.c"]
  } else if (grepl("simple", model)) {
    threshold = rep(x["a"], length(data$Trial))
  } else if (grepl("a-delayed-power", model)) {
    threshold = x["a.asym"] + x["a.start"] * ((x["a.delay"] + 1) / (x["a.delay"] + data$Trial^(-x["a.rate"])))
  } else if (grepl("a-delayed-exp", model)) {
    threshold = x["a.asym"] + (x["a.start"] * ((x["a.delay"] + 1) / (x["a.delay"] + exp(x["a.rate"] * data$Trial))))
  } else if (grepl("a-exp-mir", model)) {
    threshold = (x["a.asym"] + x["a.start"]) - x["a.start"] * exp(x["a.rate"] * data$Trial)
  } else if (model == "a-step-fixed"){
    noFeedbackTrials = length(data$Trial[data$Block%in% c(1,2,3,4)])
    threshold = c(rep(x["initial"], noFeedbackTrials), rep(x["initial"] - x["step"], length(data$Trial)-noFeedbackTrials))
  }
  threshold
}

# Threshold

thresholdPlot = function(model,nRange, round = 1, complex = FALSE, mean = FALSE){
  
  df = as.data.frame(matrix(nrow = 0, ncol = 3))
  complexAll = NULL
  allParams = NULL
  for (i in nRange) {
    
    load(here(paste("modelling/evansetal-17/optim/round-",round,"/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    
    if(mean){
      params = apply(theta, 2, mean)
      allParams = rbind(allParams, params)
    } else {
      paramChange = getThresholdChange(model, x, data)
      paramChange= as.data.frame(paramChange)
      paramChange$Participant =  i
      paramChange$Trial = trials
      df = rbind(df, paramChange)
    }
    
    # load complex block model that estimates a different parameter each block 
    load(here(paste("modelling/evansetal-17/optim/round-1/06_output/P", i,"_a-blocked-complex.Rdata", sep = "")))
    complexParams = apply(theta, 2, mean)
    complexAll = rbind(complexParams, complexAll)
  }
  
  if (mean) {
    allParams = apply(allParams, 2, median)
    paramChange = getThresholdChange(model, allParams, data)
    df = as.data.frame(paramChange)
    df$Trial = trials
  }
  
  meanComplexParams = apply(complexAll, 2, median)
  meanComplexParams = meanComplexParams[grep("a.", names(meanComplexParams))]
  trial = seq(1,max(data$Trial), length.out = max(data$Block))
  
  complexData = as.data.frame(cbind(meanComplexParams, trial))
  
  if (mean){
    colnames(df) = c("Threshold", "Trial")
    plot = ggplot(data = df) +
      geom_line(aes(x = Trial, y = Threshold)) +
      ylim(c(0,5))+
      theme_classic()
  } else {
    colnames(df) = c("Threshold", "Participant", "Trial")
    plot = ggplot(data = df) +
      geom_line(aes(x = Trial, y = Threshold, group = Participant)) +
      ylim(c(0,5))+
      theme_classic()
  }
  
  if(complex) {
    plot <- plot +
      geom_point(data = complexData, aes(x = trial, y = meanComplexParams))
  }
  
  return(plot)
}

# Drift rate

getDriftChange = function(model, parameters, data){
  x = parameters
  if (grepl("v-power", model)){
    
    drift = (x["v.asym"]+x["v.start"])-x["v.start"]*data$Trial^(-x["v.rate"])
    
  } else if (grepl("v-exp", model) | model == "v-a-exp"){
    
    drift = (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial)
    
  } else if (grepl("v-linear", model)){
    
    drift = (x["v.b"]*data$Trial)+x["v.c"] 
    
  } else if (model == "simple") {
    
    drift = rep(x["v"],length(data$Trial))
    
  } else if (grepl("v-delayed-pow", model)){
    
    drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+data$Trial^(x["v.rate"])))
    
  } else if (grepl("v-delayed-exp", model) | grepl("v-dExp", model)){
    
    drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial)))
    
  }  else if (model == "v-step-fixed" | model == "v-a-step-fixed"){
    
    noFeedbackTrials = length(data$Trial[data$Block%in% c(1,2,3,4)])
    
    drift = c(rep(x["initial"], noFeedbackTrials), rep(x["initial"] + x["step"], length(data$Trial)-noFeedbackTrials))
  }
  
  drift
}


driftPlot = function(model,nRange, round = 1, complex = FALSE, mean = FALSE){
  
  df = as.data.frame(matrix(nrow = 0, ncol = 3))
  complexAll = NULL
  allParams = NULL
  for (i in nRange) {
    
    load(here(paste("modelling/evansetal-17/optim/round-",round,"/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    
    if(mean){
      params = apply(theta, 2, mean)
      allParams = rbind(allParams, params)
    } else {
      paramChange = getDriftChange(model, x, data)
      paramChange = as.data.frame(paramChange)
      paramChange$Participant =  i
      paramChange$Trial = trials
      df = rbind(df, paramChange)
    }
    
    # load complex block model that estimates a different parameter each block 
    load(here(paste("modelling/evansetal-17/optim/round-1/06_output/P", i,"_v-blocked-complex.Rdata", sep = "")))
    complexParams = apply(theta, 2, mean)
    complexAll = rbind(complexParams, complexAll)
  }
  
  if (mean) {
    allParams = apply(allParams, 2, median)
    paramChange = getDriftChange(model, allParams, data)
    df = as.data.frame(paramChange)
    df$Trial = trials
  }
  
  meanComplexParams = apply(complexAll, 2, median)
  meanComplexParams = meanComplexParams[grep("v.", names(meanComplexParams))]
  trial = seq(1,max(data$Trial), length.out = max(data$Block))
  
  complexData = as.data.frame(cbind(meanComplexParams, trial))
  
  if (mean){
    colnames(df) = c("Drift", "Trial")
    plot = ggplot(data = df) +
      geom_line(aes(x = Trial, y = Drift)) +
      ylim(c(0,3))+
      theme_classic()
  } else {
    colnames(df) = c("Drift", "Participant", "Trial")
    plot = ggplot(data = df) +
      geom_line(aes(x = Trial, y = Drift, group = Participant)) +
      theme_classic()
  }
  
  if(complex) {
    plot <- plot +
      geom_point(data = complexData, aes(x = trial, y = meanComplexParams))
  }
  
  return(plot)
}
# 
# driftPlot = function(model,nRange){
#   
#   driftDf = as.data.frame(matrix(nrow = 0, ncol = 3))
#   
#   for (i in nRange) {
#     
#     load(here(paste("modelling/evansetal-17/optim/round-2/06_output/P", i,"_",model,".Rdata", sep = "")))
#     
#     trials = 1:length(data$Trial)
#     x =  apply(theta, 2, mean)
#     
#     
#     drift = as.data.frame(drift)
#     drift$Participant =  i
#     drift$Trial = trials
#     driftDf  = rbind(driftDf, drift)
#   }
#   
#   colnames(driftDf) = c("Drift", "Participant", "Trial")
#   
#   plot = ggplot(data = driftDf) +
#     geom_line(aes(x = Trial, y = Drift, group = Participant)) +
#     theme_classic()
#   
#   return(plot)
# }


singleParticipant_a = function(participant,
                               trials = 1000,
                               ymin = 1,
                               ymax = 7,
                               round = 1,
                               complex = TRUE,
                               models = c(
                                 "simple",
                                 "a-linear",
                                 "a-power",
                                 "a-exp",
                                 "a-delayed-power",
                                 "a-delayed-exp",
                                 "v-linear",
                                 "v-power",
                                 "v-exp",
                                 "v-delayed-pow",
                                 "v-delayed-exp",
                                 "v-a-exp"
                               )) {
  plot(-1,-1,ylim = c(ymin,ymax),xlab = "trial",ylab = "a", xlim = c(0,trials), main = paste0("Participant ", participant))
  for(model in models){
    load(here(paste("modelling/evansetal-17/optim/round-",round,"/06_output/P", participant,"_",model,".Rdata", sep = "")))
    x =  apply(theta, 2, mean)
    
    threshold = getThresholdChange(model = model, parameters = x, data = data)
    
    if(complex){
    # load complex block model that estimates a different parameter each block 
    load(here(paste("modelling/evansetal-17/optim/round-1/06_output/P", participant,"_a-blocked-complex.Rdata", sep = "")))
    
    meanComplexParams = apply(theta, 2, mean)
    meanComplexParams = meanComplexParams[grep("a.", names(meanComplexParams))]
    trial = seq(1,max(data$Trial), length.out = max(data$Block))
    
    complexData = as.data.frame(cbind(meanComplexParams, trial))
    }
    
    lines(data$Trial,threshold)
    
    if(complex){
      points(trial,complexData$meanComplexParams)
    }
    
  }
  
  
}


singleParticipant_v = function(participant,
                               trials = 1000,
                               ymin = 0,
                               ymax = 4,
                               round = 1,
                               complex = TRUE,
                               models = c(
                                 "simple",
                                 "a-linear",
                                 "a-power",
                                 "a-exp",
                                 "a-delayed-power",
                                 "a-delayed-exp",
                                 "v-linear",
                                 "v-power",
                                 "v-exp",
                                 "v-delayed-pow",
                                 "v-delayed-exp",
                                 "v-a-exp"
                               )) {
  plot(-1,-1,ylim = c(ymin,ymax),xlab = "trial",ylab = "v", xlim = c(0,trials), main = paste0("Participant ", participant))
  for(model in models){
    load(here(paste("modelling/evansetal-17/optim/round-",round,"/06_output/P", participant,"_",model,".Rdata", sep = "")))
    x =  apply(theta, 2, mean)
    
    drift = getDriftChange(model = model, parameters = x, data = data)
    
    if(complex){
      # load complex block model that estimates a different parameter each block 
      load(here(paste("modelling/evansetal-17/optim/round-1/06_output/P", participant,"_v-blocked-complex.Rdata", sep = "")))
      
      meanComplexParams = apply(theta, 2, mean)
      meanComplexParams = meanComplexParams[grep("v.", names(meanComplexParams))]
      complex_trials = seq(1,max(data$Trial), length.out = max(data$Block))
      
      complexData = as.data.frame(cbind(meanComplexParams, complex_trials))
    }
    
    lines(data$Trial,drift)
    if(complex){
      points(complex_trials,complexData$meanComplexParams)
    }
  }
  
  
}


