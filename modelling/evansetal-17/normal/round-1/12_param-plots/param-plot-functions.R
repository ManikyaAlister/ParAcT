#rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(ggplot2, lib.loc = lib)

# Threshold

thresholdPlot = function(model,nRange){
  
  thresholdDf = as.data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in nRange) {
    
    load(here(paste("modelling/evansetal-17/normal/round-1/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    if (model == "a-power"){
      
      threshold = x["a.asym"]+x["a.start"]*data$Trial^(-x["a.rate"])
      
    } else if (model == "a-exp" | model == "v-a-exp"){
      
      threshold = x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
      
    } else if (model == "a-linear"){
      
      threshold = ((-x["a.b"])*data$Trial)+x["a.c"]
      
    } else if (model == "simple") {
      
      threshold = rep(x["a"],length(trials))
      
    } else if (model == "a-delayed-power"){

      threshold = x["a.asym"]+x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+data$Trial^(-x["a.rate"])))

    } else if (model == "a-delayed-exp") {

      threshold = x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))))

    } else if (model == "a-exp-mir"){

      threshold = (x["a.asym"]+x["a.start"])-x["a.start"]*exp(x["a.rate"]*data$Trial)

    } 
    
    threshold = as.data.frame(threshold)
    threshold$Participant =  i
    threshold$Trial = trials
    thresholdDf  = rbind(thresholdDf, threshold)
  }
  
  colnames(thresholdDf) = c("Threshold", "Participant", "Trial")
  
  a_plot = ggplot(data = thresholdDf) +
    geom_line(aes(x = Trial, y = Threshold, group = Participant)) +
    theme_classic()
  
  return(a_plot)
}

# Drift rate

driftPlot = function(model,nRange){
  
  driftDf = as.data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in nRange) {
    
    load(here(paste("modelling/evansetal-17/normal/round-1/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    if (model == "v-power"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*data$Trial^(-x["v.rate"])
      
    } else if (model == "v-exp" | model == "v-a-exp"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial)
      
    } else if (model == "v-linear"){
      
     drift = (x["v.b"]*data$Trial)+x["v.c"] 
      
    } else if (model == "simple") {
      
      drift = rep(x["a"],length(trials))
      
    } else if (model == "v-delayed-pow"){

      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+data$Trial^(x["v.rate"])))
      
    } else if (model == "v-delayed-exp"){

      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial)))

    }
    
    drift = as.data.frame(drift)
    drift$Participant =  i
    drift$Trial = trials
    driftDf  = rbind(driftDf, drift)
  }
  
  colnames(driftDf) = c("Drift", "Participant", "Trial")
  
  a_plot = ggplot(data = driftDf) +
    geom_line(aes(x = Trial, y = Drift, group = Participant)) +
    theme_classic()
  
  return(a_plot)
}


singleParticipant_a = function(participant,
                               trials = 1000,
                               ymin = 1,
                               ymax = 7,
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
  plot(-1,-1,ylim = c(ymin,ymax),xlab = "trial",ylab = "a", xlim = c(0,trials), main = paste0(participant))
  for(model in models){
    load(here(paste("modelling/evansetal-17/normal/round-1/06_output/P", participant,"_",model,".Rdata", sep = "")))
    x =  apply(theta, 2, mean)
    if (model == "a-power"){
      
      threshold = x["a.asym"]+x["a.start"]*data$Trial^(-x["a.rate"])
      
    } else if (model == "a-exp" | model == "v-a-exp"){
      
      threshold = x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
      
    } else if (model == "a-linear"){
      
      threshold = ((-x["a.b"])*data$Trial)+x["a.c"]
      
    } else if (model == "simple") {
      
      threshold = rep(x["a"],length(data$Trial))
      
    } else if (model == "a-delayed-power"){
      
      threshold = x["a.asym"]+x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+data$Trial^(x["a.rate"])))
      
    } else if (model == "a-delayed-exp") {
      
      x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))))
      
    } else if (model == "a-exp-mir"){
      
      threshold = (x["a.asym"]+x["a.start"])-x["a.start"]*exp(x["a.rate"]*data$Trial)
      
    } 
    lines(data$Trial,threshold)
    }
  
  
}


singleParticipant_v = function(participant,
                               trials = 1000,
                               ymin = 0,
                               ymax = 4,
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
  plot(-1,-1,ylim = c(ymin,ymax),xlab = "trial",ylab = "v", xlim = c(0,trials), main = paste0(participant))
  for(model in models){
    load(here(paste("modelling/evansetal-17/normal/round-1/06_output/P", participant,"_",model,".Rdata", sep = "")))
    x =  apply(theta, 2, mean)
    if (model == "v-power"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*data$Trial^(-x["v.rate"])
      
    } else if (model == "v-exp" | model == "v-a-exp"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial)
      
    } else if (model == "v-linear"){
      
      drift = (x["v.b"]*data$Trial)+x["v.c"] 
      
    } else if (model == "simple") {
      
      drift = rep(x["a"],length(data$Trial))
      
    } else if (model == "v-delayed-pow"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+data$Trial^(x["v.rate"])))
      
    } else if (model == "v-delayed-exp"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial)))
      
    }
    lines(data$Trial,drift)
  }
  
  
}


