rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(ggplot2, lib.loc = lib)

n = 9

# Threshold

thresholdPlot = function(model,n){
  
  thresholdDf = as.data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in 1:n) {
    
    load(here(paste("modelling/evansetal-18/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    if (model == "a-power"){
      
      threshold = x["a.asym"]+x["a.start"]*data$Trial^(-x["a.rate"])
      
    } else if (model == "a-exp"){
      
      threshold = x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial)
      
    } else if (model == "a-linear"){
      
      threshold = ((-x["a.b"])*data$Trial)+x["a.c"]
      
    } else if (model == "simple") {
      
      threshold = rep(x["a"],length(trials))
      
    } else if (model == "a-delayed-power"){

      threshold = x["a.asym"]+x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+data$Trial^(-x["a.rate"])))

    } else if (model == "a-delayed-exp") {

      threshold = x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))))

    } else if (model == "a-exp-mir" | model == "v-a-exp-mir"){

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
power = thresholdPlot("a-power",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-pow.png"),plot = power)

exp = thresholdPlot("a-exp",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-exp.png"),plot = exp)

linear = thresholdPlot("a-linear",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-linear.png"),plot = linear)

simple = thresholdPlot("simple",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-simple.png"),plot = simple)

a_exp_mir = thresholdPlot("a-exp-mir",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-exp-mir.png"), plot = a_exp_mir)

a_delayed_pow = thresholdPlot("a-delayed-power",n)

a_delayed_exp = thresholdPlot("a-delayed-exp",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a_delayed-exp.png"), plot = a_v_exp_mir_thresh)

a_v_exp_mir_thresh = thresholdPlot("v-a-exp-mir",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a_v_exp_mir_thresh.png"), plot = a_v_exp_mir_thresh)


# Drift rate

driftPlot = function(model,n){
  
  driftDf = as.data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in 1:n) {
    
    load(here(paste("modelling/evansetal-18/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    if (model == "v-power"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*data$Trial^(-x["v.rate"])
      
    } else if (model == "v-exp" | model == "v-a-exp-mir"){
      
      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*exp(-x["v.rate"]*data$Trial)
      
    } else if (model == "v-linear"){
      
     drift = (x["v.b"]*data$Trial)+x["v.c"] 
      
    } else if (model == "simple") {
      
      drift = rep(x["a"],length(trials))
      
    } else if (model == "v-delayed-pow"){

      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+data$Trial^(x["v.rate"])))
      
    } else if (model == "v-delayed-exp"){

      drift = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(-x["v.rate"]*data$Trial)))

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

v_power = driftPlot("v-power",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/v-power.png"), plot = v_power)

v_linear = driftPlot("v-linear",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/v-linear.png"), plot = v_linear)

v_exp = driftPlot("v-exp",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/v-exp.png"), plot = v_exp)

a_v_exp_mir_drift = driftPlot("v-a-exp-mir",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a_v_exp_mir_drift.png"), plot = a_v_exp_mir_drift)
