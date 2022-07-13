rm(lists = ls())
lib = .libPaths("/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(ggplot2, lib.loc = lib)

n = 1

# Power model

thresholdPlot = function(model,n){
  
  thresholdDf = as.data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in 1:n) {
    
    load(here(paste("modelling/evansetal-18/06_output/P", i,"_",model,".Rdata", sep = "")))
    
    trials = 1:length(data$Trial)
    x =  apply(theta, 2, mean)
    if (model == "a-power"){
      
      threshold = x["a.asym"]+(x["a.asym"] + x["a.start"])*data$Trial^(-x["a.rate"])
      
    } else if (model == "a-exp"){
      
      threshold = x["a.asym"]+(x["a.asym"]+x["a.start"])*exp(-x["a.rate"]*data$Trial)
      
    } else if (model == "a-linear"){
      
      threshold = ((-x["a.b"])*data$Trial)+x["a.c"]
      
    } else if (model == "simple") {
      
      threshold = rep(x["a"],length(trials))
      
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

