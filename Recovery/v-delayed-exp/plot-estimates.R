driftPlot = function(model,nRange, plotGen = FALSE){
  
  driftDf = as.data.frame(matrix(nrow = 0, ncol = 3))
  for (i in nRange) {
    load(here(paste("Recovery/v-delayed-exp/Fits_recovery/P", i,"_v-delayed-exp.Rdata", sep = "")))
    trials = 1:length(data$Trial)
    if (plotGen){
      gen = as.vector(genParams)
      x = c(genParams)
      names(x) = rownames(genParams)
      names(gen) = rownames(genParams)
    } else {
      x =  apply(theta, 2, mean)
    }


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
  print(genParams)
  print(apply(theta, 2, mean))
  return(a_plot)
}

library(here)
library(ggplot2)

plot_gen = function(participant){
  load(here(paste0("Recovery/v-delayed-exp/Fits_recovery/P",participant,"_v-delayed-exp.Rdata")))
  gen = as.vector(genParams)
  x = c(genParams)
  names(x) = rownames(genParams)
  names(gen) = rownames(genParams)
  drift_gen = (x["v.asym"]+x["v.start"])-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial)))
  print(genParams)
  print(apply(theta, 2, mean))
  plot(data$Trial, drift_gen, "l")
}

plot_gen(4)
driftPlot("v-delayed-exp", 1, plotGen = T)
driftPlot("v-delayed-exp", 1, plotGen = F)


