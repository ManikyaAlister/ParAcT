library(dplyr)
library(ggplot2)

nSub = 1
dataset = "knowlesetal-19"

# vector of the best model for each participant  
best_models = rep("a-exp", nSub)

for (useSub in 1:nSub) {
  
  # load participant data 
  load(here(paste0("data/",dataset,"/clean/P",useSub,".Rdata")))
  
  # determine best model for that participant 
  best_model <- best_models[useSub]
  
  # Load simulated data
    load(paste(
      "modelling/knowlesetal-19/round-1/08_model-predictions/P",
      useSub,
      "_",best_model,".Rdata",
      sep = ""
    ))
    
    alt <- sim
    
  # Load simple model simulated data
    load(paste(
      "modelling/knowlesetal-19/round-1/08_model-predictions/P",
      useSub,
      "_simple.Rdata",
      sep = ""
    ))
    
    simple <- sim 
    
  # Add trial column to sim data 
  simple$Trial <- rep_len(1:length(data[,1]), length.out = length(sim[,1]))
  alt$Trial <- rep_len(1:length(data[,1]), length.out = length(sim[,1]))
  
  
  # Get means and sd of simulated data collapsed across trials
  alt_sum <- as.data.frame(alt) %>%
    group_by(Trial) %>%
    summarise(mean = mean(Time), sd = sd(Time))
  
  simple_sum <- as.data.frame(simple) %>%
    group_by(Trial) %>%
    summarise(mean = mean(Time), sd = sd(Time))
  
  # Plot 
data %>%
    ggplot(aes(x = Trial, y = Time))+
    geom_point(aes(x = Trial, y = Time))+
    geom_smooth()+
    geom_smooth(data = simple, aes(x = Trial, y = Time), colour = "green")+
    geom_smooth(data = alt, aes(x = Trial, y = Time), colour = "purple")+
    geom_vline(xintercept = 40*4, colour = "red")

data %>%
  ggplot(aes(x = Trial, y = Time))+
  geom_point(aes(x = Trial, y = Time))+
  geom_line(data = simple_sum, aes(x = Trial, y = mean), colour = "green")+
  geom_line(data = alt_sum, aes(x = Trial, y = mean), colour = "purple")+
  geom_vline(xintercept = 40*4, colour = "red")
    
  
  data %>%
    ggplot()+
    geom_point(aes(x = Trial, y = Time))+
    geom_line(data = alt_sum, aes(x = Trial, y = mean), colour = "purple")+
    geom_line(data = simple_sum, aes(x = Trial, y = mean), colour = "green")
  
  
}





