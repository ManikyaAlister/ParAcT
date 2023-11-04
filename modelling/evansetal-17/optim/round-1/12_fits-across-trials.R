library(dplyr)
library(ggplot2)

nSub = 3
dataset = "evansetal-17"
#model = "a-power"
# vector of the best model for each participant  
best_models = rep("a-power", nSub)

for (useSub in nSub) {
  
  # load participant data 
  load(here(paste0("data/",dataset,"/clean/P",useSub,"-Optim-Trial.Rdata")))
  
  # determine best model for that participant 
  best_model <- best_models[useSub]
  
  # Load simulated data
    load(paste(
      "modelling/evansetal-17/optim/round-1/08_model-predictions/P",
      useSub,
      "_",best_model,".Rdata",
      sep = ""
    ))
    sim<- sim[1:length(data$PID),] # make sure they have the same length
    sim$Block <- data$Block
    alt <- sim
    
    
  # Load simple model simulated data
    load(paste(
      "modelling/evansetal-17/optim/round-1/08_model-predictions/P",
      useSub,
      "_simple.Rdata",
      sep = ""
    ))
    
    sim<- sim[1:length(data$PID),] # make sure they have the same length
    sim$Block <- data$Block
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
  
  # Plot RTs
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
  
  # Summarize accuracy data into 
  # binning method (get average accuracy in bins of trials)
  getBinnedAcc = function(d, n_bins = 24*3){
    acc_bin <- d %>% 
      mutate(bin = rep(1:n_bins, length.out = length(d$Resp), each = (length(d$Resp)/n_bins)+1)) %>%
      group_by(Block, bin) %>%
      summarise(binned_accuracy = mean(Resp), model = model) 
    acc_bin$x = 1:nrow(acc_bin)
  
    acc_bin
    
  }
  data$model = "Emperical"
  simple$model = "Simple"
  alt$model = best_model
  
  n_bins = 24*6
    
   acc_bin_data = getBinnedAcc(data, n_bins = n_bins)
   acc_bin_simple = getBinnedAcc(simple, n_bins = n_bins)
   acc_bin_alt = getBinnedAcc(alt, n_bins = n_bins)
   
   acc_bin_plot_data = rbind(acc_bin_alt, acc_bin_data, acc_bin_simple)

   acc_bin_plot_data %>%
     ggplot(aes(x = bin, y = binned_accuracy, color = model)) +
     geom_point()+
     geom_smooth() +
     labs(color = "Model") 
  # Running accuracy (calculate the average accuracy at each trial)
  
  getRunningAccuracy = function(data){
    running_accuracy = c()
    for (i in 1:length(data$Trial)){
      acc <- data$Resp
      running_accuracy[i] = (sum(running_accuracy)+acc[i])/i
    }
    running_accuracy
  }

data$running_accuracy = getRunningAccuracy(data)
data$model = "Emperical"
simple$running_accuracy = getRunningAccuracy(simple)
simple$model = "Simple"
alt$running_accuracy = getRunningAccuracy(alt)
alt$model = best_model

acc_plot_data <- rbind(data[1:100,colnames(alt)], simple[1:100,], alt[1:100,])

acc_plot_data %>%
  #filter(model != "Simple") %>%
  ggplot(aes(x = Trial, y = running_accuracy, colour = model)) +
  geom_line()+
  geom_smooth()+
  theme(legend.position = "bottom")
  #geom_smooth(data = simple)+


}

