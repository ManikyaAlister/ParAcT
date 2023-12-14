library(dplyr)
library(ggplot2)

nSub = 1
dataset = "evansetal-17"

load(here("data/evansetal-17/derived/optim/best_BIC.Rdata"))

# vector of all of the the best models for each participant  
best_models = unique(best_BIC)
  
model <- "v-a-exp"

subjects <- which(model == best_BIC)

alt_correct = NULL
alt_incorrect = NULL

simple_correct = NULL
simple_incorrect = NULL

all_data = NULL

for (useSub in subjects) {
  
  # load participant data 
  load(here(paste0("data/",dataset,"/clean/P",useSub,"-Optim-Trial.Rdata")))

    all_data <- rbind(data, data)
  
  # determine best model for that participant 
  #best_model <- best_models[useSub]
  
  # Load simulated data
    load(paste(
      "modelling/evansetal-17/optim/round-2/08_model-predictions/P",
      useSub,
      "_",model,".Rdata",
      sep = ""
    ))
    # Add trial column to sim data 
    sim$Trial <- rep_len(1:length(data[,1]), length.out = length(sim[,1]))
    
    # divide into correct and incorrect
    sim_correct <- sim %>%
      filter(Resp == 2)
    
    sim_incorrect <-  sim %>%
      filter(Resp == 1)
    
    # combine subjects
    alt_correct <- rbind(alt_correct,sim_correct)
    
    alt_incorrect <- rbind(alt_incorrect,sim_incorrect)
    
    
  # Load simple model simulated data
    load(paste(
      "modelling/evansetal-17/optim/round-2/08_model-predictions/P",
      useSub,
      "_simple.Rdata",
      sep = ""
    ))
    # Add trial column to sim data 
    sim$Trial <- rep_len(1:length(data[,2]), length.out = length(sim[,1]))
    
    # divide into correct and incorrect
    sim_correct <-  sim %>%
      filter(Resp == 2)
    
    sim_incorrect <-  sim %>%
      filter(Resp == 1)
    
    # combine subjects
    simple_correct <- rbind(simple_correct,sim_correct)
    simple_incorrect <- rbind(simple_incorrect,sim_incorrect)
    
    

}
  
  # Get means and sd of simulated data collapsed across trials
  alt_sum_correct <- as.data.frame(alt_correct) %>%
    group_by(Trial) %>%
    summarise(Time = mean(Time), sd = sd(Time), Resp = unique(Resp))
  
  alt_sum_incorrect <- as.data.frame(alt_incorrect) %>%
    group_by(Trial) %>%
    summarise(Time = mean(Time), sd = sd(Time), Resp = unique(Resp))
  
  simple_sum_correct <- as.data.frame(simple_correct) %>%
    group_by(Trial) %>%
    summarise(Time = mean(Time), sd = sd(Time), Resp = unique(Resp))
  
  simple_sum_incorrect <- as.data.frame(simple_incorrect) %>%
    group_by(Trial) %>%
    summarise(Time = mean(Time), sd = sd(Time), Resp = unique(Resp))
  
alt <- rbind(alt_sum_correct, alt_sum_incorrect)
simple <- rbind(simple_sum_correct, simple_sum_incorrect)


all_data <- all_data %>%
  mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                               Resp == 2 ~ "Correct"))
simple <- simple %>%
  mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                               Resp == 2 ~ "Correct"))
alt <- alt %>%
  mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                               Resp == 2 ~ "Correct"))
# Plot 
all_data %>%
    ggplot(aes(x = Trial, y = Time))+
    geom_point(aes(x = Trial, y = Time), alpha = .25)+
    geom_smooth(colour = "blue", method = "loess")+
    geom_smooth(data = simple, aes(x = Trial, y = Time), colour = "orange", method = "loess")+
    geom_smooth(data = alt, aes(x = Trial, y = Time), colour = "purple", method = "loess")+
    geom_vline(xintercept = 40*4, colour = "red")+
    facet_wrap(~accuracy)

# all_data %>%
#   ggplot(aes(x = Trial, y = Time))+
#   geom_point(aes(x = Trial, y = Time))+
#   geom_line(data = simple_sum, aes(x = Trial, y = mean), colour = "green")+
#   geom_line(data = alt_sum, aes(x = Trial, y = mean), colour = "purple")+
#   geom_vline(xintercept = 40*4, colour = "red")
#     
#   
#   all_data %>%
#     ggplot()+
#     geom_point(aes(x = Trial, y = Time))+
#     geom_line(data = alt_sum, aes(x = Trial, y = mean), colour = "purple")+
#     geom_line(data = simple_sum, aes(x = Trial, y = mean), colour = "green")
#   
#   






