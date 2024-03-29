library(dplyr)
library(here)
library(ggplot2)

fitRTAcrossTime = function(model, subjects, round = 2){
  
  alt_correct = NULL
  alt_incorrect = NULL
  
  simple_correct = NULL
  simple_incorrect = NULL
  
  all_data = NULL
  
  for (useSub in subjects) {
    
    # load participant data 
    load(here(paste0("data/",dataset,"/clean/P",useSub,"-Norm-Trial.Rdata")))
    
    all_data <- rbind(data, data)
    
    # determine best model for that participant 
    #best_model <- best_models[useSub]
    
    # Load simulated data
    load(paste(
      "modelling/evansetal-17/normal/round-",round,"/08_model-predictions/P",
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
      "modelling/evansetal-17/normal/round-2/08_model-predictions/P",
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
                                Resp == 2 ~ "Correct"),
           data = "Emperical")
  simple <- simple %>%
    mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                                Resp == 2 ~ "Correct"),
           data  = "Simple Model")
  alt <- alt %>%
    mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                                Resp == 2 ~ "Correct"),
           data = model)
  # Plot 
  library(ggplot2)
  
  # Assuming 'all_data', 'simple', and 'alt' are your datasets
  
  ggplot(all_data, aes(x = Trial, y = Time)) +
    geom_point(alpha = 0.25) +
    geom_smooth(aes(colour = data), method = "loess") +
    geom_smooth(data = simple, aes(colour = data), method = "loess") +
    geom_smooth(data = alt, aes(colour = data), method = "loess") +
    labs(title = model, subtitle = paste0("n = ", length(subjects)), colour = "Model") +
    geom_vline(xintercept = 40 * 4, colour = "red") +
    facet_wrap(~accuracy) +
    scale_colour_manual(values = c("blue", "orange", "purple"),
                        labels = c("Emperical Data", "Simple Model", "Alternative Model"))
  
  
}

nSub = 1
dataset = "evansetal-17"

load(here("data/evansetal-17/derived/normal/best_AIC.Rdata"))

# vector of all of the the best models for each participant  
best_models = unique(best_AIC)

model <- "v-dExp-a-dExp"
subjects <- which(model == best_BIC)
fitRTAcrossTime(model, subjects, round = 2)


# 
# plot_list = NULL
# for (i in length(best_models)) {
#   model <- best_models[i]
#   subjects <- which(model == best_BIC)
#   plot <- fitRTAcrossTime(model, subjects)
#   plot_list[[i]] <- plot
# }
# 
# fitRTAcrossTime("v-dExp-a-dExp", subjects)
# fitRTAcrossTime("a-exp", subjects, round = 1)
# 
# 
# ggarrange(plotlist = plot_list)
# 
