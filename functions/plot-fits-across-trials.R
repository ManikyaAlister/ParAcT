library(ggplot2)
library(dplyr)

fitRTAcrossTime = function(model, subjects, dataset, subset, round = 2){
  
  alt_correct = NULL
  alt_incorrect = NULL
  
  simple_correct = NULL
  simple_incorrect = NULL
  
  all_data = NULL

  for (useSub in subjects) {
    
    # load participant data 
    if (subset == "/optim"){
      load(here(paste0("data/",dataset,"/clean/P",useSub,"-Optim-Trial.Rdata")))
    } else if (subset == "/normal"){
      load(here(paste0("data/",dataset,"/clean/P",useSub,"-Norm-Trial.Rdata")))
    } else {
      load(here(paste0("data/",dataset,"/clean/P",useSub,".Rdata")))
    }
    
    all_data <- rbind(data, data)
    
    # determine best model for that participant 
    #best_model <- best_models[useSub]
    
    # Load simulated data
    load(paste(
      "modelling/",dataset,"",subset,"/round-",round,"/08_model-predictions/P",
      useSub,
      "-",model,".Rdata",
      sep = ""
    ))
    # Add trial column to sim data 
    #sim$Trial <- rep_len(1:length(data[,1]), length.out = length(sim[,1]))
    
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
      "modelling/",dataset,"",subset,"/round-1/08_model-predictions/P",
      useSub,
      "-simple.Rdata",
      sep = ""
    ))
    # Add trial column to sim data 
    #sim$Trial <- rep_len(1:length(data[,2]), length.out = length(sim[,1]))
    
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
           data = "Emperical",
           # column for plotting legend
           Model = "Emperical Data") %>%
    select(Trial, Time, Resp, accuracy, data, Model)
  simple <- simple %>%
    mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                                Resp == 2 ~ "Correct"),
           data  = "Simple Model",
           Model = "Standard DDM") %>%
    select(-sd)
  alt <- alt %>%
    mutate(accuracy = case_when(Resp == 1 ~ "Incorrect",
                                Resp == 2 ~ "Correct"),
           data = model,
           Model = "Time-Varying (ParAcT) Variant") %>%
    select(-sd)
  
  combined_data <- rbind(all_data, simple, alt)
  
  # load full list of models and their full names for plotting
  #load(here(paste0("data/",dataset,"/derived",subset,"/all-models.Rdata")))
  #load(here(paste0("data/",dataset,"/derived",subset,"/model-full-names.Rdata")))
  
  #names(models) <- full_names
  #full_model_name <- names(models[models == model])
  
  
  ggplot(combined_data, aes(x = Trial, y = Time)) +
    geom_point(data = all_data, alpha = 0.25) +
    geom_smooth(aes(colour = Model), method = "loess") +
    labs(title = model, subtitle = paste0("n = ", length(subjects)), colour = "Model") +
    geom_vline(xintercept = 40 * 4, colour = "red") +
    facet_wrap(~accuracy) +
    theme_bw()+
    theme(panel.grid = element_blank())
}
