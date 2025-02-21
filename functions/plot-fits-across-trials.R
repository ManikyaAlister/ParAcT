library(ggplot2)
library(dplyr)

fitRTAcrossTime = function(model, subjects, output_path,data_path, dataset_id){
  
  alt_correct = NULL
  alt_incorrect = NULL
  
  simple_correct = NULL
  simple_incorrect = NULL
  
  all_data = NULL
  
  # source model info that includes full names for plotting
  source(here("modelling/model-functions.R"))
  
  for (useSub in subjects) {
    
    # load participant data 
    load(data_path(subject = useSub))
    
    all_data <- rbind(all_data, data)
    
    # Load simulated data
    load(output_path(subj = useSub, fit = FALSE, m = model))
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
    load(output_path(subj = useSub, fit = FALSE, m = "simple"))
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
  
  print(nrow(all_data))
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
  
  # if (model == "simple"){
  #   # remove the alt data because it's the same as simple
  #   combined_data <- rbind(all_data, simple) 
  #   
  # } else {
     combined_data <- bind_rows(all_data, simple, alt) %>%
       mutate(Model = factor(Model, levels = c("Emperical Data", "Time-Varying (ParAcT) Variant", "Standard DDM")))
  # }
  
  # load full list of models and their full names for plotting
  #load(here(paste0("data/",dataset,"/derived",subset,"/all-models.Rdata")))
  #load(here(paste0("data/",dataset,"/derived",subset,"/model-full-names.Rdata")))
  
  #names(models) <- full_names
  #full_model_name <- names(models[models == model])
  full_name <- all_functions[[model]]$full_name
  
  plot <- ggplot(combined_data, aes(x = Trial, y = Time)) +
    geom_point(data = all_data, alpha = 0.1) +
    geom_smooth(aes(colour = Model), method = "loess") +
    labs(title = full_name, subtitle = paste0("n Best Fit = ", length(subjects)), colour = "Model", y = "Response Time (s)") +
    facet_wrap(~accuracy) +
    theme_bw()+
    scale_color_viridis_d() + 
    #scale_color_brewer()+
    theme(panel.grid = element_blank(), plot.margin = margin(1, 9, 1, 1)) # make sure x axis isn't cut off
    #ylim(0,6.5)
  
  if (dataset_id == "evans-optim"){
    plot <- plot + 
      geom_vline(xintercept = 40 * 4, colour = "red")
      #coord_cartesian(ylim=c(0,8)) 
  } else if (dataset_id == "knowles"){
    # more some outliers make smoothing more difficult to see 
    plot <- plot + coord_cartesian(ylim=c(0,8))  
  } else if (dataset_id == "dutilh") {
    # no lims for dutlh bc RTs are much lower due to speed emphasis
    plot <- plot
  } else {
    plot <- plot + coord_cartesian(ylim=c(0,5)) 
  }
  
  plot
  
}
