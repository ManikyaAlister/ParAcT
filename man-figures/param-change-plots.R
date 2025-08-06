rm(list = ls())
library(here)
library(ggplot2)
library(ggpubr)

source(here("functions/generic-functions.R"))
source(here("modelling/define-dataset-details.R"))
source(here("modelling/model-functions.R"))

# Define datasets
datasets <- c("evans-optim", "evans-normal", "knowles", "dutilh")

dataset_id <- datasets[1]

# define the kind of plot. Either "median" plots used in manuscript or "individual" plots used in supp
plot_type <- "median"

# Define 1-parameter models
v_models_1p <- c("v-exp",
                 "v-dExp",
                 "v-exp-blocked",
                 "v-block-trial-exp",
                 "v-dExp-blocked")
a_models_1p <- c("a-exp",
                 "a-dExp",
                 "a-exp-blocked",
                 "a-dExp-blocked",
                 "a-block-trial-exp")

# Add dataset-specific models
if (dataset_id == "evans-optim") {
  v_models_1p <- c(v_models_1p, "v-step-fixed")
  a_models_1p <- c(a_models_1p, "a-step-fixed")
}

# Get dataset details
dataset_index <- which(dataset_details$dataset_id == dataset_id)
subjects <- dataset_details$n_subjects[dataset_index]
derived_data <- dataset_details$save_IC_path[dataset_index]
output_path <- dataset_details$save_output_path[dataset_index][[1]]

# Load models
load(here(file.path(derived_data, "round-2-models.Rdata")))

# Define models to compare
# parameter_models <- list(
#   "v_models" = c("simple", v_models_1p, unique_2p_best),
#   "a_models" = c("simple", a_models_1p, unique_2p_best)
# )

all_models <- c("simple", "a_models", "v-models")

parameters <- c("a", "v")

# function that aggregates the medians of the parameter estimates instead of the central tendancy of each individual functio (plots in main manuscript)
# Function to plot parameter changes
plotParamChanges <- function(m, subjects, output_path, parameter, dataset_id) {
  all_complex <- all_paract <- NULL

  for (subject_i in subjects) {
    load(here(file.path(output_path(a_round = 1, subject = subject_i, m = paste0(parameter, "-blocked-complex")))))
    all_complex <- rbind(all_complex, apply(theta, 2, median))

    load(here(file.path(output_path(a_round = 1, subject = subject_i, m = m))))
    all_paract <- rbind(all_paract, apply(theta, 2, median))
  }

  median_complex <- apply(all_complex, 2, median)
  median_paract <- apply(all_paract, 2, median)

  model_functions <- all_functions[[m]]
  paract_function <- model_functions[[parameter]]

  paract_time <- if (model_functions$blocked_likelihood) {
    paract_function(median_paract, data = data, b = data$Block)
  } else {
    paract_function(median_paract, data = data)
  }

  # Prepare data frames for plotting
  paract_data <- data.frame(Trial = seq_along(paract_time), Paract = paract_time)
  trials_complex <- seq(1, max(data$Trial), length.out = max(data$Block))
  data_complex <- data.frame(
    Trial = trials_complex,
    Paract = median_complex[grepl(paste0(parameter, "."), names(median_complex))]
  )

  plot <- ggplot(paract_data, aes(x = Trial, y = Paract)) +
    geom_line() +
    geom_point(data = data_complex, aes(x = Trial, y = Paract), size = 1) +
    theme_classic() +
    theme(plot.margin = margin(1, 13, 1, 1),
          legend.position = "none",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 16)) +
    labs(
      title = model_functions$full_name,
      subtitle = paste0("n = ", length(subjects)),
      x = "Trial",
      y = parameter
    )

  if (dataset_id == "evans-optim") {
    plot <- plot + geom_vline(xintercept = sum(data$Block %in% c(1, 2, 3, 4)), colour = "red")
  }

  return(plot)
}

# Function that plots the central tendency of each individual function (supplementary materials)
plotParamChangesIndividuals <- function(m,
                             subjects,
                             output_path,
                             parameter,
                             dataset_id) {
  all_trajectories <- list()
  all_complex <- NULL
  
  for (subject_i in subjects) {
    # Load complex blocked model
    load(here(file.path(
      output_path(
        a_round = 1,
        subject = subject_i,
        m = paste0(parameter, "-blocked-complex")
      )
    )))
    all_complex <- rbind(all_complex, apply(theta, 2, median))
    
    # Load ParAcT model
    load(here(file.path(
      output_path(
        a_round = 1,
        subject = subject_i,
        m = m
      )
    )))
    subject_theta <- apply(theta, 2, median)
    
    model_functions <- all_functions[[m]]
    paract_function <- model_functions[[parameter]]
    
    data$PID <- subject_i
    
    subject_traj <- if (model_functions$blocked_likelihood) {
      paract_function(subject_theta, data = data, b = data$Block)
    } else {
      paract_function(subject_theta, data = data)
    }
    
    all_trajectories[[length(all_trajectories) + 1]] <- subject_traj
  }
  
  # Convert trajectories to long format to prevent recycling
  long_traj <- do.call(rbind, lapply(seq_along(all_trajectories), function(i) {
    data.frame(
      Subject = i,
      Trial = seq_along(all_trajectories[[i]]),
      Paract = all_trajectories[[i]]
    )
  }))
  
  # get maximum trial for the participant with the least data
  n_trials <- long_traj %>%
    group_by(Subject) %>%
    summarise(max_trial = max(Trial))
  
  max_trial <- min(n_trials$max_trial)
  
  # Compute pointwise median trajectory per trial (ignoring missing trials)
  paract_data <- long_traj %>%
    filter(Trial <= max_trial) %>% # so that there are no strange averaging artefacts
    group_by(Trial) %>%
    summarise(Paract = median(Paract), .groups = "drop") 
  
  
  # Block-level estimates (same as before)
  trials_complex <- seq(1, max(data$Trial), length.out = max(data$Block))
  median_complex <- apply(all_complex, 2, median)
  data_complex <- data.frame(Trial = trials_complex, Paract = median_complex[grepl(paste0(parameter, "."), names(median_complex))])
  
  # Plotting
  plot <- ggplot(paract_data, aes(x = Trial, y = Paract)) +
    # Add all subject trajectories (thin grey)
    geom_line(
      data = long_traj,
      aes(
        x = Trial,
        y = Paract,
        group = Subject,
        colour = Subject
      ),
      size = 0.4,
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    # Add median trajectory (thick black line)
    geom_line(size = 1.2) +
    # Add block-level point estimates
    geom_point(data = data_complex, aes(x = Trial, y = Paract), size = 1.2) +
    theme_classic() +
    theme(plot.margin = margin(1, 13, 1, 1),
          legend.position = "none",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 16)) +
    labs(
      title = model_functions$full_name,
      subtitle = paste0("n = ", length(subjects)),
      x = "Trial",
      y = parameter
    )
  
  if (dataset_id == "evans-optim") {
    plot <- plot + geom_vline(xintercept = sum(data$Block %in% c(1, 2, 3, 4)),
                              colour = "red")
  }
  
  return(plot)
}


plot_list <- list()

for (i_param in parameters) {
  #i_models <- parameter_models[[paste0(i_param, "_models")]]
  
  # load BIC data
  load(here(file.path(derived_data, "allBIC.rdata")))
  
  #i_BIC <- allBIC[, names(allBIC) %in% i_models]
  best_models <- rank_models(allBIC)[, 1]
  
  i_weighted_BIC <- modelProb::weightedICs(allBIC)
  best_model_weight <- apply(i_weighted_BIC, 1, max)
  names(best_model_weight) <- best_models
  
  split_models <- strsplit(best_models, "\\+")
  param_models <- sapply(split_models, function(x)
    x[grepl(paste0("^", i_param, "-"), x)])
  
  models_1p <- get(paste0(i_param, "_models_1p"))
  
  sum_change_weights <- sapply(models_1p, function(x) {
    sum(best_model_weight[param_models == x], na.rm = TRUE)
  })
  
  best_3_models <- names(sort(sum_change_weights, decreasing = TRUE)[1:3])
  best_3_models <- best_3_models[sum_change_weights[best_3_models] > 0]
  
  for (m in best_3_models) {
    subjects <- which(param_models == m)
    if (plot_type == "median"){
      plot_list[[m]] <- plotParamChanges(m, subjects, output_path, i_param, dataset_id)
    } else if (plot_type == "individual"){
      plot_list[[m]] <- plotParamChangesIndividuals(m, subjects, output_path, i_param, dataset_id)
      
    }
  } 
}

ggarrange(plotlist = plot_list)
ggsave(here(file.path(
  "man-figures", paste0("param-plot-", dataset_id, "-",plot_type,".png")
)), width = 11, height = 5)
