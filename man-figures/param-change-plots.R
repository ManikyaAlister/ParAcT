rm(list = ls())
library(here)
library(ggplot2)
library(ggpubr)

source(here("functions/generic-functions.R"))
source(here("modelling/define-dataset-details.R"))
source(here("modelling/model-functions.R"))

# Define datasets
datasets <- c("evans-optim", "evans-normal", "knowles", "dutilh")
dataset_id <- datasets[3]

# Define 1-parameter models
v_models_1p <- c("v-exp", "v-dExp", "v-exp-blocked", "v-block-trial-exp", "v-dExp-blocked")
a_models_1p <- c("a-exp", "a-dExp", "a-exp-blocked", "a-dExp-blocked", "a-block-trial-exp")

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
    theme(plot.margin = margin(1, 9, 1, 1)) +
    labs(title = model_functions$full_name, subtitle = paste0("n = ", length(subjects)), x = "Trial", y = parameter)
  
  if (dataset_id == "evans-optim") {
    plot <- plot + geom_vline(xintercept = sum(data$Block %in% c(1, 2, 3, 4)), colour = "red")
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
  param_models <- sapply(split_models, function(x) x[grepl(paste0("^", i_param, "-"), x)])
  
  models_1p <- get(paste0(i_param, "_models_1p"))
  
  sum_change_weights <- sapply(models_1p, function(x) {
    sum(best_model_weight[param_models == x], na.rm = TRUE)
  })
  
  best_3_models <- names(sort(sum_change_weights, decreasing = TRUE)[1:3])
  best_3_models <- best_3_models[sum_change_weights[best_3_models] > 0]
  
  for (m in best_3_models) {
    subjects <- which(param_models == m)
    plot_list[[m]] <- plotParamChanges(m, subjects, output_path, i_param, dataset_id)
  }
}

ggarrange(plotlist = plot_list)
ggsave(here(file.path("man-figures", paste0("param-plot-", dataset_id, ".png"))), width = 11, height = 5)
