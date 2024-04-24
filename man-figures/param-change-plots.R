rm(list = ls())
library(here)
library(ggplot2)
# Parameter change plots --------------------------------------------------

# define data sets
datasets <- c("evans-optim", "evans-normal", "knowles")

dataset_id <- commandArgs(trailingOnly = TRUE)

v_models_1p <- c(
  "v-linear",
  "v-exp",
  "v-dExp",
  "v-linear-blocked",
  "v-exp-blocked",
  "v-block-trial-exp",
  "v-dExp-blocked"
)

a_models_1p <- c(
  "a-linear",
  "a-exp",
  "a-dExp",
  "a-linear-blocked",
  "a-exp-blocked",
  "a-dExp-blocked",
  "a-block-trial-exp"
)
# evans optim (ds 1 in manuscript) has extra models
if (dataset_id == "evans-optim"){
  v_models_1p <- c(v_models_1p, "v-step-fixed")
  a_models_1p <- c(a_models_1p, "a-step-fixed")
}

# source data set details
source(here("modelling/generic_scripts/define-dataset-details.R"))

dataset_index <- which(dataset_details$dataset_id == dataset_id)
subjects <- dataset_details$n_subjects[dataset_index]
derived_data <- dataset_details$save_IC_path[dataset_index]
output_path <- dataset_details$save_output_path[dataset_index][[1]]

# load 2 param models 
load(here(paste0(derived_data,"round-2-models.Rdata")))

# define models to be compared for each parameter
# simple is included because want to be able to account for participants best fit
# by standard DDM. 
parameter_models <- list(
"v_models" = c("simple",v_models_1p, unique_2p_best),
"a_models" = c("simple", a_models_1p, unique_2p_best)
)


parameters <- c("a", "v")

# function to rank models based on their wegighted probability
rank_models <- function(scores_array) {
  # apply the ranking function to each row of the array
  ranked_array <- t(apply(scores_array, 1, rank))
  
  # create a new array to store the model names
  model_names <-
    array(dim = dim(ranked_array),
          dimnames = dimnames(ranked_array))
  
  # loop over each row of the ranked array
  for (i in 1:nrow(ranked_array)) {
    # sort the row by the rank and get the corresponding column names
    model_names[i,] <-
      colnames(ranked_array)[order(ranked_array[i,])]
  }
  
  colnames(model_names) <- 1:length(scores_array[1,])
  return(model_names)
}

# function to plot parameter changes
plotParamChanges = function(m, subjects, output_path, parameter, dataset_id){
  # empty vector to store parameter estimates of complex and time varying model
  all_complex <- NULL
  all_paract <- NULL
  for (subject_i in subjects) {
    # load complex model output for subject
    load(here(paste0(output_path(a_round = 1, subject = subject_i, m = paste0(parameter,"-blocked-complex")))))
    complex_theta <- apply(theta, 2, median)
    all_complex <- rbind(all_complex, complex_theta)
    # load paract model output
    load(here(paste0(output_path(a_round = 1, subject = subject_i, m = m))))
    paract_theta <- apply(theta,2,median)
    all_paract <- rbind(all_paract, paract_theta)
  }
  # collapse across subjects
  median_complex <- apply(all_complex, 2, median)
  median_paract <- apply(all_paract, 2, median)

  # get change function
  source(here("modelling/generic_scripts/model-functions.R"))
  

  # get model functions pertaining to model
  model_functions <- all_functions[[m]]
  
  # get full name of model
  full_name <- model_functions$full_name

  # get specific time-varying function of parameter of interest
  paract_function <- model_functions[[parameter]]

  # get parameter across time
  paract_time <- paract_function(median_paract, data = data)

  # create data frame for plotting
  paract_data <- data.frame(
    Trial = 1:length(paract_time),
    Paract = paract_time
  )

  # define trials for the complex model (since it is blocked)
  trials_complex <- seq(1,max(data$Trial), length.out = max(data$Block))

  # create a data frame for the complex model
  data_complex <- data.frame(
    Trial = trials_complex,
    Paract = median_complex[grepl(paste0(parameter,"."),names(median_complex))]
  )
  # plot parameter change
  plot <- ggplot(data = paract_data, aes(x = Trial, y = Paract)) +
    geom_line() +
    geom_point(data = data_complex, aes(x = Trial, y = Paract), size = 1) +
    theme_classic() +
    labs(title = full_name, subtitle = paste0("n = ",length(subjects)), x = "Trial", y = paste0(parameter)) 
  
  if (dataset_id== "evans-optim"){
    plot <- plot + geom_vline(xintercept = length(data$Block[data$Block %in% c(1,2,3,4)]), colour = "red")
  }
  
  plot
}

plot_list <- NULL 

for (i in 1:length(parameters)){
# define parameter for iteration
i_param <- parameters[i]

# get models for parameter
i_models <- parameter_models[[paste0(i_param,"_models")]]

# load best BIC information for data set
load(here(paste0(derived_data, "allBIC.rdata")))

# get BIC data information for i_models
i_BIC <- allBIC[,names(allBIC) %in% i_models]

# get best models for parameter
best_models <- rank_models(i_BIC)[,1]

# get weighted BICs
i_weighted_BIC <- modelProb::weightedICs(i_BIC)

# get the weighted BIC value for the best models
best_model_weight <- apply(i_weighted_BIC, 1, max)

# assign names to the weighted models from the ranked vector
names(best_model_weight) <- best_models

models_1p <- get(paste0(i_param,"_models_1p"))

# for each change function, get the best one 
sum_change_weights <- sapply(models_1p, function(x) {
  sum(best_model_weight[grepl(x, names(best_model_weight))])
}
  )

# get the best 3 models 
best_3_models <- names(sort(sum_change_weights, decreasing = TRUE)[1:3])

# define empty list to store plots for parameters
plot_list_param <- NULL
# plot parameter changes for best 3 models
for ( m in best_3_models){
  subjects <- grep(m, names(best_model_weight))
  plot <- plotParamChanges(m = m,subjects = subjects,output_path = output_path, parameter =  i_param, dataset_id = dataset_id)
  plot_list[[m]] <- plot

}
}


ggpubr::ggarrange(plotlist = plot_list)
ggsave(here(paste0("man-figures/param-plot-",dataset_id,".png")), width = 8, height = 5)

