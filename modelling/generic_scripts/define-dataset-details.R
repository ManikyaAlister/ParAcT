
# output paths for model fits need to be able to dynamically handle whether it is a round 1 or 2 model. 
output_path_optim = function(a_round = 1, fit = TRUE, subject = useSub, m = model){
  if (fit){
    paste0("modelling/evansetal-17/optim/round-",a_round, "/06_output/p",subject,"-",m,".Rdata")
  } else {
    paste0("modelling/evansetal-17/optim/round-",a_round, "/08_model-predictions/p",subject,"-",m,".Rdata")
  }
}

output_path_normal = function(a_round = 1, fit = TRUE, subject = useSub, m = model){
  if (fit){
    paste0("modelling/evansetal-17/normal/round-", a_round, "/06_output/p",subject,"-",m,".Rdata")
  } else {
    paste0("modelling/evansetal-17/normal/round-", a_round, "/08_model-predictions/p",subject,"-",m,".Rdata")
  }
}


output_path_knowles = function(a_round = 1, fit = TRUE, subject = useSub, m = model){
  if (fit){
    paste0("modelling/knowlesetal-19/round-", a_round, "/06_output/p", subject, "-", m, ".Rdata")
  } else {
    paste0("modelling/knowlesetal-19/round-", a_round, "/08_model-predictions/p", subject, "-", m, ".Rdata")
  }
}

plot_path_optim = function(a_round = 1, fit = TRUE, subject = useSub, m = model){
    paste0("modelling/evansetal-17/optim/round-",a_round, "/07_plots/")
}

plot_path_normal = function(a_round = 1, fit = TRUE, subject = useSub, m = model){
    paste0("modelling/evansetal-17/normal/round-", a_round, "/07_plots/")
}


plot_path_knowles = function(a_round = 1, fit = TRUE, subject = useSub, m = model){
    paste0("modelling/knowlesetal-19/round-", a_round, "/07_plots/")
}


load_data_optim = function(subject = subj){
  paste0("data/evansetal-17/clean/P",subject,"-Optim-Trial.Rdata")
}

load_data_normal = function(subject = subj){
  paste0("data/evansetal-17/clean/P",subject,"-Norm-Trial.Rdata")
}

load_data_knowles = function(subject = subj){
  paste0("data/knowlesetal-19/clean/P",subject,".Rdata")
}

# Data structure with relevant file paths for all data sets ---------------
dataset_details <-
  list(
    dataset_number = c(1, 2, 3), # data set number in manuscript 
    dataset_id = c("evans-optim", "evans-normal", "knowles"), # id name 
    load_data_path = c(load_data_optim, load_data_normal, load_data_knowles),
    save_output_path = c(output_path_optim, output_path_normal, output_path_knowles),
    save_plot_paths = c(plot_path_optim, plot_path_normal, plot_path_knowles),
    save_IC_path = c("data/evansetal-17/derived/optim/", "data/evansetal-17/derived/normal/", "data/knowlesetal-19/derived/"),
    simulate_fits = c(T,T,T), # do you want to simulate the model fits after they are estimated? 
    n_subjects = c(10, 11, 147)
  )

save(dataset_details, file = here("data/dataset-details.Rdata"))


