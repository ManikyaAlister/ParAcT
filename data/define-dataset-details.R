
# output paths for model fits need to be able to dynamically handle whether it is a round 1 or 2 model. 
output_path_optim = function(analysis_round = round, fit = TRUE, subject = useSub, m = model){
  if (fit){
    paste0("modelling/evansetal-17/optim/round-",round, "/06_output/p",subject,"-",m,".Rdata")
  } else {
    paste0("modelling/evansetal-17/optim/round-",round, "/08_model-predictions/p",subject,"-",m,".Rdata")
  }
}

output_path_normal = function(round = analysis_round, fit = TRUE, subject = useSub, m = model){
  if (fit){
    paste0("modelling/evansetal-17/normal/round-", round, "/06_output/p",subject,"-",m,".Rdata")
  } else {
    paste0("modelling/evansetal-17/normal/round-", round, "/08_model-predictions/p",subject,"-",m,".Rdata")
  }
}


output_path_knowles = function(round = analysis_round, fit = TRUE, subject = useSub, m = model){
  if (fit){
    paste0("modelling/knowlesetal-19/round-", round, "/06_output/p", subject, "-", m, ".Rdata")
  } else {
    paste0("modelling/knowlesetal-19/round-", round, "/08_model-predictions/p", subject, "-", m, ".Rdata")
  }
}


load_data_optim = function(subject = subj){
  paste0("data/evansetal-17/clean/P",subj,"-Optim-Trial.Rdata")
}

load_data_normal = function(subject = subj){
  paste0("data/evansetal-17/clean/P",subj,"-Norm-Trial.Rdata")
}

load_data_knowles = function(subject = subj){
  paste0("data/knowlesetal-19/clean/P",subj,".Rdata")
}

# Data structure with relevant file paths for all data sets ---------------
dataset_details <-
  list(
    dataset_number = c(1, 2, 3), # data set number in manuscript 
    dataset_id = c("evans-optim", "evans-normal", "knowles"), # id name 
    load_data_path = c(load_data_optim, load_data_normal, load_data_knowles),
    save_output_path = c(output_path_optim, output_path_normal, output_path_knowles),
    save_IC_path = c("data/evansetal-17/derived/optim/", "data/evansetal-17/derived/normal/", "data/knowlesetal-19/derived/"),
    simulate_fits = c(T,T,T) # do you want to simulate the model fits after they are estimated? 
  )

save(dataset_details, file = here("data/dataset-details.Rdata"))


