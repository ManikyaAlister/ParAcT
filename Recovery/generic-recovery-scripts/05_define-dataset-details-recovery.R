models <- c("simple", "a-linear", "a-power", "a-exp", "a-dExp", "v-linear", "v-power", "v-exp", "v-dExp")

load_data = function(id = dataset_id, subject = useSub){
  here(paste0("Recovery/",id, "/Datasets/RECOVERY_DATA-DIFF_LHS-",subject,".Rdata"))
}

save_output = function(m = model, subject = useSub){
  here(paste0("Recovery/", m, "/Fits_recovery/P",subject,"-",m,".Rdata"))
}

# Generate lists of function calls for each model
load_data_paths = lapply(models, function(id=dataset_id) function() load_data(id))
save_output_paths = lapply(models, function(m=model) function() save_output(m))

# Create data set_details list with these
dataset_details <- list(
  dataset_id = models,
  save_output_path = save_output_paths,
  load_data_path = load_data_paths,
  simulate_fits = rep(FALSE, length(models)),
  save_IC_path = rep(FALSE, length(models))
)
