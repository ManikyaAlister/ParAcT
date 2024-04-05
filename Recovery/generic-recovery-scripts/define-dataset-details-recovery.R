models <- c("simple", "a-linear", "a-power", "a-exp", "a-dExp", "v-linear", "v-power", "v-exp", "v-dExp")

load_data = function(model, subject = useSub){
  here(paste0("Recovery/", model, "/Datasets/RECOVERY_DATA-DIFF_LHS-",subject,".Rdata"))
}

save_output = function(model, subject = useSub){
  here(paste0("Recovery/", model, "/Fits_recovery/P",subject,"-",model,".Rdata"))
}

# Generate lists of function calls for each model
load_data_paths = lapply(models, function(model) function() load_data(model))
save_output_paths = lapply(models, function(model) function() save_output(model))

# Create dataset_details list with these
dataset_details <- list(
  dataset_id = models,
  save_output_path = save_output_paths,
  load_data_path = load_data_paths,
  simulate_fits = rep(FALSE, length(models)),
  save_IC_path = rep(FALSE, length(models))
)
