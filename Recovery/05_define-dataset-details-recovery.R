models <- c("simple", "a-linear", "a-power", "a-exp", "a-dExp", "v-linear", "v-power", "v-exp", "v-dExp", "v-var", "z-var")

# Higher-order function for load_data
load_data_generator = function(id) {
  function(subject = useSub) {
    here(paste0("Recovery/", id, "/Datasets/RECOVERY_DATA-DIFF_LHS-", subject, ".Rdata"))
  }
}

# Higher-order function for save_output
save_output_generator = function(id) {
  function(m = model, subject = useSub) {
    here(paste0("Recovery/", id, "/Fits_recovery/P", subject, "-", m, ".Rdata"))
  }
}

# Generate lists of specific functions for each model
load_data_paths = lapply(models, load_data_generator)
save_output_paths = lapply(models, save_output_generator)

# Create dataset_details list with these specific functions
dataset_details <- list(
  dataset_id = models,
  save_output_path = save_output_paths,
  load_data_path = load_data_paths,
  simulate_fits = rep(FALSE, length(models)),
  save_IC_path = rep(FALSE, length(models))
)
