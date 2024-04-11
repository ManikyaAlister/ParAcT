rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)


IC_array = function(estimating_models, criterion, generating_data, n_sub = 100, grouping_param) {
  n_models <- length(estimating_models)
  # Set up an empty data frame
  allIC <- matrix(NA, ncol = n_models, nrow = n_sub)  # Initialize with NAs
  colnames(allIC) <- estimating_models
  gen_param = c()
  for (j in 1:n_models) {
    model <- estimating_models[j]
    for (i in 1:n_sub) {
      # Reset IC for each iteration
      IC <- NA  # Use NA as a default value
      
      # Construct file path
      file_path <- here(paste0("Recovery/", generating_data, "/Fits_recovery/P", i, "-", model, ".Rdata"))
      load(paste0("Recovery/",generating_data,"/Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata"))
      
      # Check if the file exists before trying to load it
      if (file.exists(file_path)) {
        load(file_path)
        # Determine which criterion to use
        if (criterion == "AIC") {
          IC <- AIC
        } else if (criterion == "BIC") {
          IC <- BIC
        }
      }
      gen_param[i] <- genParams[grouping_param, 1]
      # Assign the IC value to the appropriate place in the matrix
      allIC[i, j] <- IC  # Use matrix indexing [row, column]
    }
  }
  
  allIC <- allIC[order(gen_param),]
  
  # Convert the matrix to a data frame for the final output
  allIC_df <- as.data.frame(allIC)
  colnames(allIC_df) <- estimating_models
  return(allIC_df)
}

# define model groups 
a_models <- c("a-linear", "a-exp", "a-dExp")
v_models <- c("v-linear", "v-exp", "v-dExp")
a_exp_power <- c("a-exp", "a-power")
v_exp_power <- c("v-exp", "v-power")

# define details of each comparsion 
model_comparisons <- list(
  # can we distinguish between power and exponential changes? 
  list(data = "a-exp",
      models = a_exp_power,
      name = "a-exp-vs-a-power"),
  list(data = "a-power",
       models = a_exp_power,
       name = "a-power-vs-a-exp"),
  list(data = "v-exp",
      models = v_exp_power,
      name = "v-exp-vs-v-power"),
  list(data = "v-power",
       models = v_exp_power,
       name = "v-power-vs-v-exp"),
  # can we recover the simple model compared to all time-varying models? 
  list(data = "simple",
       models = c("simple", a_models, v_models),
       name = "simple-vs-all"),
  # can we distinguish between a time-varying functions? 
  # list(data = "a-linear",
  #      models = c("simple", a_models),
  #      name = "a-linear-vs-all-a"),
  list(data = "a-exp",
        models = c("simple", a_models),
        name = "a-exp-vs-all-a"),
  list(data = "a-dExp",
        models = c("simple", a_models),
        name = "a-dExp-vs-all-a"),
  # can we distinguish between v time-varying functions?
  list(data = "v-linear",
       models = c("simple", v_models),
       name = "v-linear-vs-all-v"),
  list(data = "v-exp",
        models = c("simple", v_models),
        name = "v-exp-vs-all-v"),
  list(data = "v-dExp",
        models = c("simple", v_models),
        name = "v-dExp-vs-all-v"),
  # can we distinguish between a and v exponential functions?
  list(data = "a-exp",
       models = c("a-exp", "v-exp"),
                  name = "a-exp-vs-v-exp"),
  list(data = "v-exp",
       models = c("a-exp", "v-exp"),
       name = "v-exp-vs-a-exp")
)

criteria <- c("BIC", "AIC")
output <- list()
for (i in 1:length(model_comparisons)){
  comparison_details <- model_comparisons[[i]]
  output_iteration <- list()
  estimating_models <- comparison_details$models
  generating_data <- comparison_details$data
  comparison_name <- comparison_details$name
  for (criterion in criteria){
    raw_IC <- IC_array(estimating_models = estimating_models,
                   generating_data = generating_data,
                   grouping_param = "z", # arbitrary grouping param for now
                   criterion = criterion) 
  
    #output_iteration[[paste0("n-best-",criterion)]] = ""

    weights <- modelProb::weightedICs(ICs = raw_IC)
    mean_weights <- apply(weights, 2, mean)

    # plot weighted probabilities and save
    png(paste0("Recovery/plots/", comparison_name, "-", criterion, ".png"))
    modelProb::plotWeightedICs(weights, 
                               colours = c("green", "red", "black", "blue", "purple","darkgreen", "orange"), 
                               cex = .8, # Adjust this value as needed
                               inset = c(0, 0),
                               position = "left",
                               main = paste0(comparison_name, " (generating data: ", generating_data, "), ",criterion)
                               ) # Adjust inset values as needed
    dev.off()
    
  }
  print(paste0(i, " out of ", length(model_comparisons)))
  
  
  # set up array of model comparison values
}


