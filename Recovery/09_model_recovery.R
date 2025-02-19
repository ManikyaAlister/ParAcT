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

  #allIC <- allIC[order(gen_param),]
  
  # Convert the matrix to a data frame for the final output
  allIC_df <- as.data.frame(allIC)
  colnames(allIC_df) <- estimating_models
  return(allIC_df)
}

getNBestIC <- function(allIC){
  model_names <- names(allIC)
  n_IC = table(model_names[apply(allIC, 1, which.min)])
  n = rep(0, length(model_names))
  names(n) = model_names
  n[names(n_IC)] <- n_IC
  n
}


# define model groups 
a_models <- c("a-linear", "a-exp", "a-dExp")
v_models <- c("v-linear", "v-exp", "v-dExp")
a_exp_power <- c("a-exp", "a-power")
v_exp_power <- c("v-exp", "v-power")

# define details of each comparsion 
model_comparisons <- list(
  # can we distinguish between power and exponential changes?
  list(
    data = "a-exp",
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
  list(data = "a-linear",
       models = c("simple", a_models),
       name = "a-linear-vs-all-a"),
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
       name = "v-exp-vs-a-exp"),
  # checking whether across trial variability can explain ParAcT changes and vice/versa
  list(data = "v-var",
       models=c("v-var", "v-exp"),
       name = "v-var"),
  list(data = "z-var",
       models = c("z-var", "a-exp"),
       name = "z-var"),
  list(data = "v-exp",
       models = c("v-exp", "v-var"),
       name = "v-exp-var"),
  list(data = "a-exp",
       models = c("a-exp", "z-var"),
       name = "a-exp-var")
)

criteria <- c("BIC", "AIC")
output <- list()
for (i in 1:length(model_comparisons)){
  # get comparison details for this iteration
  comparison_details <- model_comparisons[[i]]
  
  # extract comparison information for this iteration 
  estimating_models <- comparison_details$models
  generating_data <- comparison_details$data
  comparison_name <- comparison_details$name
  
  # empty data structures to fill with relevant metrics for each IC
  output_iteration <- list()
  mean_weights <- list()
  n_best <- list()
  
  # loop through each IC and extract info
  for (j in 1:length(criteria)){
    criterion <- criteria[j]
    raw_IC <- IC_array(estimating_models = estimating_models,
                   generating_data = generating_data,
                   grouping_param = "z", # arbitrary grouping param for now
                   criterion = criterion) 
    
    #output_iteration[[paste0("n-best-",criterion)]] = ""
    # get weighted model probabilities
    weights <- modelProb::weightedICs(ICs = raw_IC)
    
    # get the mean weights 
    mean_weights[[criterion]] <- round(apply(weights, 2, mean),2)
    
    # get the number of participants best fit by each model
    best <- getNBestIC(raw_IC)
    # only care about values forr the data generating model
    n_best[[criterion]] <- best[generating_data]
    
    
    # plot weighted probabilities and save
    # png(paste0("Recovery/plots/", comparison_name, "-", criterion, ".png"))
    # modelProb::plotWeightedICs(weights, 
    #                            colours = c("green", "red", "black", "blue", "purple","darkgreen", "orange"), 
    #                            cex = .8, # Adjust this value as needed
    #                            inset = c(0, 0),
    #                            position = "left",
    #                            main = paste0(comparison_name, " (generating data: ", generating_data, "), ",criterion)
    #                            ) # Adjust inset values as needed
    # dev.off()
    
  }
  
  
  # Extract BIC and AIC values
  BIC_values <- mean_weights$BIC
  AIC_values <- mean_weights$AIC
  
  # Replace values less than 0.01 with "<0.01"
  BIC_values[BIC_values < 0.01] <- "<.01"
  AIC_values[AIC_values < 0.01] <- "<.01"
  
  # Generate the latex table row
  tmp_weights <- paste0(
    sprintf("%s (%s)", BIC_values, AIC_values),
    collapse = " & "
  )
  
  tmp_best <- paste0(
  sprintf("%s (%s)", n_best$BIC, n_best$AIC),
  collapse = " & "
  )
  
  
  
  
  latex_row <- paste(tmp_best, " & ", tmp_weights)
  
  model_comparisons[[i]]$metrics <- latex_row
  
  print(paste0(i, " out of ", length(model_comparisons)))
  
  
  # set up array of model comparison values
}

save(model_comparisons, file = here("Recovery/model_comparisons.Rdata"))



