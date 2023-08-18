# calculate maximum likelihoods
rm(list=ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(modelProb)



IC_array = function(models, criterion, generating, grouping_param) {
  # set up empty array
  allIC <- as.data.frame(matrix(ncol = length(models)))
  colnames(allIC) = c(models)
  gen_param <- c()
  for (j in 1:length(models)) {
    model <- models[j]
    gen <- generating[j]
    generating_data <- paste0(models[generating == TRUE], "-generated")
    for (i in 1:n) {
      if (!gen) {
        load(here(
          paste(
            "Recovery/model-recovery/",
            generating_data,
            "/fits/P",
            i,
            "_",
            model,
            ".Rdata",
            sep = ""
          )
        ))
        
        gen_param[i] <- genParams[grouping_param, 1]
        
      } else {
        load(here(
          paste(
            "Recovery/",
            model,
            "/Fits_recovery/P",
            i,
            "_",
            model,
            ".Rdata",
            sep = ""
          )
        ))
      }
      if (criterion == "AIC") {
        IC <- AIC
      } else if (criterion == "BIC") {
        IC <- BIC
      } else if (criterion == "ML"){
        IC <- max(weight)
      }
      allIC[i, model] = IC
    
    }
  }
  
  
  #allIC <- cbind(allIC, gen_param)
  
  allIC <- allIC[order(gen_param),]
  allIC
}
  
  n = 100
  
  # power - exp comparison
  
  recovering_model <- "simple"
  generating_model <- "v-power"
  
  models <- c(recovering_model,
              generating_model)
  
  generating <- c(FALSE, TRUE)
  
  
  allML<- IC_array(models,"ML", generating, grouping_param = "v.rate")

  # Transpose the matrix
  data <- t(allML)
  
  library(ggplot2)
  
  # Create a data frame from the transposed matrix
  df <- as.data.frame(data)
  
  # Add row names as a column
  df$Group <- rownames(df)
  
  # Reshape the data to long format using tidyr package
  library(tidyr)
  df_long <- pivot_longer(df, cols = -Group, names_to = "Variable", values_to = "Value")
  
  # Plot the grouped bar chart
  ggplot(df_long, aes(x = Variable, y = Value, fill = Group)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Group", y = "Value", title = "Grouped Bar Chart") +
    theme_minimal() +
    theme(legend.position = "top")
  
  