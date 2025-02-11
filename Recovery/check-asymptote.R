rm(list = ls())
library(here)

#define models
data_model <- "z-var"
other_model <- "a-exp"
models <- c(data_model, other_model)

# define parameters that are to be correlated
params <- c("a", "a.asym")

# define empty data set to be filled with values
all_params <- data.frame("1" = rep(NA,100), "2" = rep(NA,100))
colnames(all_params) <- params

for(j in 1:length(models)){
  model <- models[j]
  model_param <- params[j]
  for(i in 1:100){
    load(here(paste0("Recovery/",data_model,"/Fits_recovery/P",i,"-",model,".Rdata")))
    params_summ <- apply(theta,2,mean)
    corr_param <- params_summ[model_param]
    if (grepl("v.asym", model_param)){
      params_start <- params_summ[grepl(".start",names(params_summ))]
      corr_param <- corr_param +params_start
    }
    all_params[i,model_param] <- corr_param
  }
  
}

png(file = here(paste0("Recovery/", data_model, "/check_asymptote_", params[1], "-", params[2], ".png")))
corr <- round(cor(all_params[, params[1]], all_params[, params[2]], ), 2)
plot(
  all_params[, params[1]],
  all_params[, params[2]],
  sub = paste0("r = ", corr),
  xlab = params[1],
  ylab = params[2]
)
abline(a = 0, b = 1, col = "red")
dev.off()

