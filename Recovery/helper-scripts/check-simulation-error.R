# check if data sets have a negative Resp column, caused by maxCount variable in simulation being too low

v_models <- c("simple", 
              "v-linear",
              "v-power-rr",
              "v-exp-re",
              "v-delayed-exp")

a_models <- c(
  "a-linear",
  "a-power-rr",
  "a-exp-rr",
  "a-delayed-exp",
  )

models <- c(a_models, v_models)

model = "a-power-rr"
nSub = 100
all_bad_datasets <- list()
for (model in "v-power-rr") {
  bad_datasets <- NULL
  for (useSub in 1:nSub) {
  load(paste("Recovery/",model,"/Datasets/RECOVERY_DATA-DIFF_LHS-",useSub,".Rdata",sep=""))
  is_neg <- sum(data[["Resp"]]==-1)
  if (is_neg > 0) {
    bad_datasets <- c(bad_datasets, useSub)
  }
  }
  if (length(bad_datasets) > 0) {
    all_bad_datasets[[model]] <- bad_datasets
  }
  
}



