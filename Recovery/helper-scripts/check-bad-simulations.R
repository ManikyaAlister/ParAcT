model <- "a-power-rr"
dir <- paste0("~/Documents/Projects/ParAcT/Recovery/",model,"/Datasets/")

bad_d <- NULL
for (i in 1:100){
  load(paste0(dir,"RECOVERY_DATA-DIFF_LHS-",i,".Rdata"))
  sum = sum(data[["Resp"]] <0)
  if (sum>0){
    bad_d <- c(bad_d, i)
    
  }
}
print(bad_d)