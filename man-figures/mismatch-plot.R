rm(list = ls())
library(here)

getParams <- function(param, theta, gen_params = NULL) {
  if (is.null(gen_params)){
    params <- apply(theta, 2, mean)
  }else {
    params <- as.numeric(gen_params[,1])
    names(params) <- rownames(gen_params)
  }
  est <- params[param]
  est
}

getCredIntParams <- function(param, theta) {
    params <- apply(theta, 2, function(x) quantile(x, probs = c(0.055, 0.5, 0.945)))
  est <- params[,param]
  est
}



getEstimateMismatch = function(n_subjects, path_to_output, gen_models = NULL) {
  a_asym = matrix(ncol = 3, nrow = n_subjects)
  colnames(a_asym) <- c("lower89", "median", "upper89")
  a_rate = c()
  v_asym = matrix(ncol = 3, nrow = n_subjects)
  colnames(v_asym) <- c("lower89", "median", "upper89")
  v_rate = c()
  a = matrix(ncol = 3, nrow = n_subjects)
  colnames(a) <- c("lower89", "median", "upper89")
  v = matrix(ncol = 3, nrow = n_subjects)
  colnames(v) <- c("lower89", "median", "upper89")
  # load estimated parameters
  for (i in 1:n_subjects) {
    if (is.null(gen_models)){
      load(here(paste0(path, "P", i, "-a-exp.Rdata")))
      a_asym[i,] <- getCredIntParams("a.asym", theta)
      a_rate[i] <- getParams("a.rate", theta)
      load(here(paste0(path, "P", i, "-v-exp.Rdata")))
      v_asym[i,] <-
        getCredIntParams("v.asym", theta) + getCredIntParams("v.start", theta)
      v_rate[i] <- getParams("v.rate", theta)
      load(here(paste0(path, "P", i, "-simple.Rdata")))
      a[i,] <- getCredIntParams("a",  theta)
      v[i,] <- getCredIntParams("v",  theta)
    } else {
      load(here(paste0("Recovery/model-recovery/",gen_models[grep("a-",gen_models)],"-generated/fits/P",i,"-simple.Rdata")))
      # get the true generating parameters
      a_asym[i,] <- rep(getParams("a.asym", theta, gen_params = genParams),3)
      a_rate[i] <- getParams("a.rate", theta, gen_params = genParams)
      a[i,] <- rep(getParams("a",  theta),3)
      load(here(paste0("Recovery/model-recovery/",gen_models[grep("v-",gen_models)],"-generated/fits/P",i,"-simple.Rdata")))
      v[i,] <- rep(getParams("v",  theta),3)
      v_asym[i,] <-
        rep(getParams("v.asym", theta, gen_params = genParams) + getParams("v.start", theta, gen_params = genParams),3)
      v_rate[i] <- getParams("v.rate", theta, gen_params = genParams)
    }
  } 

  
  
  order_a_rate <- order(a_rate)
  ordered_labels_a <- round(a_rate[order_a_rate],3)
  
  order_v_rate <- order(v_rate)
  ordered_labels_v <- round(v_rate[order_v_rate],3) 
  
  a_diff <- a - a_asym
  print(paste0("Median a: ", round(median(a[,"median"]),2), " Median a.asym: ", round(median(a_asym[,"median"]),2)))
  
  a_diff <- a_diff[order_a_rate,]
  median_a_diff <- median(a_diff[,"median"])
  print(paste0("median a: ", round(median_a_diff,2)))
  range_a_diff <- round(range(a_diff[,"median"]),2)
  print(paste0("range a: ", range_a_diff))
  
  
  v_diff <- v - v_asym
  print(paste0("median v: ", round(median(v),2), " median v.asym: ", round(median(v_asym),2)))
  
  
  v_diff <- v_diff[order_v_rate,] # needs to be in the same order as 
  median_v_diff <- median(v_diff[,"median"])
  range_v_diff <- range(v_diff[,"median"])
  print(paste0("median v: ", round(median_v_diff,2)))
  print(paste0("range v: ",round(range_v_diff,2)))
  
  plot(
    1:n_subjects,
    a_diff[,"median"],
    "l",
    ylim = c(-4, 3),
    col = "darkgreen",
    xlab = "",
    ylab = "",
    xaxt = "n"  )
  lines(1:n_subjects, v_diff[,"median"], col = "blue")
  #abline(h = median_a_diff, col = "red")
  #abline(h = median_v_diff, col = "red")
  abline(h = 0, col = "red")
  
  
  # Create shaded polygon for credible interval a_diff
  polygon(
    c(1:n_subjects, rev(1:n_subjects)),
    c(a_diff[,"upper89"], rev(a_diff[,"lower89"])),
    col = rgb(0, 1, 0, 0.3),
    border = NA
  )
  
  # Create shaded polygon for credible interval v_diff
  polygon(
    c(1:n_subjects, rev(1:n_subjects)),
    c(v_diff[,"upper89"], rev(v_diff[,"lower89"])),
    col = rgb(0, 0, 1, 0.25),
    border = NA
  )
  
  axis(side = 1, at = 1:n_subjects, labels = ordered_labels_a)
  axis(side = 3, at = 1:n_subjects, labels = ordered_labels_v)
  mtext(side = 3, line = 3, expression(paste("a ", eta, " (rate parameter)")), cex = 1.2)  
  mtext(side = 2, line = 3, "Difference between asymptote and standard DDM estimates", cex = 1.2)
  mtext(side = 1, line = 3, expression(paste("v ", eta, " (rate parameter)")), cex = 1.2)  
}

# Start a PDF device to save the plots to a PDF file
pdf(paste0("man-figures/estimate-mismatch.pdf"), width = 13, height = 6)
par(mfrow = c(1, 4), oma = c(0, 1, 4, 0))  # Adjust the bottom margin (oma) to move titles closer

n_subjects =  10
path = "modelling/evansetal-17/optim/round-1/06_output/"
getEstimateMismatch(n_subjects = n_subjects, path_to_output = path)
mtext("Data Set 1 (Detailed feedback after 4th Block)", side = 3, line = 5)

n_subjects =  11
path = "modelling/evansetal-17/normal/round-1/06_output/"
getEstimateMismatch(n_subjects = n_subjects, path_to_output = path)
mtext("Data Set 2", side = 3, line = 5)

path = "modelling/knowlesetal-19/round-1/06_output/"
n_subjects <- 147
getEstimateMismatch(n_subjects = n_subjects, path_to_output = path)
mtext("Data Set 3 (Practice Block Removed)", side = 3, line = 5)

path = "modelling/dutilhetal-09/round-1/06_output/"
n_subjects <- 4
getEstimateMismatch(n_subjects = n_subjects, path_to_output = path)
mtext("Data Set 4", side = 3, line = 5)

dev.off()

# path = "Recovery/model-recovery/a-exp-generated/fits/"
# n_subjects <- 100
# getEstimateMismatch(n_subjects = n_subjects, path_to_output = "", gen_models = c("a-exp", "v-exp"))
# 
# 


