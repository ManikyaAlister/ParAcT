rm(list = ls())

# library(here)
# setwd("Recovery")
library(msm)


source("Recovery/02_simulate-DIFF-linear.R")
library(lhs)


nParams <- 5
model <- "v-linear"


# When in the same working directory as the C code, this line will compile the code (first time usage, like installing a package)
# system("R CMD SHLIB DIFF.c") # <--- Make sure to run this first before trying to source

# Set up hypercube

use.LHS <- randomLHS(n = 100, k = nParams) # = n params


colnames(use.LHS) <- c(
"v.b","v.c", "a", "t0", "z"
)

use.range <- array(NA, c(nParams, 2)) # c(nparams, 2)

rownames(use.range) <- colnames(use.LHS)
colnames(use.range) <- c("Min", "Max")


# Define ranges for parameters for hypercube sampling

use.range["v.c", ] <- c(0.001, 3)
use.range["v.b", ] <- c(0.0001, 0.005)
use.range["a", ] <- c(0.1, 4)
use.range["t0", ] <- c(0.1, 0.6)
use.range["z", ] <- c(0.3, 0.7)

for (useParam in colnames(use.LHS)) {
  use.LHS[, useParam] <- use.range[useParam, "Min"] +
    use.LHS[, useParam] * (use.range[useParam, "Max"] - use.range[useParam, "Min"])
}

# define the max value of the parameter. Because linear functions will just increase/decrease 
# forever, we want to choose a combination of parameters that won't result in a maximum/min
# value that is too extreme. 

max_value <- 4

linear_fun = function(x, row) {
  v = x["v.b"] * 1:1000 + x["v.c"]
  v
}

# for each row of use.LHS, find the max value of the linear function
largest_param <- apply(use.LHS, 1, function(x) max(linear_fun(x)))

# if largest param is greater than 4, then resample use.LHS for that row
while (any(largest_param > max_value)) {
  # find the rows that need to be resampled
  resample_rows <- which(largest_param > max_value)
  # resample those rows
  use.LHS[resample_rows, ] <- randomLHS(n = length(resample_rows), k = nParams)
  # rescale the resampled rows
  for (useParam in colnames(use.LHS)) {
    use.LHS[resample_rows, useParam] <- use.range[useParam, "Min"] +
      use.LHS[resample_rows, useParam] * (use.range[useParam, "Max"] - use.range[useParam, "Min"])
  }
  # recalculate the largest param
  largest_param <- apply(use.LHS, 1, function(x) max(linear_fun(x)))
}


conds <- c(1)



# Simulate all data sets

for (i in 1:nrow(use.LHS)) {
  cat("\n", i, "/", nrow(use.LHS))

  # Set up simulated data storage
  data <- list(time = NULL, Resp = NULL, Cond = NULL, Trial = NULL)

  # Set up generating parameters storage
  genParams <- array(NA, c(nParams + 4, length(conds)), dimnames = list(c("z", "a", "ter", "v.c", "v.b", "stoch.s", "sz", "sv", "ster"), conds))

  # Loop over conditions
  for (cond in conds) {
    # Define generating parameters for this condition
    genParams[, paste(cond)] <- c(
      as.numeric(use.LHS[i, "z"]),
      as.numeric(use.LHS[i, "a"]),
      as.numeric(use.LHS[i, "t0"]),
      as.numeric(use.LHS[i, "v.c"]),
      as.numeric(use.LHS[i, "v.b"]),
      1, 0, 0, 0
    )

    N <- 1000
    # Actually simulate
    tmp <- simulate.DIFF(
      N = N, params = genParams[, paste(cond)], maxCounter = 50000, stepSize = 0.001,
      varyV = T, varyA = F, varyZ = F, varyT0 = F, use.table = use.table, n.table.options = n.table.options
    )

    # Store sim data
    data$time <- c(data$time, tmp$rt)
    data$Resp <- c(data$Resp, tmp$resp)
    data$Cond <- c(data$Cond, rep(cond, length(tmp$rt)))
    data$Trial <- c(data$Trial, 1:N)
  }

  # Save sim data
  save(file = paste("Recovery/", model, "/Datasets/RECOVERY_DATA-DIFF_LHS-", i, ".Rdata", sep = ""), data, genParams, conds)
}
