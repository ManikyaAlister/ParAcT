rm(list = ls())

# library(here)
# setwd("Recovery")
library(msm)


source("Recovery/02_simulate-DIFF-linear.R")
library(lhs)


nParams <- 5
model <- "a-linear"


# When in the same working directory as the C code, this line will compile the code (first time usage, like installing a package)
# system("R CMD SHLIB DIFF.c") # <--- Make sure to run this first before trying to source

# Set up hypercube

use.LHS <- randomLHS(n = 100, k = nParams) # = n params


colnames(use.LHS) <- c(
"a.b","a.c", "v", "t0", "z"
)

use.range <- array(NA, c(nParams, 2)) # c(nparams, 2)

rownames(use.range) <- colnames(use.LHS)
colnames(use.range) <- c("Min", "Max")


# Define ranges for parameters for hypercube sampling

use.range["a.c", ] <- c(0.5, 5)
use.range["a.b", ] <- c(0.001, 0.03)
use.range["v", ] <- c(0.1, 4)
use.range["t0", ] <- c(0.1, 0.6)
use.range["z", ] <- c(0.3, 0.7)

for (useParam in colnames(use.LHS)) {
  use.LHS[, useParam] <- use.range[useParam, "Min"] +
    use.LHS[, useParam] * (use.range[useParam, "Max"] - use.range[useParam, "Min"])
}

# define the min value of the parameter. Because linear functions will just increase/decrease 
# forever, we want to choose a combination of parameters that won't result in a maximum/min
# value that is too extreme. 

min_value <- 0

linear_fun = function(x, row) {
  a = x["a.b"] * 1:1000 + x["a.c"]
  a
}

# for each row of use.LHS, find the max value of the linear function
smallest_param <- apply(use.LHS, 1, function(x) min(linear_fun(x)))

# if smallest param is less than 0, then re sample use.LHS for that row
while (any(smallest_param < min_value)) {
  # find the rows that need to be resampled
  resample_rows <- which(smallest_param < min_value)
  # resample those rows
  use.LHS[resample_rows, ] <- randomLHS(n = length(resample_rows), k = nParams)
  # rescale the resampled rows
  for (useParam in colnames(use.LHS)) {
    use.LHS[resample_rows, useParam] <- use.range[useParam, "Min"] +
      use.LHS[resample_rows, useParam] * (use.range[useParam, "Max"] - use.range[useParam, "Min"])
  }
  # recalculate the largest param
  smallest_param <- apply(use.LHS, 1, function(x) max(linear_fun(x)))
}


conds <- c(1)



# Simulate all data sets

for (i in 1:nrow(use.LHS)) {
  cat("\n", i, "/", nrow(use.LHS))

  # Set up simulated data storage
  data <- list(time = NULL, Resp = NULL, Cond = NULL, Trial = NULL)

  # Set up generating parameters storage
  genParams <- array(NA, c(nParams + 4, length(conds)), dimnames = list(c("z", "v", "ter", "a.c", "a.b", "stoch.s", "sz", "sv", "ster"), conds))

  # Loop over conditions
  for (cond in conds) {
    # Define generating parameters for this condition
    genParams[, paste(cond)] <- c(
      as.numeric(use.LHS[i, "z"]),
      as.numeric(use.LHS[i, "v"]),
      as.numeric(use.LHS[i, "t0"]),
      as.numeric(use.LHS[i, "a.c"]),
      as.numeric(use.LHS[i, "a.b"]),
      1, 0, 0, 0
    )

    N <- 1000
    # Actually simulate
    tmp <- simulate.DIFF(
      N = N, params = genParams[, paste(cond)], maxCounter = 20000, stepSize = 0.001,
      varyV = F, varyA = T, varyZ = F, varyT0 = F, use.table = use.table, n.table.options = n.table.options
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
