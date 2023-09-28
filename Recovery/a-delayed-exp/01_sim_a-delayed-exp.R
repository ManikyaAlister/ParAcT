rm(list = ls())

# library(here)
# setwd("Recovery")
library(msm)


source("Recovery/02_simulate-DIFF-exp.R")
library(lhs)


nParams <- 7
model <- "a-delayed-exp"


# When in the same working directory as the C code, this line will compile the code (first time usage, like installing a package)
# system("R CMD SHLIB DIFF.c") # <--- Make sure to run this first before trying to source

# Set up hypercube

use.LHS <- randomLHS(n = 100, k = nParams) # = n params


colnames(use.LHS) <- c(
  "a.start", "a.asym", "a.rate", "a.delay", # v. center/change params? Maybe not because we're not looking at diffs across conditions
  "v", "t0", "z"
)

use.range <- array(NA, c(nParams, 2)) # c(nparams, 2)

rownames(use.range) <- colnames(use.LHS)
colnames(use.range) <- c("Min", "Max")


# Define ranges for parameters for hypercube sampling

use.range["a.start", ] <- c(0, 3)
use.range["a.asym", ] <- c(0.45, 2)
use.range["a.rate", ] <- c(0.001, 0.3)
use.range["a.delay", ] <- c(0.1, 30)
use.range["v", ] <- c(0.1, 4)
use.range["t0", ] <- c(0.1, 0.6)
use.range["z", ] <- c(0.3, 0.7)

for (useParam in colnames(use.LHS)) {
  use.LHS[, useParam] <- use.range[useParam, "Min"] +
    use.LHS[, useParam] * (use.range[useParam, "Max"] - use.range[useParam, "Min"])
}


conds <- c(1)



# Simulate all data sets

for (i in 1:nrow(use.LHS)) {
  cat("\n", i, "/", nrow(use.LHS))

  # Set up simulated data storage
  data <- list(time = NULL, Resp = NULL, Cond = NULL, Trial = NULL)

  # Set up generating parameters storage
  genParams <- array(NA, c(nParams + 4, length(conds)), dimnames = list(c("z", "v", "ter", "a.start", "a.asym", "a.rate", "a.delay", "stoch.s", "sz", "sv", "ster"), conds))

  # Loop over conditions
  for (cond in conds) {
    # Define generating parameters for this condition
    genParams[, paste(cond)] <- c(
      as.numeric(use.LHS[i, "z"]),
      as.numeric(use.LHS[i, "v"]),
      as.numeric(use.LHS[i, "t0"]),
      as.numeric(use.LHS[i, "a.start"]),
      as.numeric(use.LHS[i, "a.asym"]),
      as.numeric(use.LHS[i, "a.rate"]),
      as.numeric(use.LHS[i, "a.delay"]),
      1, 0, 0, 0
    )

    N <- 1000
    # Actually simulate
    tmp <- simulate.DIFF(
      N = N, params = genParams[, paste(cond)], maxCounter = 10000, stepSize = 0.001,
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
