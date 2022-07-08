rm(lists = ls())
library(here)
library(ggplot2)

n = 71
nTrials = 1536
trials = 1:nTrials

# Power model

thresholdPow = as.data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:n) {
  load(here(
    paste("modelling/xuetal-18/06_output/P", i, "pow5.Rdata", sep = "")
  ))
  params =  apply(theta, 2, mean)
  threshold = as.data.frame(params["a.asym"] + params["a.start"] * trials ^
                              params["a.rate"])
  threshold$Participant =  i
  threshold$Trial = trials
  thresholdPow  = rbind(thresholdPow, threshold)
}

colnames(thresholdPow) = c("Threshold", "Participant", "Trial")

ggplot(data = thresholdPow) +
  geom_line(aes(x = Trial, y = Threshold, group = Participant)) +
  theme_classic()

# Linear model

thresholdLin = as.data.frame(matrix(nrow = 0, ncol = 3))
colnames(thresholdLin) = c("Threshold", "Participant", "Trial")

for (i in 1:n) {
  load(here(
    paste("modelling/xuetal-18/06_output/P", i, "linear.Rdata", sep = "")
  ))
  params =  apply(theta, 2, mean)
  threshold = as.data.frame(params["a.c"] + params["a.b"] * trials)
  threshold$Participant =  i
  threshold$Trial = trials
  thresholdLin  = rbind(thresholdLin, threshold)
}

colnames(thresholdLin) = c("Threshold", "Participant", "Trial")

ggplot(data = thresholdLin) +
  geom_line(aes(x = Trial, y = Threshold, group = Participant)) +
  theme_classic()
            