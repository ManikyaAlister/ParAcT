lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/evansetal-17/normal/round-2/12_param-plots/param-plot-functions.R"))
n = c(1,2,3,4,6) # participants who you want to plot 
n = 1:10
# Drift rate

driftPlot("delayed-exp", n)