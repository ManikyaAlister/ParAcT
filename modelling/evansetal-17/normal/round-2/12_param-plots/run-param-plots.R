rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/evansetal-17/normal/round-2/12_param-plots/param-plot-functions.R"))
load(here("data/evansetal-17/derived/normal/best_BIC.Rdata"))

# Drift rate

# v_power = driftPlot("v-power",n)
# ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/v-power.png"), plot = v_power)


n = grep("v-exp|v-a-exp", best_BIC)
v_exp = driftPlot("v-exp",n, complex = TRUE, mean = TRUE, title = "Exponential")
v_exp
ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/v-exp.png"), plot = v_exp)

n = c(1,10)
simple_v = driftPlot("simple", n, complex =TRUE, mean = TRUE, title = "Standard DDM")
simple_v

n = grep("v-linear", best_BIC)
v_linear = driftPlot("v-linear",n, complex = TRUE, mean = TRUE, title = "Linear")
v_linear 
ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/v-linear.png"), plot = v_linear)


n = grep("v-blocked-simple", best_BIC)
v_blocked_simple = driftPlot("v-blocked-simple",n, complex = TRUE, mean = TRUE, title = "Constant Block")
v_blocked_simple


# Threshold

#power = thresholdPlot("a-power",n)
#ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/a-pow.png"),plot = power)


n = grep("a-exp|v-a-exp", best_BIC)
a_exp = thresholdPlot("a-exp",n, complex = TRUE, mean = TRUE, title = "Exponential")
a_exp
ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/a-exp.png"),plot = exp)

n = grep("a-linear|v-a-linear", best_BIC)
linear = thresholdPlot("a-linear",n, complex = TRUE, round = 1, mean = TRUE,title = "Linear")
linear
ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/a-linear.png"),plot = linear)

n = c(7,8,9)
a_simple = thresholdPlot("simple",n, complex = TRUE, round = 1, mean = TRUE, title = "Standard DDM")
a_simple
ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/a-simple.png"),plot = a_simple)

n = grep("a-blocked-simple", best_BIC)
a_blocked_simple = thresholdPlot("a-blocked-simple",n, complex = TRUE, round = 1, mean = TRUE, title = "Constant Block")
a_blocked_simple

# n = c(5)
# a_delayed_exp = thresholdPlot("a-delayed-exp",n, complex = TRUE, mean = TRUE, title = "Delayed Exponential")
# a_delayed_exp
# ggsave(filename = here("modelling/evansetal-17/normal/round-2/09_plots/a_delayed-exp.png"), plot = a_delayed_exp)

# n = 7
# a_delayed_exp_block = thresholdPlot("a-delayed-exp-blocked",n, complex = TRUE, mean = TRUE, title = "Delayed Exponential Block Varying")
# a_delayed_exp_block


# manuscript figure
library(ggpubr)
combined <- ggarrange(a_exp, a_blocked_simple, a_simple, v_exp, v_linear, v_blocked_simple,nrow = 2, ncol = 3)
combined
ggsave(filename = here("man-figures/param-plot-normal.pdf"), plot = combined, width = 15, height = 10)
