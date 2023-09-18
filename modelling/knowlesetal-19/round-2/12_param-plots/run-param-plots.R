lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/knowlesetal-19/round-2/12_param-plots/param-plot-functions.R"))
n = 1:10 # participants who you want to plot 

# Drift rate

v_power = driftPlot("v-power",n)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/v-power.png"), plot = v_power)
n = c(7)
v_linear = driftPlot("v-linear",n, complex = TRUE, mean = TRUE)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/v-linear.png"), plot = v_linear)
n = c(2,3,4,6)
v_exp = driftPlot("v-exp",n, complex = TRUE, mean = TRUE)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/v-exp.png"), plot = v_exp)

# Threshold

power = thresholdPlot("a-power",n)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-pow.png"),plot = power)


exp = thresholdPlot("a-exp",n, complex = TRUE, mean = TRUE)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-exp.png"),plot = exp)

thresholdPlot("v-linear-a-exp",n, complex = TRUE, mean = TRUE, round = 2)
n = c(7)
linear = thresholdPlot("a-linear",n, complex = TRUE, round = 1, mean = TRUE)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-linear.png"),plot = linear)

simple = thresholdPlot("simple",n)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-simple.png"),plot = simple)

a_exp_mir = thresholdPlot("a-exp-mir",n)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-exp-mir.png"), plot = a_exp_mir)

a_delayed_pow = thresholdPlot("a-delayed-power",n)

a_delayed_exp = thresholdPlot("a-delayed-exp",n)
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a_delayed-exp.png"), plot = a_delayed_exp)

# Participants who showed exponential change for a

load(here("data/knowlesetal-19/derived/best_BIC"))
a_exp_p <- grep("a-exp", best_BIC)
thresholdPlot("a-exp", a_exp_p)

