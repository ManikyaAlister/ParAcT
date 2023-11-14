lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/evansetal-17/optim/round-2/12_param-plots/param-plot-functions.R"))
n = 1 # participants who you want to plot 

# Drift rate
n = c(5,9)
v_linear = driftPlot("v-linear",n, complex = TRUE, mean = TRUE)
v_linear
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-linear.png"), plot = v_linear)

n = c(2,7)
v_dExp = driftPlot("v-delayed-exp",n, round = 1, complex = TRUE, mean = TRUE)
v_dExp
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-dExp.png"), plot = v_dExp)


n = c(3,4,6,8)
v_exp = driftPlot("v-exp",n, round = 1,complex = TRUE, mean = TRUE)
v_exp
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-exp.png"), plot = v_exp)


# Threshold
n = c(2,4, 5, 6, 7, 8)
a_exp = thresholdPlot("a-exp",n, complex = TRUE, mean = TRUE)
a_exp
ggsave(filename = here("modelling/evansetal-17/optim/round-1/09_plots/a-exp.png"),plot = a_exp)

linear = thresholdPlot("a-linear",n, complex = TRUE, mean = TRUE)
ggsave(filename = here("modelling/evansetal-17/optim/round-1/09_plots/a-linear.png"),plot = linear)

n = 9
simple = thresholdPlot("simple",n)
simple
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/a-simple.png"),plot = simple)

n = 1
thresholdPlot("a-step-fixed",n, complex = TRUE, mean = TRUE)

a_exp = thresholdPlot("a-exp",n)
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/a-exp-mir.png"), plot = a_exp_mir)

a_delayed_pow = thresholdPlot("a-delayed-power",n)

a_delayed_exp = thresholdPlot("a-delayed-exp",n, complex = TRUE)
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/a_delayed-exp.png"), plot = a_delayed_exp)
