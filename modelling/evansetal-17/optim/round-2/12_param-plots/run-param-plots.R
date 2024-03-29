lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/evansetal-17/optim/round-2/12_param-plots/param-plot-functions.R"))
load(here("data/evansetal-17/derived/optim/best_BIC.Rdata"))
load(here("data/evansetal-17/derived/optim/best_AIC.Rdata"))


# Drift rate
n = grep("v-linear", best_BIC)
v_linear = driftPlot("v-linear",n, complex = TRUE, mean = TRUE, title = "Linear")
v_linear
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-linear.png"), plot = v_linear)

n = grep("v-dExp", best_BIC)
v_dExp = driftPlot("v-delayed-exp",n, round = 1, complex = TRUE, mean = TRUE, title = "Delayed Exponential")
v_dExp
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-dExp.png"), plot = v_dExp)


# n = grep("v-exp|v-a-exp", best_BIC)
# v_exp = driftPlot("v-exp",n, round = 1,complex = TRUE, mean = TRUE, title = "Exponential")
# v_exp
# ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-exp.png"), plot = v_exp)

n = grep("v-step|v-a-step", best_BIC)
v_step =driftPlot("v-step-fixed",n, complex = TRUE, mean = TRUE, title = "Step")
v_step
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/v-step.png"), plot = v_step)

n = c(3,6)
simple_v = driftPlot("simple",n, round = 1,complex = TRUE, mean = TRUE)
simple_v

# Threshold
n = grep("a-exp", best_BIC)
a_exp = thresholdPlot("a-exp",n, complex = TRUE, mean = TRUE, title = "Exponential")
a_exp
ggsave(filename = here("modelling/evansetal-17/optim/round-1/09_plots/a-exp.png"),plot = a_exp)

# n = grep("a-linear", best_BIC)
# linear = thresholdPlot("a-linear",n, complex = TRUE, mean = TRUE)
# linear
# ggsave(filename = here("modelling/evansetal-17/optim/round-1/09_plots/a-linear.png"),plot = linear)

n = 9
simple_a = thresholdPlot("simple",n, complex = TRUE, title= "Standard DDM")
simple_a
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/a-simple.png"),plot = simple_a)

n = grep("a-step", best_BIC)
a_step <- thresholdPlot("a-step-fixed",n, complex = TRUE, mean = TRUE, title = "Step")
a_step
ggsave(filename = here("modelling/evansetal-17/optim/round-2/09_plots/a_delayed-exp.png"), plot = a_step)

n = grep("a-blocked-simple", best_BIC)
a_blocked_simple <- thresholdPlot("a-blocked-simple", n, complex = TRUE, mean = TRUE, title = "Constant Block")
a_blocked_simple

# manuscript figure
library(ggpubr)
combined <- ggarrange(a_exp, a_step, a_blocked_simple, v_linear, v_step, simple_v, nrow = 2, ncol = 3)
combined
ggsave(filename = here("man-figures/param-plot-optim.pdf"), plot = combined, width = 15, height = 10)
