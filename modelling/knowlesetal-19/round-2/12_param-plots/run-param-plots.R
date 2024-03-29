rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
source(here("modelling/knowlesetal-19/round-2/12_param-plots/param-plot-functions.R"))

v_models <- c(
  # "simple",
  "v-linear",
  # "v-power",
  "v-exp",
  # "v-delayed-pow",
  "v-delayed-exp",
  "v-blocked-simple",
  # "v-blocked-complex",  # only including complex blocked models as a sanity check, not in model comparison
  # "v-blocked-exp-ul",
  "v-delayed-exp-blocked",
  "v-blocked-exp-sb"
)

a_models <- c(  "a-linear",
                # "a-power",
                "a-exp",
                # "a-delayed-power",
                "a-delayed-exp",
                "a-blocked-simple",
                # "a-blocked-complex", # only including complex blocked models as a sanity check, not in model comparisons
                "a-delayed-exp-blocked",
                "a-blocked-exp-sb"
                # "a-blocked-exp-ul",
                #"a-step"
)

models_2p <- c(
  "v-a-exp",
  "v-linear-a-exp",
  "v-linear-a-blocked-simple",
  "v-dExp-a-Exp",
  "v-exp-a-dExp-blocked",
  "v-linear-a-dExp",
  "v-dExp-blocked-a-blocked-simple",
  "v-dExp-blocked+a-dExp" 
)

models <- c("simple", a_models, v_models, models_2p)

load(here("data/knowlesetal-19/derived/best_BIC"))
load(here("data/knowlesetal-19/derived/best_AIC"))


load(here("data/knowlesetal-19/derived/best_a_1p_BIC"))
load(here("data/knowlesetal-19/derived/best_v_1p_BIC"))

# figure out the most common models
table(best_v)
table(best_a)


# Drift rate

n = which(best_BIC %in% a_models | best_BIC == "simple")
#n = grep("simple", best_v)
v_simple = driftPlot("simple",n, mean = TRUE, complex = TRUE, title = "Standard DDM")
v_simple
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/v-simple.png"),plot = simple)

#n = grep("v-linear", best_v)
n = grep("v-linear|v-a-linear", best_BIC)
v_linear = driftPlot("v-linear",n, complex = TRUE, mean = TRUE, title = "Linear")
v_linear
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/v-linear.png"), plot = v_linear)

#n = grep("v-exp", best_v)
n = grep("v-a-exp|v-exp", best_BIC)
v_exp = driftPlot("v-exp",n, complex = TRUE, mean = TRUE, title = "Exponential")
v_exp
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/v-exp.png"), plot = v_exp, width = 4, height = 4)

n = grep("v-delayed-exp", best_BIC)
v_delayed_exp <- driftPlot("v-delayed-exp",n, complex = TRUE, mean = TRUE)
v_delayed_exp
# Threshold

n = grep("a-exp|v-a-exp", best_BIC)
a_exp = thresholdPlot("a-exp",n, complex = TRUE, mean = TRUE)
a_exp
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-exp.png"),plot = exp)

n = grep("a-linear|v-a-linear", best_BIC)
a_linear = thresholdPlot("a-linear",n, complex = TRUE, round = 1, mean = TRUE)
a_linear
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-linear.png"),plot = linear)

n = which(best_BIC %in% v_models | best_BIC == "simple")
a_simple = thresholdPlot("simple",n, complex = TRUE, round = 1, mean = TRUE)
a_simple
ggsave(filename = here("modelling/knowlesetal-19/round-2/09_plots/a-simple.png"),plot = linear)


# manuscript figure
library(ggpubr)
combined <- ggarrange(a_exp, a_linear, a_simple, v_exp, v_linear, v_simple, nrow = 2, ncol = 3)
combined
ggsave(filename = here("man-figures/param-plot-knowles.pdf"), plot = combined, width = 15, height = 10)
