library(here)
source(here("modelling/evansetal-18/08_param-plot-functions.R"))
n = 1:9 # participants who you want to plot 

# Drift rate

v_power = driftPlot("v-power",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/v-power.png"), plot = v_power)

v_linear = driftPlot("v-linear",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/v-linear.png"), plot = v_linear)

v_exp = driftPlot("v-exp",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/v-exp.png"), plot = v_exp)

# Threshold

power = thresholdPlot("a-power",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-pow.png"),plot = power)

exp = thresholdPlot("a-exp",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-exp.png"),plot = exp)

linear = thresholdPlot("a-linear",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-linear.png"),plot = linear)

simple = thresholdPlot("simple",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-simple.png"),plot = simple)

a_exp_mir = thresholdPlot("a-exp-mir",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a-exp-mir.png"), plot = a_exp_mir)

a_delayed_pow = thresholdPlot("a-delayed-power",n)

a_delayed_exp = thresholdPlot("a-delayed-exp",n)
ggsave(filename = here("modelling/evansetal-18/09_plots/a_delayed-exp.png"), plot = a_delayed_exp)
