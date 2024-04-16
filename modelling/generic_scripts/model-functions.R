library(here)
source(here("modelling/generic_scripts/time-varying-functions.R"))
all_functions <- list(
  "simple" = list(
    a = a_standard,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-linear" = list(
    a = a_linear,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-linear" = list(
    a = a_standard,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-exp" = list(
    a = a_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-exp" = list(
    a = a_standard,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-dExp" = list(
    a = a_dExp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-dExp" = list(
    a = a_standard,
    v = v_dExp,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-power" = list(
    a = a_power,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-power" = list(
    a = a_standard,
    v = v_power,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-linear-blocked" = list(
    a = a_linear,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-linear-blocked" = list(
    a = a_standard,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-exp-blocked" = list(
    a = a_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-exp-blocked" = list(
    a = a_standard,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard
  ),
  "a-dExp-blocked" = list(
    a = a_dExp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard
  ),
  "v-dExp-blocked" = list(
    a = a_standard,
    v = v_dExp,
    z = z_standard,
    t0 = t0_standard
  )
)
