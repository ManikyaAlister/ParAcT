library(here)
source(here("modelling/generic_scripts/time-varying-functions.R"))
all_functions <- list(
  "simple" = list(
    a = a_standard,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-linear" = list(
    a = a_linear,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
    
  ),
  "v-linear" = list(
    a = a_standard,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-exp" = list(
    a = a_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "v-exp" = list(
    a = a_standard,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-dExp" = list(
    a = a_dExp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "v-dExp" = list(
    a = a_standard,
    v = v_dExp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-power" = list(
    a = a_power,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "v-power" = list(
    a = a_standard,
    v = v_power,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-linear-blocked" = list(
    a = a_linear,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "v-linear-blocked" = list(
    a = a_standard,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-exp-blocked" = list(
    a = a_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "v-exp-blocked" = list(
    a = a_standard,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-dExp-blocked" = list(
    a = a_dExp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "v-dExp-blocked" = list(
    a = a_standard,
    v = v_dExp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE
  ),
  "a-step-fixed" = list(
    a = a_step_fixed,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE
  ),
  "v-step-fixed" = list(
    a = a_standard,
    v = v_step_fixed,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE
  ),
  "a-block-trial-exp" = list(
    a = a_block_trial_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE
  ),
  "v-block-trial-exp" = list(
    a = a_standard,
    v = v_block_trial_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE
  ),
  "a-blocked-complex" = list(
    a = a_blocked_complex,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE
  ),
  "v-blocked-complex" = list(
    a = a_standard, 
    v = v_blocked_complex, 
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE
  )
)
