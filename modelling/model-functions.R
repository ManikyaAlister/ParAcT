library(here)
source(here("modelling/time-varying-functions.R"))
# some models here are not included in the manuscript
all_functions <- list(
  "simple" = list(
    a = a_standard,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "Standard DDM"
  ),
  "a-linear" = list(
    a = a_linear,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Linear"
    
  ),
  "v-linear" = list(
    a = a_standard,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Linear"
  ),
  "a-exp" = list(
    a = a_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exponential"
  ),
  "v-exp" = list(
    a = a_standard,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Exponential"
  ),
  "a-dExp" = list(
    a = a_dExp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Delayed Exp"
  ),
  "v-dExp" = list(
    a = a_standard,
    v = v_dExp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Delayed Exp"
  ),
  "a-power" = list(
    a = a_power,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Power"
  ),
  "v-power" = list(
    a = a_standard,
    v = v_power,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Power"
  ),
  "a-linear-blocked" = list(
    a = a_linear_blocked,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Linear Blocked"
  ),

# Block-varying models ----------------------------------------------------

  "v-linear-blocked" = list(
    a = a_standard,
    v = v_linear_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Linear Blocked"
  ),
  "a-exp-blocked" = list(
    a = a_exp_blocked,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp Blocked"
  ),
  "v-exp-blocked" = list(
    a = a_standard,
    v = v_exp_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Exp Blocked"
  ),
  "a-dExp-blocked" = list(
    a = a_dExp_blocked,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Delayed Exp Blocked"
  ),
  "v-dExp-blocked" = list(
    a = a_standard,
    v = v_dExp_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "v Delayed Exp Blocked"
  ),  
"a-step-fixed" = list(
    a = a_step_fixed,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Step"
  ),
"v-step-fixed" = list(
  a = a_standard,
  v = v_step_fixed,
  z = z_standard,
  t0 = t0_standard,
  blocked_likelihood = FALSE,
  full_name = "v Step"
),

# Blocked likelihood models -----------------------------------------------
  "a-block-trial-exp" = list(
    a = a_block_trial_exp,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "a Exp Trial with Block Bump"
  ),
  "v-block-trial-exp" = list(
    a = a_standard,
    v = v_block_trial_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "v Exp Trial with Block Bump"
  ),
  "a-blocked-complex" = list(
    a = a_blocked_complex,
    v = v_standard,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "a Blocked Complex"
  ),
  "v-blocked-complex" = list(
    a = a_standard, 
    v = v_blocked_complex, 
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "v Blocked Complex"
  ),

# Two parameter models ----------------------------------------------------
# 2 param models to include: 
#      [,1]                               
# [1,] "a-step-fixed+v-step-fixed"        
# [2,] "a-exp+v-exp"                      
# [3,] "a-exp+v-linear-blocked"           
# [4,] "a-exp+v-step-fixed"               
# [5,] "a-dExp-blocked+v-linear-blocked"  
# [6,] "a-exp-blocked+v-linear-blocked"   
# [7,] "a-linear-blocked+v-linear-blocked"
# [8,] "a-dExp+v-dExp"  
  "a-step-fixed+v-step-fixed" = list(
    a = a_step_fixed,
    v = v_step_fixed,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "a Step + v Step"
  ),
  "a-exp+v-exp" = list(
    a = a_exp,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp + v Exp"
  ),
  "a-exp+v-linear-blocked" = list(
    a = a_exp,
    v = v_linear_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp + v Linear Blocked"
  ),
  "a-exp+v-step-fixed" = list(
    a = a_exp,
    v = v_step_fixed,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "a Exp + v Step"
  ),
  "a-dExp-blocked+v-linear-blocked" = list(
    a = a_dExp_blocked,
    v = v_linear_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Delayed Exp Blocked + v Linear Blocked"
  ),
  "a-exp-blocked+v-linear-blocked" = list(
    a = a_exp_blocked,
    v = v_linear_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp Blocked + v Linear Blocked"
  ),
  "a-linear-blocked+v-linear-blocked" = list(
    a = a_linear_blocked,
    v = v_linear_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Linear Blocked + v Linear Blocked"
  ),
  "a-dExp+v-dExp" = list(
    a = a_dExp,
    v = v_dExp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Delayed Exp + v Delayed Exp"
  ),
  "a-linear-blocked+v-linear" = list(
    a = a_linear_blocked,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Linear Blocked + v Linear"
  ),
  "a-block-trial-exp+v-exp" = list(
    a = a_block_trial_exp,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "a Exp Trial with Block Bump + v Exp"
  ),
  # Knowles models ----------------------------------------------------
  "a-exp+v-exp" = list(
    a = a_exp,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp + v Exp"
  ),
  "a-exp+v-linear" = list(
    a = a_exp,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp + v Linear"
  ),
  "a-exp+v-linear-blocked" = list(
    a = a_exp,
    v = v_linear_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp + v Linear Blocked"
  ),
  "a-linear-blocked+v-linear" = list(
    a = a_linear_blocked,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Linear Blocked + v Linear"
  ),
  "a-linear+v-linear" = list(
    a = a_linear,
    v = v_linear,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Linear + v Linear"
  ),
  "a-exp-blocked+v-exp-blocked" = list(
    a = a_exp, 
    v = v_exp_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp Blocked + v Exp Blocked"
  ),
  "a-dExp-blocked+v-exp" = list(
    a = a_dExp_blocked,
    v = v_exp,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Delayed Exp Blocked + v Exp"
  ),
  "a-exp+v-exp-blocked" = list(
    a = a_exp,
    v = v_exp_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Exp + v Exp Blocked"
  ), "a-dExp-blocked+v-exp-blocked" = list(
    a = a_dExp_blocked,
    v = v_exp_blocked,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = FALSE,
    full_name = "a Delayed Exp Blocked + v Exp Blocked"
  ), "a-exp-blocked+v-step-fixed" = list(
    a = a_exp_blocked,
    v = v_step_fixed,
    z = z_standard,
    t0 = t0_standard,
    blocked_likelihood = TRUE,
    full_name = "a Exp Blocked + v Step"
)
)