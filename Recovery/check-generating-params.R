source(here("modelling/generic_scripts/time-varying-functions.R"))


# Check generating parameter ranges ---------------------------------------
checkGenParams = function(model_name, paract_function){
  load(here(paste0("Recovery/",model_name,"/Datasets/RECOVERY_DATA-DIFF_LHS-1.Rdata")))
  params = genParams[,1]
  trials = 1:1000
  paract <- paract_function(params, trial = trials)
  plot(trials, paract, "l", ylim = c(0,7))
  
  for (i in 2:100){
    load(here(paste0("Recovery/",model_name,"/Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata")))
    params =  genParams[,1]
    paract <- paract_function(params, trials = trials)
    lines(trials, paract)
  }
}

# linear
checkGenParams("a-linear", a_linear)
checkGenParams("v-linear", v_linear)

# power
checkGenParams("a-power", a_power)
checkGenParams("v-power", v_power)

# exp
checkGenParams("a-exp", a_exp)
checkGenParams("v-exp", v_exp)

# dExp
checkGenParams("a-dExp", a_dExp)
checkGenParams("v-dExp", v_dExp)

# choose better ranges

all_params <- NULL
for (i in 1:100){
  load(here(paste0("Recovery/v-power/Fits_recovery/P",i,"-v-power.Rdata")))
  params <- apply(theta,2,mean)
  all_params <- rbind(all_params, params)
}

all_params = as.data.frame(all_params)
range(all_params$v.asym)
range(all_params$v.start)
range(all_params$v.rate)


