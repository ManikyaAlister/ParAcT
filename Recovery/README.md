The basic structure of a parameter recovery is that you 1. simulate a data set 
based off a sample of "generating" parameters, and then 2. fit the model to that
data set and see whether the estimated parameters are the same as the generating
parameters. 

Scripts 1-4 pertain to step 1. They generate simulated data sets from randomly 
sampled parameter values within set ranges. 

01 - Allows you to define the sampling ranges of parameters in the models you will
be recovering from. If any additional models with new parameters are added, this
needs to be updated. 

02 - This is a background function that simulates the diffusion model. 

03 - This has the bulk of the code that runs the recovery and calls 02. 

04 - This is the script you actually run in order to run the simulation, providing
the parameter ranges have been properly defined. 


Scripts 5-7 pertain to step 2. They fit models onto the generated data. A lot of 
the code that these scripts source come from modelling/generic-scripts. 

05 - Defines file paths. These need to be up to date for the fit to work properly. 

06 - Allows you to define the parameters of the run (model, dataset, etc.). 

07 - Run this script to fit the model given the defined run parameters. 

08 - A function for plotting the correlation between generated and estimated parameters. 

The "DIFF_TrialVarying" files are c files that help speed up the simulation of data. 