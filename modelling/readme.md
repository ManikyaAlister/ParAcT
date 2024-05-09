Due to the high computational cost of running these models, I have set the models up to run on my university's high performance computer (HPC). Each run runs for a different participant, model, and data set. To run a model locally, you can run it from the terminal defining the participant, dataset, and model as follows: 

``Rscript run-paract-DDM.R <participant> <dataset id> <model>``

All of the other scripts in this folder are sourced by ``run-paract-DDM.R`` at some point. To transfer this framework to another project, there are three main things that need to be updated: 

1. define-dataset-details.R - This script defines key details pertaining to the data sets that are being fit, including file paths. 

2. time-varying-functions.R - This script defines the ParAcT functions for individual parameters that are used in the models. You only need to update this if you want to run different time varying functions. 

3. model-functions.R - This script defines the models that are being fit. You only need to update this if you want to fit different models.

If you define new functions that have new parameters, you will also need to update priors.R to include the new parameters. This will include adding a start.point, start.point.sd, lower.bounds, upper.bounds, means, and sds for the new parameters.

As you can see, each data set has a folder within "modelling" which has a number of data set specific folders and files. 

!! IMPORTANT !!

Make sure that there are "06_output" and "08_model-prediction" folders for each data set. Sorry about the strange numbering, it comes from an older, terrible version of the directory that I have since cleaned up.  This is the same as the "round-1" directory that comes after the data set. If you make your own set up with new data sets, this isn't necessary and you can remove the need for both of these (redundant numbering and round-1 file) by updating the file paths in "define-dataset-details.R". 