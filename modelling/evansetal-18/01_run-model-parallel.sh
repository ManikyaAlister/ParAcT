# This script runs seperate parallel jobs for each participant. For each value 
# in the loop, it splits off another parallel run of the model where the 
# subject corresponds to ii in the loop. 

for ii in $(seq 1 9)
  do
  Rscript modelling/evansetal-18/05_run-models/5.1.1-simple.R $ii &
  done
