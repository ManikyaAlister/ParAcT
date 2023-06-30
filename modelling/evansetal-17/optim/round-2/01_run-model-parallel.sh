# This script runs seperate parallel jobs for each participant. For each value 
# in the loop, it splits off another parallel run of the model where the 
# subject corresponds to ii in the loop. 

for ii in $(seq 1 9)
  do
  Rscript modelling/evansetal-17/optim/round-2/05_run-models/5.3.1_v-a-exp.R $ii &
  done
