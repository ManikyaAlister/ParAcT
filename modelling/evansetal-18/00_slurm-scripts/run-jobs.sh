#!/bin/bash

cd /data/gpfs/projects/punim1751/ParAcT/modelling/evansetal-18/00_slurm-scripts

sbatch  sbatch-script-1.2_a-linear.txt
sbatch  sbatch-script-1.3_a-pow.txt
sbatch  sbatch-script-1.4_a-exp.txt
sbatch  sbatch-script-2.4_v-exp.txt
sbatch  sbatch-script-1.5_a-delayed-exp.txt
sbatch  sbatch-script-1.6_a-delayed-pow.txt
sbatch  sbatch-script-2.6_v-delayed-exp.txt
sbatch  sbatch-script-2.5_v-delayed-pow.txt
sbatch  sbatch-script-3.1_v-a-exp.txt
