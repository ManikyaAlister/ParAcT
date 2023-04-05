#!/bin/bash

cd /data/gpfs/projects/punim1751/ParAcT/modelling/chenetal-18/00_slurm-scripts

sbatch  sbatch-script-1.2_a.txt
sbatch  sbatch-script-1.3_a.txt
sbatch  sbatch-script-1.4_a.txt
sbatch  sbatch-script-2.2_v.txt
sbatch  sbatch-script-2.3_v.txt
sbatch  sbatch-script-2.4_v.txt

