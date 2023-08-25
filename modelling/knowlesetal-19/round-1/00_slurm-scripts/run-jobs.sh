#!/bin/bash

cd /Users/manikyaalister/Documents/Projects/ParAcT/modelling/knowlesetal-19/round-1/

for ((i=1; i<=7; i++))
do
    
    echo "Running participant $i"
    Rscript 05_run-models/5.2.4_v-exp.R $i &
done

wait