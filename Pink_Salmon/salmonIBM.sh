#!/bin/bash
#SBATCH --job-name=salmonIBN
#SBATCH -A beagle
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem=6G
#SBATCH -t 300:00:00



module load gcc
module load r

Rscript lake_population_v2.R