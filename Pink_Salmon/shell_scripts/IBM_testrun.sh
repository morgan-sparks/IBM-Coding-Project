#!/bin/bash
#SBATCH --job-name=Pink_Salmon_IBM_test
#SBATCH -A beagle
#SBATCH -N 1
#SBATCH -n 20
#SBATCH --mem=40G
#SBATCH -t 300:00:00
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=sparks35@purdue.edu

module load gcc
module load r

R --no-save  < /scratch/bell/sparks35/IBM-Coding-Project/Pink_Salmon/model_v.1/lake_population_v1.R

module purge
