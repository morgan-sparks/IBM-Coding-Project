#!/bin/bash
#SBATCH --job-name=Pink_Salmon_IBM_test
#SBATCH -A beagle
#SBATCH -N 1
#SBATCH -n 10
#SBATCH --mem=20G
#SBATCH -t 200:00:00
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=sparks35@purdue.edu

module unload openmpi
module purge

module load gcc
module load r

R --no-save  < /scratch/bell/sparks35/IBM-Coding-Project/Pink\ Salmon/model\ v.1/lake\ population\ v1.R

module purge
