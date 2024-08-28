#!/bin/bash
#SBATCH --ntasks=5
#SBATCH --time=00:30:00
#SBATCH --job-name=cilength

module load math/R/4.1.2

R --no-save < sim_cilength.R > Simulation.out