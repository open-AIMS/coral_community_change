#!/bin/bash
#SBATCH --job-name=comm_change
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=62
#SBATCH --partition=mlogan
#SBATCH --mem=600GB
#SBATCH --output=analysis_%j.log
#SBATCH --mail-user=m.logan@aims.gov.au
#SBATCH --mail-type=ALL

module load singularity
singularity exec -B .:/home/Project ltmp_dashboard1.sif make run_R
                            