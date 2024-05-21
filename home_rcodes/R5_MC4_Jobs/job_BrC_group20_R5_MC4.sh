#!/bin/bash
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 0-02:30:00
#SBATCH -p shared
#SBATCH --mem=40G
#SBATCH -c 2
#SBATCH -J BrC_group20
#SBATCH -o R5_MC4_Jobs/BrC_group20.out
#SBATCH -e R5_MC4_Jobs/BrC_group20.err
#SBATCH --mail-user=oliviashen953@gmail.com

module load Mambaforge/23.11.0-fasrc01
cd /n/home00/jiaxinshen/mc4-tmp/MC4-1
conda activate my_env

python mc4/command_line.py test_datasets/RANK_transposed_EUR_R5_BrC_group20.csv -o column -hr 0
conda deactivate
