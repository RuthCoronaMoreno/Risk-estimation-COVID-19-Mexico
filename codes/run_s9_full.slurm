#!/bin/bash
#
# Nombre del trabajo
#SBATCH --job-name=s9
#
# Directorio de trabajo actual
#SBATCH --chdir=.
#
# Unir stdout y stderr
#SBATCH --partition=defq
#SBATCH --output=s9_%j.log
#SBATCH --error=s9_%j.log
#
# Correo al finalizar
#SBATCH --mail-type=END
#SBATCH --mail-user=ruth.corona.m@im.unam.mx
#
# Recursos 
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --mem=16G
#SBATCH --time=120:00:00

source /etc/profile

#Cargar módulos
module load r/4.0.2

#Ejecutar script de R
Rscript 3_covidestim_state9.R
