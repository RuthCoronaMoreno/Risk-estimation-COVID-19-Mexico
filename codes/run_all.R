# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : run_all.R
# Purpose    : Master pipeline script
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#             
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================

#---------------------------
#  Verifying data existence
#---------------------------
required_files <- c(
  "data/processed/confirmed_20-21.csv",
  "data/raw/population_mx.csv",
  "data/raw/official_traficlight_colors.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if(length(missing_files) > 0){
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}


#------------------------------
#  Libraries
#------------------------------
library(tidyverse)
library(data.table)
library(readxl)
library(fitdistrplus)
library(irr)
library(lubridate)
library(reshape2)
library(cowplot)
library(zoo)


#------------------------------
# Upload files
#------------------------------
pos_tot    <- read.csv("data/processed/confirmed_20-21.csv")
popmx      <- read.csv("data/raw/population_mx.csv")
traf_light <- read.csv("data/raw/official_traficlight_colors.csv")


# -------------------------------
# Step 1: Data processing
# -------------------------------
# If the process of the raw Mexican official COVID-19 data is required,
# uncomment the first part of *codes/1_processing_data.R* script.
# Otherwise, only the already upload files are enough to run the remaining script

lastDay <- "2021-12-31" #up to 2021-12-31

cat("Running data processing\n")
for(type in c("deaths", "incidence", "hosp")){
  for(groupBy in c("mun","states")){
    source("codes/1_processing_data.R")
  }
}
cat("Output is in *data/processed/* folder")



# -------------------------------
# Step 2: Distribution estimation
# -------------------------------
#----Parameters----
state     <-  9 #Official state code from "popmx"
lastDay   <-  "2021-01-31"
dist_name <- "sym_prg_delay" #p_sev_if_sym, p_die_if_sev, sev_prg_delay, sym_prg_delay, sym_prg_regist

cat("Running estimation of mexican a priori distributions \n")
source("codes/2_distribution_estimation.R") 

cat("Outputs in *plots/distribution_adjustment/* and *plots/distribution_histograms/* folders ")


# -----------------------------------------------
# Step 3: covidestim estimation (SLURM execution)
# -----------------------------------------------
cat(
  "State- and municipality-level covidestim analyses were executed on the LAVIS high-performance computing cluster at the National Autonomous University of Mexico (UNAM). ",
  "State-level analyses used scripts analogous to '3_covidestim_state9.R' submitted through SLURM batch scripts analogous to 'run_s9_full.slurm', ",
  "whereas municipality-level analyses used scripts analogous to '3_covidestim9_munAll.R' submitted through scripts analogous to 'run_s9_mun.slurm'. ",
  "The number 9 in these filenames corresponds to the official state code for Mexico City.",
  sep = ""
)
cat("This step is NOT executed here.\n")


# ----------------------------------
#  Step 4: merge covidestim outputs
# ----------------------------------

# Variables to extract
vars <- c("severe", "severe.lo", "severe.hi",
          "infections.lo", "infections", "infections.hi",
          "cum.incidence.lo", "cum.incidence", "cum.incidence.hi",
          "Rt.lo", "Rt", "Rt.hi")

cat("Running covidestim merge \n")
source("codes/4_covidestim_merge_mun.R")
source("codes/4_covidestim_merge_states.R")
cat("Outputs in *data/covidestim/* folder")


# ------------------------------------------
#  Step 5: reported data vs covidestim plots
# ------------------------------------------
# Requires the data generated in Step 4

#----parameter settings----
lastDay   <- as.Date("2021-12-25") #Available up to 2021-12-31
data_type <- c("incidence") #, "hosp", "Rt")

cat("Running plot covidestim vs reported_states \n")
source("codes/5_plot_covidestim_vs_reported_states.R")
#source("codes/5_plot_covidestim_vs_reported_mun.R") #It may take a long time to complete
cat("Outputs in *plots/covidestim_estimates* folder")



# ------------------------------------------
#  Step 6: Risk vs official traffic-light
# ------------------------------------------
# Requires the data generated in Step 4

#Gathering size 
k=100

cat("Running risk vs official traffic-light \n")
source("codes/6_risk_and_OfficialTrafficLight_states.R")

k=100
source("codes/6_risk_and_OfficialTrafficLight_mun.R")
cat("Outputs in *data/risk* and *plots/risk/* folders")


#---------------
#-----Finish----
#---------------
cat("Maste pipeline completed successfully.")