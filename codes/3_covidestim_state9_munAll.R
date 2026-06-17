# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 3_covidestim_state9_munAll.R
# Purpose    : covidestim estimation for all municipalities of state 9.
#
#Requirements: Run it in the supercomputer of "National Laboratory for Advanced Scientific Visualization (LAVIS-UNAM)"
#              by using the code "run_s9_mun.slurm" 
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#
# Input      : incidence_mun_mx.csv
#              deaths_mun_mx.csv
#              population_mx.csv

# Output     : "data/covidestim/state9/",name,"_covidestim.csv"
#             
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================

library(covidestim)
library(tidyverse)

state=9

#databases
I=read_csv("data/processed/incidence_mun_mx.csv")
D=read_csv("data/processed/deaths_mun_mx.csv")
popcsv=read_csv("data/raw/population_mx.csv")

#Run it over all municipalities of state "state"
for(i in 2:17){
  mun=i
  
  #Name and total population of municipality (2020)
  if(mun >=10 & mun< 99){
    name=paste0(state,"0",mun)
    pop=popcsv[[which(popcsv$CVEGEO==name), 5]] #Column 5 has population of each municipality  
  }else if(mun <10){
    name=paste0(state,"00",mun)
    pop=popcsv[[which(popcsv$CVEGEO==name), 5]] #Column 5 has population of each municipality  
  }else if(mun >99){
    name=paste0(state,mun)
    pop=popcsv[[which(popcsv$CVEGEO==name), 5]] #Column 5 has population of each municipality  
  }
  
  if(state<10){
    name=paste0(0,name)
  }
  
  #Selecting incidence and deaths time series
  dI=I[,c("date",name)]
  dD=D[,c("date",name)]
  
  #Custumizing format of databases according to covidestim requirements
  dI$date=as.Date(dI$date, format="%d/%m/%Y")
  dD$date=as.Date(dD$date, format="%d/%m/%Y")
  
  dI[is.na(dI)]<-0
  dD[is.na(dD)]<-0
  
  dI=dI[dI$date<"2022-01-01",]
  dD=dD[dD$date<"2022-01-01",]
  
  dI=rename(dI, c("observation"=name))
  dD=rename(dD, c("observation"=name))
  
  #Run covidestim
  cfg <- covidestim(ndays=nrow(df1), ndays_before=10, window.length=7, pop_size=pop, region='California')+priors_progression(pri_serial_i=c(81,450), sev_prg_delay=c(1.5064,0.1672), sym_prg_delay=c(2.8574, 0.4762)) + priors_transitions( p_sev_if_sym = c(7.1888, 55.0788), p_die_if_sev = c(18.4077,24.3287)) + input_cases(dI, type="occurred") + input_deaths(dD, type = "occurred")
  
  output <- run(cfg, cores = parallel::detectCores(3))
  output.summary <- summary(output, include.before=TRUE, index=TRUE)
  
  
  #Save output
  dir_path <- paste0("data/covidestim/state",state)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  } 
  write.csv(output.summary, paste0(dir_path,"/",name,"_covidestim.csv"))
  
  
}



