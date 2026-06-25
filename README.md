# Risk-estimation-COVID-19-Mexico
This repository contains the scripts used to estimate an alternative COVID-19 risk traffic light system for Mexico during the COVID-19 epidemic. The analyses were conducted at both **state** and **municipal** levels using the Mexican official epidemiological surveillance data and were designed to provide guidance on population gathering sizes according to local epidemic risk.

The workflow is structured as follows:

**Data curation.** Process the official Mexican COVID-19 surveillance data (2020–2021) to extract the daily time series of confirmed cases, hospitalizations, and deaths at both state and municipal levels.

**Parameter calibration.** Estimate the prior Mexican epidemiological distributions required to calibrate the `covidestim` model.

**Epidemic estimation.** Execute the `covidestim` model to reconstruct the unobserved epidemic dynamics for all 32 states and over 2,400 municipalities across Mexico.

**Risk metric computation.** Compute the probability of encountering at least one infected individual in gatherings of a specific size $k$ (e.g., $k = 100$).


The results of this repository are presented in the preprint:  
&emsp; **A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico**  
&emsp; *Ruth Corona-Moreno, M. Adrian Acuña-Zegarra, Mario Santana-Cibrian, Jorge X. Velasco-Hernandez*  
&emsp; medRxiv 2026.06.09.26355274;  
&emsp; doi: https://doi.org/10.64898/2026.06.09.26355274



## Repository structure
COVID19-MX-Estimation/  
&emsp; &emsp; |  
&emsp; &emsp; |---codes/  
&emsp; &emsp; |&emsp; &emsp;     ├──1_processing_data.R  
&emsp; &emsp; |&emsp; &emsp;     ├──2_distribution_estimation.R  
&emsp; &emsp; |&emsp; &emsp;     ├──3_covidestim_state9_munAll.R  
&emsp; &emsp; |&emsp; &emsp;     ├──3_covidestim_state9.R  
&emsp; &emsp; |&emsp; &emsp;     ├──4_covidestim_merge_mun.R  
&emsp; &emsp; |&emsp; &emsp;     ├──4_covidestim_merge_states.R  
&emsp; &emsp; |&emsp; &emsp;     ├──5_plot_covidestim_vs_reported_mun.R  
&emsp; &emsp; |&emsp; &emsp;     ├──5_plot_covidestim_vs_reported_states.R  
&emsp; &emsp; |&emsp; &emsp;     ├──6_risk_and_OfficialTrafficLight_mun.R  
&emsp; &emsp; |&emsp; &emsp;     ├──6_risk_and_OfficialTrafficLight_state.R  
&emsp; &emsp; |&emsp; &emsp;     ├──7_heatmap_OfficialTrafficLight.R  
&emsp; &emsp; |&emsp; &emsp;     ├──run_all.R  
&emsp; &emsp; |&emsp; &emsp;     ├──run_s9_full.slurm  
&emsp; &emsp; |&emsp; &emsp;     └──run_s9_mun.slurm  
&emsp; &emsp; |  
&emsp; &emsp; |---data/  
&emsp; &emsp; |&emsp; &emsp;     ├──covidestim/  
&emsp; &emsp; |            
&emsp; &emsp; |&emsp; &emsp;     ├──processed/  
&emsp; &emsp; |            
&emsp; &emsp; |&emsp; &emsp;     ├──raw/  
&emsp; &emsp; |  
&emsp; &emsp; |&emsp; &emsp;     └──risk/  
&emsp; &emsp; |  
&emsp; &emsp; |---plots/  
&emsp; &emsp; |  
&emsp; &emsp; |---README.md  


## Software requirements

* R ($\geq$ 4.0.2)  
* SLURM-managed high-performance computing (HPC) cluster

### Required R packages
* `tidyverse`
* `data.table`
* `readxl`
* `fitdistrplus`
* `irr`
* `lubridate`
* `reshape2`
* `cowplot`
* `zoo`

* `covidestim` version of  corresponding to commit 1e431b987efaa656cd2e1c499f8965a4ea8ab976 (March 24, 2021). In order to reproduce our results you should install this exact version of the software:  

```bash
 git clone https://github.com/covidestim/covidestim.git
 
 cd covidestim  
 
 git checkout 1e431b987efaa656cd2e1c499f8965a4ea8ab976
```
### Computational requirements 
&emsp; &emsp; The `covidestim` package requires its execution on a HPC cluster managed with SLURM.


## Data availability
Due to file size limitations, the complete datasets are not stored in this GitHub repository.

All datasets and supplementary materials are available through Zenodo:

DOI: XXXXX

The repository contains only the code required to reproduce the analyses.


## Reproducing the Analyses
In order to reproduce our results you may follow the following steps:   

1. Create a folder with the Repository structure last mentioned.

2. From our Zenodo repository, download the following databases and save them into the specified folder:  

* data/processed/confirmed_20-21.csv
* data/raw/population_mx.csv
* data/raw/official_traficlight_colors.csv
* data/raw/official_states_municipalities_keys.xlsx

3. Execute the R code *run_all.R* in the specified order so that the required data for the following steps is first created.

In the following table we specify the use, output files as well as the folder where they will be storage once *run_all.R* code executes the specified code.


| R code   |  Use |Output | 
| -------- | -------- | -------- | 
| 1_processing_data.R  |Extract COVID-19 Mexican time series of reported incidence, hospitalizations and deaths at municipal and state level (2020-2021)|  **data/processed/** <br> ├──deaths_mun_mx.csv,<br> ├──deaths_state_mx.csv,<br> ├──hosp_mun_mx.csv,<br> ├──hosp_state_mx.csv, <br> ├──incidence_mun_mx.csv,<br> ├──incidence_state_mx.csv|
| 2_distribution_estimation.R  |Estimation of Mexican prior distribution probabilities parameters for covidestim calibration | **plots/distribution_adjustment/**<br> ├── 9_sym_prg_delay_adjust.pdf<br> ├── 9_sev_prg_delay_adjust.pdf <br> ├── 9_p_sev_if_sym_adjust.pdf <br> ├── 9_p_die_if_sev_adjust.pdf **plots/distribution_histograms/**<br> ├── 9_sym_prg_delay_hist.pdf<br> ├── 9_sev_prg_delay_hist.pdf <br> ├── 9_p_sev_if_sym_hist.pdf <br> ├── 9_p_die_if_sev_hist.pdf<br> <br> *The adjusted distributions parameters are printed after the code execution*|
| 3_covidestim_state9.R\*  |The `covidestim` estimation for state 9 (Mexico city) at state level | **data/covidestim/states/**<br> ├──9_covidestim.csv <br> ├──...|
| 3_covidestim_state9_munAll.R\*\*  |The `covidestim` estimation for each borough(municipality) of the state 9 (Mexico city)| **data/covidestim/state9/** <br> ├──09002_covidestim.csv, <br> ├──... <br> ├──09017_covidestim.csv |
| run_s9_full.slurm\* | slurm code to run `3_covidestim_state9.R` in HPC cluster.|- |
| run_s9_mun.slurm\*\* | slurm code to run `3_covidestim_state9_munAll.R` in HPC cluster.|- |
| 4_covidestim_merge_states.R | Merge `covidestim` estimates for all states by covidestim **variable** output (severe, Rt, infections, deaths, cum.incidence) |**data/covidestim/** <br> ├──severe_states.csv,<br> ├── ...<br> ├──cum.incidence_states.csv|
| 4_covidestim_merge_mun.R | Merge `covidestim` estimates for all municipalities by covidestim **variable** output | **data/covidestim/** <br> ├──severe_mun.csv, <br> ├── ... <br> ├──cum.incidence_mun.csv|
| 5_plot_covidestim_vs_reported_states.R | Plot covidestim outputs vs reported data for each state |**plots/covidestim_estimates/*variable* ** <br> ├──**variable**_estimates_state9.pdf <br> ├── ...|
| 5_plot_covidestim_vs_reported_mun.R | Plot covidestim outputs vs reported data for each municipality | **plots/covidestim_estimates/*variable* ** <br> ├──*variable*_estimates_mun9002.pdf <br> ├──...| 
| 6_risk_and_OfficialTrafficLight_mun.R | Calculate and plot the risk at municipal and state level for each state, and compare them with the official traffic-light system. | **data/risk/** <br> ├──continuous_risk_mun_k100.csv <br> ├──discreet_risk_mun_k100.csv <br> **plots/risk/** <br> ├──heatmap_discreet_mun_s9_k100.pdf <br> ├──...|   
| 6_risk_and_OfficialTrafficLight_states | Calculate and plot the risk state level for all states, and compare them with the official traffic-light system. | **data/risk/** <br> ├──continuous_risk_mx_kN.csv <br> ├──discreet_risk_mx_kN.csv <br> ├──discreet_severalK_1-200.csv <br> ├──continuous_risk_mx_k100.csv <br> **plots/risk/** <br> ├──official_trafficlight.pdf <br> ├──heatmap_continuous_mx_100.pdf<br> ├──heatmap_discreet_mx_k100.pdf <br> ├──heatmap_discreet_mun_s9_k100.pdf<br> ├──...|



\* Similar scripts were created for each state of Mexico, according to its official number-key label available in *data/raw/official_states_municipalities_keys.xlsx* file.  

\*\* Similar scripts were created for each municipality in Mexico using the corresponding official five-digit municipality code provided in *data/raw/official_states_municipalities_keys.xlsx* file. Municipalities that did not report any COVID-19 cases during 2020–2021 were excluded from the analyses.  







