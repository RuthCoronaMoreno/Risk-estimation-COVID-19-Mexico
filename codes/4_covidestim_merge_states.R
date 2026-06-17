# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 4_covidestim_states_merge.R
# Purpose    : Summarize all covidestim estimates for all states by variable
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#
# Input      : csv files containing covidestim output of all states
#
# Output     : "data/covidestim/",variable,"_",states,".csv"
#             
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================


#covidestim files
files <- list.files("data/covidestim/states", pattern = "\\.csv$",full.names = TRUE)

#Create list for saving all data for each variable
lists <- setNames(vector("list", length(vars)), vars)

#Extract columns of each variable
for(f in files){
  
  df <- fread(f, select = c("date", vars))
  
  #get state number
  id <- gsub("|\\_covidestim.csv", "", basename(f))
  
  #Filter each variable data for each state
  for(v in vars){
    tmp <- df[, list(date, value = get(v))]
    setnames(tmp, "value", id)
    
    lists[[v]][[id]] <- tmp
  }
  
}

# Merge data of all states in a unique dataframe for each variable
for(v in vars){
  
  # Merge 
  df_final <- Reduce(
    function(x, y) merge(x, y, by = "date", all = TRUE),
    lists[[v]]
  )
  
  # Order variables in dataframe
  setorder(df_final, date)
  df_final[is.na(df_final)] <- 0
  
  
  # Saving dataframe
  fwrite(df_final, file.path("data/covidestim/", paste0(v, "_states.csv")))
  
  print(v)
}
