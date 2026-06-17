# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 1_processing_data.R
# Purpose    : Obtaining time series data on COVID-19 cases, deaths, and hospitalizations in Mexico (2020–2021) at the state and municipal levels
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#
# Input      : COVID19MEXICO_2020.csv (https://www.gob.mx/salud/documentos/datos-abiertos-152127)
#              COVID19MEXICO_2021.csv (https://www.gob.mx/salud/documentos/datos-abiertos-152127)
#              data/processed/confirmed_20-21.csv
#              data/population_mx.csv
#
# Output     : data/processed/",type,"_",groupBy,"_mx.csv
#             
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================


################################################################################################
#If you want to first process full official database "COVID19MEXICO_2020.csv" run from this part

#---Filter COVID-19 confirmed cases from the official raw data----
#-Upload dataframes-
#R = read.csv("data/raw/COVID19MEXICO_2020.csv") # raw COVID-19 data
#pos20 = R[R$CLASIFICACION_FINAL %in% c(1, 2, 3),]
#deaths20 = pos20[pos20$FECHA_DEF!="9999-99-99",]

#R2 = read.csv("data/raw/COVID19MEXICO_2021.csv") # raw COVID-19 data
#pos21 = R2[R2$CLASIFICACION_FINAL %in% c(1, 2, 3),]
#deaths21 = pos21[pos21$FECHA_DEF!="9999-99-99",]

#----Merge dataframes----
#pos_tot= rbind(pos20, pos21)
#write.csv(pos_tot, "data/processed/confirmed_20-21.csv")



#####################################################################
#If you already have "confirmed_20-21.csv" you can run from this part

pos_tot$CVEGEO <- paste0(pos_tot$ENTIDAD_RES,sprintf("%03d", pos_tot$MUNICIPIO_RES))

#----Execution----

#Filter data
pos = pos_tot[pos_tot$FECHA_SINTOMAS<lastDay & pos_tot$FECHA_SINTOMAS>="2020-03-23" ,]
pos$FECHA_SINTOMAS = as.Date(pos$FECHA_SINTOMAS)


#Process data
if(groupBy =="states"){
  group="ENTIDAD_RES"
}else if(groupBy == "mun"){
  group="CVEGEO"
}

if(type=="incidence"){
  df <- pos %>%
    group_by(FECHA_SINTOMAS, .data[[group]]) %>%
    summarise(casos = n(), .groups = "drop")
}else if(type=="deaths"){
  df <- pos %>%
    group_by(FECHA_SINTOMAS, .data[[group]]) %>%
    summarise(casos = sum(FECHA_DEF != "9999-99-99"), .groups = "drop")
}else if(type=="hosp"){
  df <- pos %>%
    group_by(FECHA_SINTOMAS, .data[[group]]) %>%
    summarise(casos = sum(TIPO_PACIENTE==2), .groups = "drop")
}

df <- df %>%
  pivot_wider(
    names_from = .data[[group]],
    values_from = casos,
    values_fill = 0
  )

names(df)[1] <- "date"

#Save data
write.csv(df, paste0("data/processed/",type,"_",groupBy,"_mx.csv"))


  
