# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 2_distribution_estimates.R
# Purpose    : Estimation of mexican prior distribution probabilities for covidestim 
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#
# Input      : confirmed_20-21.csv
#
# Output     : plots of adjustment and histograms of the estimates compared with covidestim propouse
#             
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================

library(dplyr) 
library(fitdistrplus)

#----Upload Data-----
pos_tot=read.csv("data/processed/confirmed_20-21.csv")

#----Functions----
pos_data <-function(st, lastDay){
  pos = pos_tot[pos_tot$FECHA_SINTOMAS<lastDay & pos_tot$FECHA_SINTOMAS>"2020-03-23" & pos_tot$SEXO==1 & pos_tot$EDAD>=40 & pos_tot$EDAD<=59,]
  if(st!=0){
    pos = pos[pos$ENTIDAD_RES==st,]  
  }
  
  return(pos)
}

dist_beta <-function(x,q1,q2){
  fit.beta     = fitdist(x, distr = "beta")
  Dist.beta    = summary(fit.beta)
  a            = Dist.beta$estimate["shape1"]
  b            = Dist.beta$estimate["shape2"]
  
  return(list(fit.beta, a,b))
}

dist_gamma <-function(x){
  fit.gamma     = fitdist(x, distr = "gamma")
  Dist.gamma    = summary(fit.gamma)
  a            = Dist.gamma$estimate["shape"]
  b            = Dist.gamma$estimate["rate"]

  return(list(fit.gamma, a,b))
}

create_x <-function(numer, denomin){
  #----Counting cases by day----
  num_byDay = numer%>%count(FECHA_SINTOMAS)
  denom_byDay = denomin%>%count(FECHA_SINTOMAS)
  
  df = merge( num_byDay, denom_byDay, by = "FECHA_SINTOMAS", all.x=TRUE, all.y=TRUE)
  colnames(df) = c("FECHA_SINTOMAS", "num", "denom")
  df$ratio= df$num/df$denom
  x=df$ratio[!is.na(df$ratio)]
  boxplot(x, main='Probability')
  return(x)
}

estim_func <- function(pos, param) { 
  if(param=="p_sev_if_sym"){
    numerators = pos[pos$TIPO_PACIENTE==2,]
    denominators = pos
    x= create_x(numerators, denominators)
    x= x[x>=0 & x<= quantile(x,0.95)]
    estim = dist_beta(x)
    s1_cvd= 1.8854 #Parameter in covidestim
    s2_cvd= 20.002 #Parameter in covidestim
    
    
  }else if(param=="p_die_if_sev"){
    hosp = pos[pos$TIPO_PACIENTE==2,]
    numerators = hosp[hosp$FECHA_DEF!="9999-99-99",]
    denominators = hosp
    x= create_x(numerators, denominators)
    x= x[x>=0 & x<= quantile(x,0.95)]
    estim = dist_beta(x)
    s1_cvd= 28.239  #Shape parameter in covidestim
    s2_cvd= 162.3 #Shape parameter in covidestim
    s1_cdmx=18.4077 #Parameter in cdmx estimate
    s2_cdmx=24.3287 #Parameter in cdmx estimate
    
    
  }else if(param=="sev_prg_delay"){
    hosp = pos[pos$TIPO_PACIENTE==2,]
    
    hosp_death=hosp[hosp$FECHA_DEF!="9999-99-99",]
    hosp_death$days_delay <- as.Date(as.character(hosp_death$FECHA_DEF), format="%Y-%m-%d")-
      as.Date(as.character(hosp_death$FECHA_INGRESO), format="%Y-%m-%d")
    hosp_death$days_delay = as.numeric(hosp_death$days_delay)
    x= hosp_death$days_delay[!is.na(hosp_death$days_delay)]
    x= x[x>0 & x<= quantile(x,0.95)]
    #x=x[x>=0 & x<=50]
    boxplot(x)
    estim = dist_gamma(x)
    s1_cvd= 2.061 #Shape parameter covidestim
    s2_cvd= 0.2277 * 7 #Rate parameter covidestim
    
    
  }else if(param=="sym_prg_delay"){
    hosp = pos[pos$TIPO_PACIENTE==2,]
    hosp$date_diff <- as.Date(as.character(hosp$FECHA_INGRESO), format="%Y-%m-%d")-
      as.Date(as.character(hosp$FECHA_SINTOMAS), format="%Y-%m-%d")
    hosp$date_diff = as.numeric(hosp$date_diff)
    
    x= hosp$date_diff[!is.na(hosp$date_diff)]
    x=x[x>0 & x<=quantile(x,0.95)]
    boxplot(x)
    estim = dist_gamma(x)
    s1_cvd= 1.624 #Shape parameter covidestim
    s2_cvd= 0.2175 * 7 #Rate parameter covidestim
    
  }else if(param=="sym_prg_regist"){
    pos$date_diff <- as.Date(as.character(pos$FECHA_INGRESO), format="%Y-%m-%d")-
      as.Date(as.character(pos$FECHA_SINTOMAS), format="%Y-%m-%d")
    pos$date_diff = as.numeric(pos$date_diff)
    
    x= pos$date_diff[!is.na(pos$date_diff)]
    x=x[x>0 & x<=quantile(x,0.95)]
    boxplot(x)
    estim = dist_gamma(x)
    s1_cvd= 1.624 #Shape parameter covidestim
    s2_cvd= 0.2175 * 7 #Rate parameter covidestim
    
  }
  return(list(x, estim, s1_cvd, s2_cvd))
}

hist_plot <- function(res, dist){
  # Histogram
  hist(res[[1]],
       breaks = 5,
       probability = TRUE,
       col = "lightgray",
       border = "white",
       main = paste0("Histogram and density for CDMX\n(",dist_name,")"),
       xlab = "x",
       ylim = c(0, 1), #1 gamma / 20 beta
       #xlim = c(0, 1), #comment for gamma
       cex.main = 2.3,
       cex.lab = 1.7,
       cex.axis = 2) #25 gamma / 1 beta
  if(dist=="p_die_if_sev" || dist=="p_sev_if_sym"){
    # Curve beta
    curve(dbeta(x, shape1 = res[[3]], shape2 = res[[4]]),
          from = 0, to = 1,
          add = TRUE,
          lwd = 2,
          col = "blue")
    # Curve beta
    curve(dbeta(x, shape1 = res[[2]][[2]], shape2 = res[[2]][[3]]),
          from = 0, to = 1,
          add = TRUE,
          lwd = 2,
          col = "red",
          lty = 2)
  }else{
    # Curve Gamma
    curve(dgamma(x,
                 shape = res[[3]],
                 rate = res[[4]]),
          add = TRUE,
          lwd = 2,
          col = "blue")
    
    # Curve Gamma
    curve(dgamma(x,
                 shape = res[[2]][[2]],
                 rate = res[[2]][[3]]),
          add = TRUE,
          lwd = 2,
          col = "red")
  }
  
  legend("right",
         legend = c("Covidestim",
                    "Estimate"),
         col = c("blue", "red"),
         lwd = 2,
         lty = c(1, 2),
         bty = "n",
         cex = 1.9)
}

#----Parameters----
state = 9
lastDay = "2020-12-31"
dist_name="sym_prg_regist"

#----Execution----
positives = pos_data(state, lastDay)
results = estim_func(positives, dist_name)

#Adjustment plots
pdf(paste0("plots/distribution_adjustment/",as.character(state),"_",dist_name,"_adjust.pdf"))
plot(results[[2]][[1]])
dev.off() 

#Histogram plot
pdf(paste0("plots/distribution_histograms/",as.character(state),"_",dist_name,"_hist.pdf"))
hist_plot(results, dist_name)
dev.off() 


#Output prameter values
a=results[[2]][[2]]
b=results[[2]][[3]]
cvdstim_a= results[[3]]
cvdstim_b= results[[4]]
