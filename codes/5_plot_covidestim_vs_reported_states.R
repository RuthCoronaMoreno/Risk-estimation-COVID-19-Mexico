# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 5_plot_covidestim_vs_reported_states.R
# Purpose    : Plot covidestim outputs vs reported data for each state
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

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

#----parameter settings----
lastDay <- as.Date("2021-12-25")
data_type=c("incidence", "hosp", "Rt")

#----population mx----
pop_csv <- read.csv("data/raw/population_mx.csv")


#----Reported data----
report_files <- list.files("data/processed/", pattern = "\\_states_mx.csv$",full.names = TRUE)
report_vars <- sub("_states_mx\\.csv$", "", basename(report_files))

report_list <- lapply(report_files, read.csv)
names(report_list) <- report_vars

report_list <- lapply(report_list, function(df) {
  df$date <- as.Date(df$date)
  df<-df[df$date<=lastDay,]
  df}
  )


#----covidestim data----
estim_files <- list.files("data/covidestim/", pattern = "\\_states.csv$",full.names = TRUE)
estim_vars <- sub("_states\\.csv$", "", basename(estim_files))

estim_list <- lapply(estim_files, read.csv)
names(estim_list) <- estim_vars

estim_list <- lapply(estim_list, function(df) {
  df$date <- as.Date(df$date)
  df<-df[df$date<=lastDay,]
  df}
)

#----plot estimate infections vs reported incidence----

plot <- function(type){
  
  if(type=="incidence"){
    x="incidence"
    x.e="infections"
    title_var="cases"
    y_name="Cases"
  }else if(type=="deaths"){
    x="deaths"
    x.e="deaths"
    title_var="deaths"
    y_name="Deaths"
  }else if(type=="hosp"){
    x="hosp"
    x.e="severe"
    title_var="severe"
    y_name="Cases"
  }else if(type=="Rt"){
    x="incidence"
    x.e="infections"
    title_var="cases"
    y_name="Cases"
    x.e2="Rt"
  }
  
  Rep=report_list[[x]][,c("date",paste0("X",state))]
  colnames(Rep)<-c("Date","Cases")
  
  estim=data.frame("date"=estim_list[[x.e]][,"date"], 
                   "median"= estim_list[[x.e]][,paste0("X",state1)],
                   "lo"=estim_list[[paste0(x.e,".lo")]][,paste0("X",state1)],
                   "hi"=estim_list[[paste0(x.e,".hi")]][,paste0("X",state1)]
                   )
  if (exists("x.e2")){
    estim2=data.frame("date"=estim_list[[x.e2]][,"date"], 
                     "median"= estim_list[[x.e2]][,paste0("X",state1)],
                     "lo"=estim_list[[paste0(x.e2,".lo")]][,paste0("X",state1)],
                     "hi"=estim_list[[paste0(x.e2,".hi")]][,paste0("X",state1)])
    estim2[estim2==0]<-NA
  }
  
  ymax <- max(
    Rep[,"Cases"],
    estim[,"median"],
    estim[,"hi"]
  ) + 3
  
  
  if(type=="Rt"){
    rt_max <- 3      
    rt_min <- 0
    
    factor_rt <- max(
      Rep$Cases,
      estim[["hi"]],
      na.rm = TRUE
    ) / rt_max
    
    y_min <- rt_min * factor_rt
    y_max <- rt_max * factor_rt
    
    p2 <- ggplot() +
      geom_col(
        data = Rep,
        aes(Date, Cases, fill = "Reported")
      ) +
      scale_fill_manual(name=NULL,
                        values=c("Reported"=rgb(252,203,101,maxColorValue = 255)))+
      geom_ribbon(
        data = estim,
        aes(
          x=date,
          ymin=lo,
          ymax=hi),
        fill = rgb(235,225,220, maxColorValue = 255), alpha=0.6
      ) +
      geom_line(
        data = estim,
        aes(date, median,
            color = "Median estimate"),
        linewidth = 1
      ) +
      
      geom_line(
        data = estim2,
        aes(date,median*factor_rt,
            color = "Rt"),
        linewidth = 1
      ) +
      geom_ribbon(
        data = estim2,
        aes(
          x=date,
          ymin=lo*factor_rt,
          ymax=hi*factor_rt),
        fill = rgb(0,0,50, maxColorValue = 255), alpha=0.2
      )+
      
      geom_hline(
        yintercept = 1*factor_rt,
        color = "red"
      ) +
      scale_y_continuous(
        name = "Incidence",
        limits = c(y_min,y_max),
        sec.axis = sec_axis(
          ~./factor_rt,
          name = "Rt"
        )
      ) +
      scale_color_manual(
        values = c(
          "Median estimate" = "black",
          "Rt" = "blue"
        )
      ) +
      labs(
        title = paste0(
          "Median and 95% interval around estimate ",x.e2 ," in ",
          state_name),
        y = y_name,
        x = NULL,
        color = NULL
      ) +
      theme_bw(base_size = 16)
    
    
    ggsave(paste0("plots/covidestim_estimates/",x.e2,"/",x.e2,"_estimates_state",as.character(state1),".pdf"))
    
  }else{
    p1 <- ggplot() +
      
      geom_col(
        data = Rep,
        aes(Date, Cases, fill = "Reported")
      ) +
      scale_fill_manual(name=NULL,
                        values=c("Reported"=rgb(252,203,101,maxColorValue = 255)))+
      geom_ribbon(
        data = estim,
        aes(
          x=date,
          ymin=lo,
          ymax=hi),
        fill = rgb(235,225,220, maxColorValue = 255), alpha=0.6
      ) +
      geom_line(
        data = estim,
        aes(date, median,
            color = "Median estimate"),
        linewidth = 1
      ) +
      coord_cartesian(ylim = c(0,ymax))+
      labs(
        title = paste(
          "Median and 95% interval around reported ",title_var ," in",
          state_name
        ),
        y = y_name,
        x = NULL,
        color = NULL
      ) +
      theme_bw(base_size = 16)
      
      
    ggsave(paste0("plots/covidestim_estimates/",title_var,"/",title_var,"_estimates_state",as.character(state1),".pdf"))

  }
}


#----Execution----
for(state1 in 1:32){
  state=sprintf("%02d", state1)
  state_name=pop_csv$NOM_REGION[pop_csv$CLAVE_REGION==state1][1]
  lapply(data_type, plot)
  
}











