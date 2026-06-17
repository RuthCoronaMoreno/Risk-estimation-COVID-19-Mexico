# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 6_risk_and_OfficialTrafficLight_mun.R
# Purpose    : Calculate and plot the risk at municipal and state levels, and compare them with the official traffic-light system 
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#
# Input      : data/raw/population_mx.csv
#              data/raw/official_traficlight_colors.csv
#              data/covidestim/infections_mun.csv
#              data/risk/discreet_risk_mx_k",k,".csv
#              data/risk/discreet_risk_mun_k",k,".csv

# Output     : plots/risk/heatmap_discreet_mun_s",s,"_k",k,".pdf
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================


#-----municipalities information----
popmx$key_mun <- paste0("X", sprintf("%05d", popmx$CVEGEO))
popmx$NOM_MUN = str_to_title(popmx$NOM_MUN)

#----------------------------------------------------------------------------------------------
#--RUN this part if CSV for gathering size k in municipalities does not exist--
#----------------------------------------------------------------------------------------------

if(file.exists(paste0("data/risk/continuous_risk_mun_k",k,".csv"))==FALSE & file.exists(paste0("data/risk/discreet_risk_mun_k",k,".csv"))==FALSE){
  
  #-covidestim estimates for municipalities-
  df=read.csv("data/covidestim/infections_mun.csv")
  
  pop_vec <- setNames(popmx$POBTOT, paste0("X",popmx$CVEGEO))
  
  #-Risk function-
  risk=function(p,n){
    r=0
    r=(1-((1-p)^n))*100
    return (r)
  } 
  
  #-Computation of p=active/pop for risk function-
  
  #Active cases
  df_active<- as.data.frame(
    lapply(df[-1], function(x)
      rollsum(x, k = 14, align = "right", fill = NA)
    )
  )
  df_active <- cbind(date = df$date , df_active)
  df_active[is.na(df_active)] <- 0
  
  pop <- pop_vec[match(colnames(df_active), names(pop_vec))][-1]
  
  #computation of p
  df_p <- as.data.frame(
    lapply(names(df_active[-1]), function(col) {
      df_active[[col]] / pop[col]
    })
  )
  
  #computation of risk for n=k
  df_risk <- as.data.frame(lapply(df_p, risk, n = k))
  colnames(df_risk)=colnames(df_active[-1])
  df_risk <- cbind(date = df_active$date , df_risk)
  
  #save continuous risk as .csv
  write.csv(df_risk, paste0("data/risk/continuous_risk_mun_k",k,".csv"), row.names=FALSE)
  
  #----Discreet risk----
  df_risk_disc = df_risk
  df_risk_disc[-1][] <- lapply(df_risk_disc[-1], function(x) {
    ifelse(x < 25, "1",
           ifelse(x < 50, "2",
                  ifelse(x < 75, "3", "4")))
  })
  
  
  #save discreet risk as .csv
  write.csv(df_risk_disc, paste0("data/risk/discreet_risk_mun_k",k,".csv"), row.names=FALSE)
  
}

#----------------------------------------------------------------------
#---------Comparison plots of risk and official traffic-light----------
#----------------------------------------------------------------------

#-----Official traffic light----
start_date <- as.Date("2020-07-06")
end_date <- as.Date("2020-12-31") 

#Epidemiological weeks number
nweek_start_date<-ifelse(start_date<"2021-01-01",isoweek(start_date)-1, isoweek(start_date))
nweek_end_date<-ifelse(end_date<"2021-01-01",isoweek(end_date)-1,isoweek(end_date))

#First Monday of every month
date_seq <- seq(start_date, end_date, by = "days")
mondays <- date_seq[weekdays(date_seq) == 'Monday']
date1_months=mondays[!duplicated(format(mondays, "%Y-%m"))]


Official_TrafficLight<-function(){
  traf_light<- read.csv("data/raw/official_traficlight_colors.csv")
  
  #Change rows order
  new_order <- c("Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México")
  rows <- which(traf_light$estado %in% c("Chiapas","Chihuahua","Ciudad de México","Coahuila","Colima"))
  traf_light[rows, ] <- traf_light[rows[match(new_order, traf_light$estado[rows])], ]
  
  traf_light$estado <- toupper(traf_light$estado)
  
  startweek = which(colnames(traf_light)==paste0("X",year(start_date),".W",nweek_start_date))
  endweek = which(colnames(traf_light)==paste0("X",year(end_date),".W",nweek_end_date))
  traf_light_cut = traf_light[,c(1, startweek:endweek)]
  
  colnames(traf_light_cut)<- c("state", format(mondays, format="%Y-%m-%d"))
  
  traf_light_final=traf_light_cut[,c("state",as.character(date1_months))]
  
  
  #-----Transpose format-----
  traf_light_final <- as.data.frame(t(traf_light_final))
  colnames(traf_light_final) <- as.character(traf_light_final[1, ])
  traf_light_final <- traf_light_final[-1, ]
  
  
  #-----Transform to ordinal values-----
  traf_light_final[] <- lapply(traf_light_final, function(x) {
    recode <- c(
      "verde" = 1,
      "amarillo" = 2,
      "naranja" = 3,
      "rojo" = 4
    )
    recode[x]
  })
  
  traf_light_final<-cbind(date = rownames(traf_light_final) , traf_light_final)
  traf_light_final$date=as.Date(traf_light_final$date)
  rownames(traf_light_final) <- NULL
  
  melted_official <- melt(traf_light_final, id="date")
  
  return(melted_official)
}


official<-Official_TrafficLight()
official$value <- factor(official$value)
official$variable<-stri_trans_general(official$variable, "Latin-ASCII")

#------Risk traffic light (states)------
risk_state_disc = read.csv(paste("data/risk/discreet_risk_mx_k",k,".csv",sep=""))
risk_state_disc$date=as.Date(risk_state_disc$date)
risk_state_disc=risk_state_disc[risk_state_disc$date %in% date1_months,]
melted_state_risk <- melt(risk_state_disc, id="date")
melted_state_risk$value <- factor(melted_state_risk$value)
melted_state_risk$variable<- gsub("[.]", " ", melted_state_risk$variable)


  
#-------Risk traffic light (municipalities)-----
risk_mun_disc = read.csv(paste("data/risk/discreet_risk_mun_k",k,".csv",sep=""))
risk_mun_disc$date=as.Date(risk_mun_disc$date)
risk_mun_disc=risk_mun_disc[risk_mun_disc$date %in% date1_months,]
colnames(risk_mun_disc)[-1]=paste0("X",sprintf("%05d", as.numeric(sub("^X", "", colnames(risk_mun_disc)[-1]))))
melted_mun_risk <- melt(risk_mun_disc, id="date")
melted_mun_risk$value <- factor(melted_mun_risk$value)
melted_mun_risk$variable  <-popmx$NOM_MUN[match(melted_mun_risk$variable, popmx$key_mun)]


#-------Plot function----------

plot_func <- function(s){
  
  state_name=popmx$NOM_REGION[popmx$CLAVE_REGION==s][1]
  #state=sprintf("%02d", s)
  mun_state=popmx$NOM_MUN[popmx$CLAVE_REGION==s]
  
  if(length(mun_state)<20){
    a=0.1 
    b=0.8
    hprop=10
    w=10
  }else if(length(mun_state)>=20 & length(mun_state)<90){
    a=0.05 
    b=0.9
    hprop=20
    w=10
  }else if(length(mun_state)>=90 & length(mun_state)<125){
    a=0.05
    b=0.9
    hprop=30
    w=10
  }else if(length(mun_state)>=125 & length(mun_state)<250){
    a=0.02 
    b=0.96
    hprop=50
    w=10
  }else if(length(mun_state)>500){
    a=0.01 
    b=0.98
    hprop=100
    w=15
  }

  #----plot official traffic-light----
  p0 <- ggplot(official[official$variable==state_name,],
               aes(x = date, y = "", fill = value)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c(
        "4" = "#D73027",
        "3" = "#FC8D59",
        "2" = "#FEE08B",
        "1" = "#1A9850"
      ),breaks = c("rojo", "naranja", "amarillo", "verde"), 
      labels = c(
        "4" = "Red",
        "3" = "Orange",
        "2" = "Yellow",
        "1" = "Green"
      ),
      na.value = "grey90") + labs(fill = "Color ")+
    scale_x_date(expand = c(0,0), breaks = official$date)+
    labs(
      title = paste0("Official traffic light in ", state_name),
      x = "", 
      y = "" 
    )+
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 7),  
          legend.key.size = unit(0.5, "lines"), 
          plot.title = element_text(size = 16) 
    )
  
  #----plot risk traffic-light at state level----
  p1 <- ggplot(melted_state_risk[melted_state_risk$variable==state_name,],
               aes(x = date, y = variable, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c(
        "4" = "#D73027",
        "3" = "#FC8D59",
        "2" = "#FEE08B",
        "1" = "#1A9850"
      ),breaks = c("rojo", "naranja", "amarillo", "verde"),
      labels = c(
        "4" = "#D73027",
        "3" = "#FC8D59",
        "2" = "#FEE08B",
        "1" = "#1A9850"
      ),
      na.value = "grey90"
    )+
    scale_x_date(expand = c(0,0), breaks = risk_state_disc$date)+
    labs(
      title = "Montly risk estimate at state level",
      x = "", 
      y = "" 
    )+
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 13) 
    )
  
  #----plot risk traffic-light at municipality level----
  p2 <- ggplot(melted_mun_risk[melted_mun_risk$variable %in% mun_state,],
               aes(x = date, y = variable, fill = factor(value))) +
    geom_tile(color = "white") +
    scale_y_discrete(limits = rev)+
    scale_fill_manual(
      name="Color",
      values = c(
        "4" = "#D73027",
        "3" = "#FC8D59",
        "2" = "#FEE08B",
        "1" = "#1A9850"
      ),breaks = c("rojo", "naranja", "amarillo", "verde"),
      labels = c(
        "4" = "Red",
        "3" = "Orange",
        "2" = "Yellow",
        "1" = "Green"
      ),
      na.value = "grey90"
    )+
    scale_x_date(expand = c(0,0), breaks = risk_mun_disc$date)+
    labs(
      title= "Montly risk estimate at municipal level",
      x = "",
      y = "str_to_title(type_mun)"
    )+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_blank()
          
    )
  
  #set sizes of heatmaps p0, p1 and p2 respectively
  h0=a*length(rownames) 
  h1=a*length(rownames) 
  h2=b*length(rownames)
  
  #Combine all the heatmaps into a single chart
  final_plot <- plot_grid(p0,p1, p2,
                          ncol = 1,
                          align = "v",
                          axis = "lr", 
                          rel_heights = c(h0,h1,h2))
  
  #Save final_plot
  fig_name=paste0("plots/risk/heatmap_discreet_mun_s",s,"_k",k,".pdf")
  ggsave(fig_name, plot=final_plot, height = hprop*length(rownames), #change factor of height to change size of pdf
         width = w, limitsize = FALSE)  
  
  
  
  return(final_plot)
}

#----plot execution----
for(n in 1:32){
  plot_func(n) #n: official state number
}


