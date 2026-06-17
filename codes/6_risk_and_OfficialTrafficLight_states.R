# =============================================================================
# Project    : A risk-of-contagion index using a Bayesian based model for the COVID-19 epidemic in Mexico
# Script     : 6_risk_and_OfficialTrafficLight_states.R
# Purpose    : Calculate and plot risk metrics and compare them with the official traffic-light system
#
# Authors    : Ruth Corona-Moreno*, M. Adrian Acuña-Zegarra**, Mario Santana-Cibrian***, *Jorge X. Velasco-Hernandez
# Affiliations: *Instituto de Matemáticas UNAM-Juriquilla, **Departamento de Matemáticas Universidad de Sonora, ***Escuela Nacional de Estudios Superiores UNAM Juriquilla
#
# Contact    : ruth.corona.m@im.unam.mx
#
# Input      : "data/raw_data/official_traficlight_colors.csv"
#              "data/covidestim/infections_states.csv"
#              "data/population_mx.csv"
#
# Output     : "data/risk/continuous_risk_mx_kN.csv"
#              "data/risk/discreet_risk_mx_kN.csv"
#              "data/risk/discreet_severalK_1-200.csv"
#             
# Repository : https://github.com/RuthCoronaMoreno/COVID19-MX-Estimation
# =============================================================================

library(tidyr)
library(stringi)
library(ggplot2)
library(irr)
library(lubridate)
library(dplyr)
library(reshape2)

#-------------------------------
#-----Official traffic light----
#-------------------------------
Official_TrafficLight<-function(heatmap_trafficlight){
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
    
  
  #----- Plot heatmap-----
  if(heatmap_trafficlight){
    traf_light_long <- traf_light_cut %>% 
      pivot_longer(
        cols = -state,
        names_to = "mondays",
        values_to = "values"
      )
    
    traf_light_long$state <- factor(
      traf_light_long$state,
      levels = rev(unique(traf_light_long$state))
    )
    
    # Heatmap
    ggplot(traf_light_long, aes(x = mondays, y = state, fill = values)) +
      geom_tile(color = "white") +
      scale_fill_manual(
        values = c(
          "rojo" = "#D73027",
          "naranja" = "#FC8D59",
          "amarillo" = "#FEE08B",
          "verde" = "#1A9850"
        ),breaks = c("rojo", "naranja", "amarillo", "verde"),
        labels = c(
          "rojo" = "Red",
          "naranja" = "Orange",
          "amarillo" = "Yellow",
          "verde" = "Green"
        ),
        na.value = "grey90"
      ) +
      labs(
        title = "Official traffic-light",
        x = "", y = "", fill = "Color "
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11)
      )
    
    ggsave("plots/risk/official_trafficlight.pdf") 
  } 
  
  return(traf_light_final)
}


#-------------------------------
#------------Risk---------------
#-------------------------------

#Risk function
risk=function(p,n){
  r=0
  r=(1-((1-p)^n))*100
  return (r)
}

#risk_Traffic-Light
risk_TrafficLight <-function(type_traffic_light){
  
  if (!exists("setup_done")){
    pop_info=pop_func()
    df = pop_info[[1]]
    pop_sort = pop_info[[2]][]
    ddss=pop_info[[3]][]
    
    setup_done <- TRUE
  }
  
  #----Full continuous risk----
  df_risk <- data.frame()
  for(i in (1:(length(df$date)-13))){
    df$date<- as.Date(df$date)
    dt1=df$date[i]
    dt2=df$date[i+13]
    
    d=df%>%filter(df$date>=dt1 & df$date<=dt2)
    df1=colSums(d[,-1])
    p = df1/pop_sort
    rsk=risk(p,k)
    df_risk=rbind(df_risk,rsk)
  }
  
  colnames(df_risk) <- names(pop_sort)
  newdates <- seq(df$date[14]+8, df$date[length(df$date)]+8, by="days")
  
  df_risk <- cbind("date" = newdates, df_risk)
  
  #Filter continuous risk in date1_months
  df_risk_filt <- df_risk[df_risk$date %in% date1_months,]
  rownames(df_risk_filt) <- df_risk_filt$date
  df_risk_filt$date <- NULL
  
  keys=as.numeric(gsub("X", "", colnames(df_risk_filt)))
  
  names_new=sapply(keys, function(i){
    ddss$NOM_REGION[ddss$CLAVE_REGION==i][1]  
  })
  
  colnames(df_risk_filt)=names_new
  
  
  df_risk_filt <- df_risk_filt[,  stri_trans_general(colnames(TrafLight), "Latin-ASCII")]
  
  
  #-----Discrete risk-----
  df_risk_discret <- df_risk_filt
  
  df_risk_discret[] <- lapply(df_risk_discret, function(x) {
    ifelse(x < 25, 1,
           ifelse(x < 50, 2,
                  ifelse(x < 75, 3, 4)))
  })
  
  
#----------------------------------------------------------------  
  if(type_traffic_light=="continuous_risk"){
    
    #----save csv----
    write.csv(df_risk_filt, paste0("data/risk/continuous_risk_mx_k",k,".csv"), row.names=FALSE)
    
    #----plot continuous heatmap-----
    melted_state_risk <- melt(data.frame(date=as.Date(rownames(df_risk_filt)), df_risk_filt), id="date")
    melted_state_risk$variable <- factor(
      melted_state_risk$variable,
      levels = rev(unique(melted_state_risk$variable))
    )
    
    figname=paste0("plots/risk/heatmap_continuous_mx_k",k,".pdf")
    ggplot(melted_state_risk,
           aes(x = date, y = variable, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(limits=c(0,100),colours = c("#4cde2f","#fefb4c","#e8002f"),name="% Risk")+
      scale_x_date(breaks = unique(melted_state_risk$date),
                   date_labels = "%Y-%m-%d",expand = c(0,0))  +
      labs(
        title = paste0("Continuous risk estimate in Mexico (k=",k,")"),
        x = "", 
        y = "" 
      )+
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
            axis.text.y = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 11)
      )
    
    ggsave(figname, height = 7*length(colnames), width = 7, limitsize = FALSE) #change factor of height to change size of pdf
    #width = 10, limitsize = FALSE)
    
    df_output=df_risk_filt
  }else if(type_traffic_light=="discreet_risk"){
    
    
    #----save csv----
    write.csv(df_risk_discret, paste0("data/risk/discreet_risk_mx_k",k,".csv"), row.names=FALSE)
    
    
    #----plot discret heatmap-----
    melted_state_risk2 <- melt(data.frame(date=as.Date(rownames(df_risk_discret)), df_risk_discret), id="date")
    melted_state_risk2$variable <- factor(
      melted_state_risk2$variable,
      levels = rev(unique(melted_state_risk2$variable))
    )
    
    figname=paste0("plots/risk/heatmap_discreet_mx_k",k,".pdf")
    ggplot(melted_state_risk2,
           aes(x = date, y = variable, fill = factor(value))) +
      geom_tile(color = "white") +
      scale_fill_manual(name="Color",
                        values = c(
                          "4" = "#D73027",
                          "3" = "#FC8D59",
                          "2" = "#FEE08B",
                          "1" = "#1A9850"
                        ),breaks = c("4", "3", "2", "1"),
                        labels = c(
                          "4" = "Red",
                          "3" = "Orange",
                          "2" = "Yellow",
                          "1" = "Green"
                        ),
                        na.value = "grey90"
      )+
      scale_x_date(breaks = unique(melted_state_risk2$date),
                   date_labels = "%Y-%m-%d",expand = c(0,0))  +
      #scale_x_date(expand = c(0,0), breaks = date1_months)+
      labs(
        title = paste0("Discretize risk estimate in Mexico (k=",k,")"),
        x = "", # "Dates",
        y = "" #str_to_title(type_mun)
      )+
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
            axis.text.y = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 11)
      )
    
    ggsave(figname, height = 7*length(colnames), width = 7, limitsize = FALSE) #change factor of height to change size of pdf
    #width = 10, limitsize = FALSE)
   
    df_output=df_risk_discret
  }
  
  
  return(df_output)
}  


#-------------------------------
#---------Population------------
#-------------------------------
pop_func<-function(){
  #covidestim infections
  df=read.csv(paste0("data/covidestim/infections_states.csv"))  
  df[is.na(df)] <- 0
  
  #population
  ddss = read.csv(paste("data/raw/population_mx.csv",sep=""))
  
  pop <- ddss %>%
    group_by(CLAVE_REGION) %>%
    summarise(suma_total = sum(POBTOT))
  
  pop_vec <- setNames(pop$suma_total, paste0("X",pop$CLAVE_REGION))
  pop_s <- pop_vec[match(colnames(df), names(pop_vec))][-1]
  
  return(list(df, pop_s, ddss))
}


#-----------------------
#-------Execution-------
#-----------------------

#--Set parameters--
start_date <- as.Date("2020-07-06")
end_date <- as.Date("2021-10-26") 
k=100

#Epidemiological weeks number
nweek_start_date<-ifelse(start_date<"2021-01-01",isoweek(start_date)-1, isoweek(start_date))
nweek_end_date<-ifelse(end_date<"2021-01-01",isoweek(end_date)-1,isoweek(end_date))

#First Monday of every month
date_seq <- seq(start_date, end_date, by = "days")
mondays <- date_seq[weekdays(date_seq) == 'Monday']
date1_months=mondays[!duplicated(format(mondays, "%Y-%m"))]

TrafLight <- Official_TrafficLight(TRUE) #Argument:heatmap
risk_k <- risk_TrafficLight(type_traffic_light="discreet_risk") #(n, continuous_risk, discreet_risk)



#------------------------------------------------
#---Official Traffic-Light vs Discreet-risk------
#------------------------------------------------

#--Confusion matrix--
conf_matrix=table(
  as.vector(as.matrix(TrafLight)),
  as.vector(as.matrix(risk_k))
)
print(conf_matrix)



#--Weighted Cohen’s Kappa--
#(statistical measure in which large discrepancies are penalized more heavily than small ones)

for(i in 1:32){
  kappaTest=kappa2(
    cbind(
      as.vector(as.matrix(TrafLight[i])),
      as.vector(as.matrix(risk_n[i]))
    ),
    weight = "squared"
  )
  if(kappaTest$value<0.6){
    print(c(i, kappaTest$value))
  }
}  


subestim=sum(conf_matrix[lower.tri(conf_matrix)])
sobrestim=sum(conf_matrix[upper.tri(conf_matrix)])
exactestim=sum(diag(conf_matrix))

print(c(k, subestim, sobrestim, exactestim))



#----Several comparisons----
summary= data.frame()
for(k in 1: 200){
  TrafLight <- Official_TrafficLight(FALSE) 
  risk_n <- risk_TrafficLight(type_traffic_light="discreet_risk")
    
  conf_matrix=table(
    as.vector(as.matrix(TrafLight)),
    as.vector(as.matrix(risk_n))
  )
  
  subestim=sum(conf_matrix[lower.tri(conf_matrix)])
  sobrestim=sum(conf_matrix[upper.tri(conf_matrix)])
  exactestim=sum(diag(conf_matrix))
  
  kappaTest=kappa2(
    cbind(
      as.vector(as.matrix(TrafLight)),
      as.vector(as.matrix(risk_n))
    ),
    weight = "squared"
  )
  
  new_row <- as.data.frame(as.list(c(k=k, kappa = kappaTest$value, underestim=subestim, overestim=sobrestim, match=exactestim)))
  summary <- rbind(summary, new_row)
}

write.csv(df_risk_filt, paste0("data/risk/discreet_risk_severalK_1-200.csv"), row.names=FALSE)
