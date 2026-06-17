library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)


start_date <- as.Date("2020-06-08")
end_date <- as.Date("2022-04-30") #as.Date("2020-12-31")


#------------------Official traffic light-------------------
traf_light<- read.csv("data/raw/official_traficlight_colors.csv")

#Change rows order
new_order <- c("Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México")
rows <- which(traf_light$estado %in% c("Chiapas","Chihuahua","Ciudad de México","Coahuila","Colima"))
traf_light[rows, ] <- traf_light[rows[match(new_order, traf_light$estado[rows])], ]

traf_light$estado <- toupper(traf_light$estado)
#Cut dataframe
#endweek = which(colnames(traf_light)=="X2021.W43")
traf_light_cut = traf_light#[,c(1, 3:endweek)]

#Change colnames by dates
date_seq <- seq(start_date, end_date, by = "days")
mondays <- date_seq[weekdays(date_seq) == 'Monday']

colnames(traf_light_cut)<- c("state", format(mondays, format="%Y-%m-%d"))

#Chose columns of the first Monday of each month
date1_months=mondays[!duplicated(format(mondays, "%Y-%m"))]

traf_light_final=traf_light_cut[,c("state",as.character(date1_months))]

traf_light_long <- traf_light_final %>%
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
    title = "Full Official traffic-light",
    x = "",
    y = "",
    fill = "Color "
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7)#,
    #axis.text.y = element_text(size = 8)
  )

ggsave("plots/risk/official_trafficlight_comparison.pdf") #,height = 6, #change factor of height to change size of pdf
       #width = 6)















