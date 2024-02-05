library(tidyverse)

setwd("..")
temp <- read.csv("data/TempMedia1988-2022.csv", header=TRUE, sep = ";", dec = ",")
temp$Tmedia <- as.numeric(temp$Tmedia)
temp_media <- aggregate(Tmedia ~ ANNO, temp, mean, na.rm=TRUE)

ggplot(data=temp_media, aes(x=ANNO,
           y=Tmedia))+
  geom_point()+
  geom_line()+
  geom_smooth(se=FALSE, color="#6a6846", method = lm)+
  labs(x = NULL,
       y = "Temperatura(C°)",
       title = "Temperature medie misurate in Italia (1988-2022)",
       col="#233e30")+
  scale_x_continuous(breaks = unique(temp_media$ANNO))+
  scale_y_continuous(breaks = seq(0,40,1))+
  theme(plot.background = element_rect(fill = "white"),
        axis.text = element_text(color="#233e30"),
        axis.ticks = element_line(color="white"),
        axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("images/italia_temperature_medie.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000) 
