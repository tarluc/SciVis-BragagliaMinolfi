library(tidyverse)

setwd("..")
viterbotmax <- read.csv("data/TempMaxMedVT.csv", header=TRUE, sep = ";", dec = ".")
viterbotmin <- read.csv("data/TempMinMedVT.csv", header=TRUE, sep=";", dec=".")
viterbotmed <- read.csv("data/TempMedVT.csv", header=TRUE, sep=";", dec=".")
Tmax <- viterbotmax %>%
  filter(ANNO != 2000, ANNO != 2001) %>%
  aggregate(VALORE ~ MESE + ANNO, mean, na.rm=TRUE)
viterbo_minmedia <- aggregate(Tmin ~ ANNO, viterbotmin, mean, na.rm=TRUE)
viterbo_media <- aggregate(Tmed ~ ANNO, viterbotmed, mean, na.rm=TRUE)
temp_max <- Tmax %>%
  group_by(ANNO) %>%
  summarize(Tmax = mean(VALORE))%>%
  merge(viterbo_minmedia)
temp_max <- temp_max %>%
  merge(viterbo_media)


temp_max %>%
  pivot_longer(-ANNO) %>%
  ggplot(aes(x=ANNO,
            y=value,
            group=name,
            color = name))+
  geom_point()+
  geom_line()+
  geom_smooth(se=FALSE, color="#6a6846", method=lm)+
  labs(x = NULL,
       y = "Temperatura(C°)",
       title = "Temperature medie annuali Viterbo")+
  scale_x_continuous(breaks = unique(temp_max$ANNO))+
  scale_y_continuous(breaks = seq(0,40,1))+
  theme(plot.background = element_rect(fill = "white"),
        axis.text = element_text(color="#233e30"),
        axis.ticks = element_line(color="white"),
        axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

ggsave("images/TempMaxMinMedVT.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000)
