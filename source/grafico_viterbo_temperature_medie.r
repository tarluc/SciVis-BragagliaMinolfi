library(tidyverse)

setwd("..")
viterbodf <- read.csv("data/TempViterboMediaMese.csv", header=TRUE, sep = ";", dec = ".")
viterbo_media <- aggregate(TEMP ~ MESE + ANNO, viterbodf, mean, na.rm=TRUE)
viterbo_media$MESE <- as.character(viterbo_media$MESE)
viterbo_media$DATA <- as.Date(paste(viterbo_media$MESE,viterbo_media$ANNO, 1), format='%m %Y %d')
viterbo_media$MESE <- format(viterbo_media$DATA,format="%m")

ggplot(data=viterbo_media,
        aes(x=MESE,
            y=TEMP,
            group=ANNO,
            color=ANNO))+
    geom_line()+
    geom_hline(yintercept = 0, color="white")+
    scale_x_discrete(labels=month.abb)+
    scale_y_continuous(breaks = seq(-12,30,1))+
    scale_color_viridis_c(option = 'plasma')+
    labs(x = NULL,
        y = "Temperatura media mensile (C°)",
        title = "Temperature medie mensili a Viterbo")+
    theme(panel.background = element_rect(fill="black"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color="black"),
        axis.ticks = element_line(color="white"),
        axis.ticks.length = unit(-5, "pt"))
ggsave("images/viterbo_media_mensile.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000) 
