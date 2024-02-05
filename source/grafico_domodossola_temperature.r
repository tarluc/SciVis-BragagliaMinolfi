library(tidyverse)

setwd("..")
domodossoladf <- read.csv("data/DOMODOSSOLA_1988.csv", header=TRUE)
domodossoladf$DATA <- as.Date(domodossoladf$DATA)

#Aggrego Mesi e Anni per fare la media mensile e annuale copiandolo in un secondo dataset
domodossoladf$Mese <- format(domodossoladf$DATA,format="%m")
domodossoladf$Anno <- format(domodossoladf$DATA,format="%Y")

domodossoladf_media_mensile <- aggregate(Temperatura.media...C. ~ Mese + Anno, domodossoladf, mean, na.rm=TRUE)
domodossoladf_media_mensile$DATA <- as.Date(paste(domodossoladf_media_mensile$Mese,domodossoladf_media_mensile$Anno, 1), format='%m %Y %d')
domodossoladf_media_mensile$Anno <- as.numeric(domodossoladf_media_mensile$Anno)

domodossoladf_media_annuale <- domodossoladf %>%
  filter(Anno != 1988) %>% #filtro il 1988 non essendo completo
  aggregate(Temperatura.media...C. ~ Anno, mean, na.rm = TRUE)
domodossoladf_media_annuale <- aggregate(Temperatura.massima...C. ~ Anno, domodossoladf, mean, na.rm = TRUE) %>%
  merge(domodossoladf_media_annuale)
domodossoladf_media_annuale <- aggregate(Temperatura.minima...C. ~ Anno, domodossoladf, mean, na.rm = TRUE) %>%
  merge(domodossoladf_media_annuale)

#plot temperature medie
domodossoladf_media_annuale %>%
  filter(Anno != 2000) %>% #non abbastanza dati questo anno
  pivot_longer(-Anno) %>% 
  ggplot(aes(x = Anno,
             y = value, 
             group = name,
             color = name))+
  geom_point()+
  geom_line()+
  geom_smooth(se=FALSE, color="darkgrey", method = lm, linewidth = 0.3)+
  labs(x = NULL,
       y = "Temperatura(C°)",
       title = "Temperature medie annuali a Domodossola")+
  scale_x_discrete(breaks = seq(1990,2022,4))+
  scale_y_continuous(breaks = seq(-6,30,2))+
  theme(plot.background = element_rect(fill = "#CCCCCC"),
        axis.text = element_text(color="black"),
        axis.ticks = element_line(color="white"),
        axis.ticks.length = unit(-5, "pt"),
        legend.title = element_blank())
ggsave("images/domodossola_media_annuale.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000)

#plot mesi su anni
ggplot(data=domodossoladf_media_mensile,
       aes(x=Mese,
           y=Temperatura.media...C.,
           group=Anno,
           color=Anno))+
    geom_line()+
    geom_hline(yintercept = 0, color="lightblue")+
    scale_x_discrete(labels=month.abb)+
    scale_y_continuous(breaks = seq(-6,30,2))+
    scale_color_gradient2(low = "darkblue", mid = "white",high = "darkred",midpoint = 2005)+
    labs(x = NULL,
         y = "Temperatura media mensile (C°)",
         title = "Temperature medie mensili a Domodossola")+
    theme(panel.background = element_rect(fill="black"),
          plot.background = element_rect(fill = "#CCCCCC"),
          panel.grid = element_blank(),
          axis.text = element_text(color="black"),
          axis.ticks = element_line(color="white"),
          axis.ticks.length = unit(-5, "pt"))
ggsave("images/domodossola_media_mensile.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000)
