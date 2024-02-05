library(tidyverse)

setwd("..")
formazzadf <- read.csv("data/FORMAZZA_1988.csv", header=TRUE)
formazzadf$DATA <- as.Date(formazzadf$DATA)

#Aggrego Anni per fare la media annuale copiandolo in un secondo dataset
formazzadf$Anno <- format(formazzadf$DATA,format="%Y")

formazzadf_media_annuale <- formazzadf %>%
  filter(Anno != 1988) %>% #filtro il 1988 non essendo completo
  aggregate(Temperatura.media...C. ~ Anno, mean, na.rm = TRUE)
formazzadf_media_annuale <- aggregate(Temperatura.massima...C. ~ Anno, formazzadf, mean, na.rm = TRUE) %>%
  merge(formazzadf_media_annuale)
formazzadf_media_annuale <- aggregate(Temperatura.minima...C. ~ Anno, formazzadf, mean, na.rm = TRUE) %>%
  merge(formazzadf_media_annuale)

#plot temperature medie
formazzadf_media_annuale %>%
  pivot_longer(-Anno) %>% 
  ggplot(aes(x = Anno,
             y = value, 
             group = name,
             color = name))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0, color="lightblue", linewidth = 0.5)+
  geom_smooth(se=FALSE, color="darkgrey", method=lm, linewidth = 0.3)+
  labs(x = NULL,
       y = "Temperatura(C°)",
       title = "Temperature medie mensili a P. Camosci (2453 mslm)")+
  scale_x_discrete(breaks = seq(1990,2022,4))+
  scale_y_continuous(breaks = seq(-6,30,2))+
  theme(plot.background = element_rect(fill = "#CCCCCC"),
        axis.text = element_text(color="black"),
        axis.ticks = element_line(color="white"),
        axis.ticks.length = unit(-5, "pt"),
        legend.title = element_blank())
ggsave("images/formazza_media_annuale.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000)
