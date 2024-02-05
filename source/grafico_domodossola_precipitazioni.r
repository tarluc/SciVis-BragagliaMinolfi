library(tidyverse)

setwd("..")
domodossoladf <- read.csv("data/DOMODOSSOLA_1988.csv", header=TRUE)
domodossoladf$DATA <- as.Date(domodossoladf$DATA)

#Aggrego Anni per fare la media annuale copiandolo in un secondo dataset
domodossoladf$Anno <- format(domodossoladf$DATA,format="%Y")

domodossoladf_precipitazioni_annuali_cumulate <- domodossoladf %>%
  filter(Anno != 1988) %>% #il 1988 e' incompleto
  aggregate(Precipitazione..mm. ~ Anno, sum, na.rm = TRUE)

domodossoladf$nevicate <- ifelse(((domodossoladf$Precipitazione..mm. > 0) & (domodossoladf$Temperatura.media...C. <= 0)),1,0)
domodossoladf_precipitazioni_annuali_cumulate <- aggregate(nevicate ~ Anno, domodossoladf, sum, na.rm = TRUE) %>%
  merge(domodossoladf_precipitazioni_annuali_cumulate)

domodossoladf_precipitazioni_annuali_cumulate$Anno <- as.numeric(domodossoladf_precipitazioni_annuali_cumulate$Anno)

#plot precipitazioni cumulate
ggplot(domodossoladf_precipitazioni_annuali_cumulate, aes(x = Anno, y = Precipitazione..mm.)) +
  geom_bar(stat = "identity", fill = "#33CCFF", alpha = 0.7) +
  geom_smooth(se=FALSE, color="darkgrey", method = lm)+
  scale_x_continuous(breaks = seq(1990,2022,4))+
  theme_minimal() +
  labs(title = "Precipitazioni cumulative per anno Domodossola",
       x = NULL,
       y = "Somma Cumulativa delle Precipitazioni (mm)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ruota etichette sull'asse x per una migliore leggibilità
ggsave("images/precipitazioni_cumulate.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000)

#plot nevicate cumulate
ggplot(domodossoladf_precipitazioni_annuali_cumulate, aes(x = Anno, y = nevicate)) +
  geom_bar(stat = "identity", fill = "#33CCFF", alpha = 0.7) +
  geom_smooth(se=FALSE, color="darkgrey", method = lm)+
  scale_x_continuous(breaks = seq(1990,2022,4))+
  theme_minimal() +
  labs(title = "Giorni con nevicate stimate per anno Domodossola",
       x = NULL,
       y = "Numero totale delle precipitazioni") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota etichette sull'asse x per una migliore leggibilità 
ggsave("images/nevicate_cumulate.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000)
