library(tidyverse)

setwd("..")
viterboprec <- read.csv("data/PrecCumVT.csv", header=TRUE, sep = ";", dec = ".")
viterbo_prectot <- aggregate(PREC ~ MESE + ANNO, viterboprec, mean, na.rm=TRUE)
prec_sommate <- viterbo_prectot %>%
  group_by(ANNO) %>%
  summarize(sommaprec = sum(PREC))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
prec_moda= getmode(prec_sommate$sommaprec)

ggplot(prec_sommate, aes(x = ANNO, y = sommaprec)) +
  geom_bar(stat = "identity", fill = "#33CCFF", alpha = 0.7) +
  geom_hline(yintercept = prec_moda, color="red")+
  scale_x_continuous(breaks = unique(prec_sommate$ANNO))+
  theme_minimal() +
  labs(title = "Precipitazioni cumulative per Anno",
       x = NULL,
       y = "Somma Cumulativa delle Precipitazioni (mm)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota etichette sull'asse x per una migliore leggibilità
ggsave("images/viterbo_precipitazioni.png", device = "png", plot = last_plot(), units = "px", width = 1500, height = 1000) 
