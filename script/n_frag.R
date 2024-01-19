# Fri Jan 19 13:41:54 2024 ------------------------------
#script to compare the number of fragments between GEO and FSH

#libraries----
library(here)
library(dplyr)
library(ggplot2)
library(ggsignif)



#data----
read.csv(here("data/planilha_metricas_prima.csv"), sep=";", dec = ",") ->tab_metricas_prima

##organization----
tab_metricas_prima %>% 
  mutate(padrao = factor(padrao, levels = c("geo","esp"), ordered = T),
         ano = as.factor(ano)) %>% 
  glimpse

