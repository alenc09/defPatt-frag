# Fri Jan 19 15:09:12 2024 ------------------------------
#Script to compare the mean landscape isolatoion

#Libraries----
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
  glimpse-> tab_metricas_prima

#analysis----
##organisation----
met_1985<- tab_metricas_prima[tab_metricas_prima$ano==1985,] 
met_1990<- tab_metricas_prima[tab_metricas_prima$ano==1990,]
met_1995<- tab_metricas_prima[tab_metricas_prima$ano==1995,]
met_2000<- tab_metricas_prima[tab_metricas_prima$ano==2000,]
met_2005<- tab_metricas_prima[tab_metricas_prima$ano==2005,]
met_2010<- tab_metricas_prima[tab_metricas_prima$ano==2010,] 
met_2015<- tab_metricas_prima[tab_metricas_prima$ano==2015,]

l_anos<- list(met_1985, met_1990, met_1995, met_2000, met_2005, met_2010, met_2015)

wilcox_list3<- function(x){
  x<- wilcox.test(ENN_AM ~ padrao, data = x, 
                  alternative= "t", exact = T)
}

##Test----
u_ENNAM_anos<- lapply(l_anos, wilcox_list3)
names(u_ENNAM_anos)<- paste0("ENN_AM ", seq(1985,2015, by=5))
u_ENNAM_anos

#Figure----
ggplot(tab_metricas_prima, aes(x = ano, y = ENN_AM, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  geom_signif(y_position = c(76, 82, 75, 94, 108, 125, 121), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.16","0.05", "0.71", "0.31", "0.53", "0.16", "0.25"),
              tip_length = 0, textsize = 4)+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),breaks= c("geo", "esp"), values = c("grey80","grey40"))+
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "ENN_AM (m)")+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans")) -> bp_ennam
bp_ennam

ggsave(here("img/enn_am.jpg"), plot=bp_ennam, dpi=300)
