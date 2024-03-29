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

##test----
wilcox_list2<- function(x){
  x<- wilcox.test(nfrag_total ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_nfrag_anos<- lapply(l_anos, wilcox_list2)
names(u_nfrag_anos)<- paste0("Numero de fragmentos ", seq(1985,2015, by=5))
u_nfrag_anos

#figure----
ggplot(tab_metricas_prima, aes(x = ano, y = nfrag_total, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  geom_signif(y_position = c(325, 830, 1000, 1400, 1700, 1560, 1590),
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.04","0.001", "0.001", "0.006", "0.004", "0.002", "0.004"),
              tip_length = 0, textsize = 4)+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),breaks= c("geo", "esp"), values = c("grey80", "grey40")) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name= "Number of forest fragments") +
  theme_classic()+
  theme(text = element_text(size=16,  family="sans")) -> bp_nfrag

ggsave(filename = here("img/bp_nfrag.jpg"), plot=bp_nfrag, dpi = 300)
