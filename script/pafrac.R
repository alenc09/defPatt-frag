# Fri Jan 19 14:25:14 2024 ------------------------------
#script to compare PAFRAC metric between GEO and FSH

#Libraries----
library(here)
library(dplyr)
library(ggplot2)
library(ggsignif)

#Data----
read.csv(here("data/PAFRAC_prima.csv"), sep=";", dec = ",") -> tab_PAFRAC

##organisation----
tab_PAFRAC %>% 
  mutate(padrao = factor(padrao, levels = c("geo", "esp"), ordered = T),
         ano = as.factor(ano)) %>% 
  glimpse-> tab_PAFRAC

#Analysis----
##organisation----
wilcox_list5<- function(x){
  x<- wilcox.test(PAFRAC ~ padrao, data = x, 
                  alternative= "t", exact = T)
}

met_1985<- tab_PAFRAC[tab_PAFRAC$ano==1985,]
met_1990<- tab_PAFRAC[tab_PAFRAC$ano==1990,]
met_1995<- tab_PAFRAC[tab_PAFRAC$ano==1995,]
met_2000<- tab_PAFRAC[tab_PAFRAC$ano==2000,]
met_2005<- tab_PAFRAC[tab_PAFRAC$ano==2005,]
met_2010<- tab_PAFRAC[tab_PAFRAC$ano==2010,] 
met_2015<- tab_PAFRAC[tab_PAFRAC$ano==2015,]

l_anos<- list(met_1985, met_1990, met_1995, met_2000, met_2005, met_2010, met_2015)

##test----
u_PAFRAC_anos<- lapply(l_anos, wilcox_list5)
names(u_PAFRAC_anos)<- paste0("PAFRAC ", seq(1985,2015, by=5))
u_PAFRAC_anos

#Figure----
ggplot(tab_PAFRAC, aes(x = ano, y = PAFRAC, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  geom_signif(y_position = c(1.41, 1.445, 1.43, 1.40, 1.413, 1.411, 1.411), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.006", "0.002", "0.002", "0.01", "0.31", "0.20", "0.07"),
              tip_length = 0, textsize = 4)+
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "PAFRAC")+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"), breaks= c("geo", "esp"), values = c("grey80","grey40"))+
  theme_classic()+
  theme(text = element_text(size=16,  family="sans")) -> bp_pafrac
bp_pafrac

ggsave(here("img/pafrac.jpg"), plot=bp_pafrac, dpi=300)
