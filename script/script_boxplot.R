setwd("C:/Users/Lucas Alencar/OneDrive/Documentos/Mestrado/INPE/R/parte_1")
install.packages("ggplot2")
install.packages("ggsignif")
install.packages("colorspace")
install.packages("tidyr")

#Bibliotecas####
library(ggplot2)
library(ggsignif)
library(tidyr)

#importar tabela e separar tabelas
tab_metricas_prima<- read.delim("planilha_metricas_prima.csv", sep=";", dec = ",")
tab_metricas_prima
str(tab_metricas_prima)
tab_metricas_prima$padrao<- factor(tab_metricas_prima$padrao, levels = c("geo","esp"), ordered = T)
tab_metricas_prima$ano<- factor(tab_metricas_prima$ano)

#fazer boxplot para ENNAM
bp_ennam<- ggplot(tab_metricas_prima, aes(x = ano, y = ENN_AM, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  scale_x_discrete(name = "Year") + 
  scale_y_continuous(name = "ENN_AM (m)")+
  theme_classic()+
  theme(text = element_text(size=12,  family="sans"))+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),breaks= c("geo", "esp"), values = c("grey80","grey20"))+
  geom_signif(y_position = c(80, 85, 85, 100, 110, 128, 123), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.16","0.05", "0.71", "0.31", "0.53", "0.16", "0.25"),
              tip_length = 0, textsize = 2.5)
bp_ennam
ggsave("C:/Users/Lucas Cordeiro Alenc/OneDrive/Documentos/Mestrado/dissertacao/1art_LE/enn_eng.png", plot=bp_ennam, width = 174,
       height = 129, units = "mm", dpi = 300, limitsize = F, device = png(res = 300, units = "mm"))

#fazer boxplot para número de fragmentos nas paisagens
bp_nfrag<- ggplot(tab_metricas_prima, aes(x = ano, y = nfrag_total, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  scale_x_discrete(name = "Year") + 
  scale_y_continuous(name= "Number of forest fragments", limits = c(0, 1600))+
  theme_classic()+
  theme(text = element_text(size=12,  family="sans"))+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),breaks= c("geo", "esp"), values = c("grey80", "grey20"))+
  geom_signif(y_position = c(400, 900, 1100, 1450, 1400, 1600, 1600), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.04","0.001", "0.001", "0.006", "0.004", "0.002", "0.004"),
              tip_length = 0, textsize = 2.5)
  
bp_nfrag
ggsave("C:/Users/Lucas Cordeiro Alenc/OneDrive/Documentos/Mestrado/dissertacao/1art_LE/nff_eng.png", plot=bp_nfrag, width = 174,
       height = 129, units = "mm", dpi = 300, limitsize = F, device = png(res = 300, units = "mm"))

#boxplot para cobertura florestal
bp_cf<- ggplot(tab_metricas_prima, aes(x = ano, y = perc_cobert, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  scale_x_discrete(name = "Year") + 
  scale_y_continuous(name= " Forest cover (%)", limits = c(0, 100))+
  theme_classic()+
  theme (text = element_text(size=12,  family="sans"))+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),breaks= c("geo", "esp"), values = c("grey80","grey20"))+
  geom_signif(y_position = c(100, 99, 96, 94, 80, 76, 75), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("1.00", "0.45", "0.20", "0.38", "0.45", "0.53", "0.31"),
              tip_length = 0, textsize = 2.5)
bp_cf
ggsave("C:/Users/Lucas/OneDrive/Documentos/Mestrado/dissertacao/2art_/bp_fc.png", plot=bp_cf, width = 174,
       height = 129, units = "mm", dpi = 300, limitsize = F, device = png(res = 300, units = "mm"))
