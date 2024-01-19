setwd("C:/Users/Lucas/OneDrive/Documentos/Mestrado/INPE/R/parte_1")
install.packages("ggplot2")

library("ggplot2")

#importar tabela e separar tabelas
tab_PAFRAC<- read.delim("PAFRAC_prima.csv", sep=";", dec = ",", 
                        colClasses = c("character", "factor", "factor", "numeric")
                        )
tab_PAFRAC
str(tab_PAFRAC)
tab_PAFRAC$padrao<- factor(tab_PAFRAC$padrao, levels = c("geo", "esp"), ordered = T)


#fazer boxplot para um padrão
bp_pafrac<- ggplot(tab_PAFRAC, aes(x = ano, y = PAFRAC, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  scale_x_discrete(name = "Year") + 
  scale_y_continuous(name = "PAFRAC", breaks = seq(1,1.6,0.15), limits = c(1,1.6))+
  theme_classic()+
  theme(text = element_text(size=12,  family="sans"))+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),breaks= c("geo", "esp"), values = c("grey80","grey20"))+
  geom_signif(y_position = c(1.43, 1.46, 1.45, 1.42, 1.44, 1.44, 1.43), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.006", "0.002", "0.002", "0.01", "0.31", "0.20", "0.07"),
              tip_length = 0, textsize = 2.5)

bp_pafrac
ggsave("C:/Users/Lucas Cordeiro Alenc/OneDrive/Documentos/Mestrado/dissertacao/1art_LE/pafrac_eng.png", plot=bp_pafrac, width = 174,
       height = 129, units = "mm", dpi = 300, limitsize = F, device = png(res = 300, units = "mm"))
