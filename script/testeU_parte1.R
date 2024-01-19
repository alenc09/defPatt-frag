setwd("C:/Users/User/OneDrive/Documentos/Mestrado/INPE/R/teste_u")

#importar tabela com métricas
tab_metricas_prima<- read.delim("C:/Users/User/OneDrive/Documentos/Mestrado/INPE/planilha_metricas_prima.csv", sep=";", dec = ",")
tab_metricas_prima
str(tab_metricas_prima)

#criar tabelas com métricas para cada data
met_1985<- tab_metricas_prima[tab_metricas_prima$ano==1985,] 
met_1990<- tab_metricas_prima[tab_metricas_prima$ano==1990,]
met_1995<- tab_metricas_prima[tab_metricas_prima$ano==1995,]
met_2000<- tab_metricas_prima[tab_metricas_prima$ano==2000,]
met_2005<- tab_metricas_prima[tab_metricas_prima$ano==2005,]
met_2010<- tab_metricas_prima[tab_metricas_prima$ano==2010,] 
met_2015<- tab_metricas_prima[tab_metricas_prima$ano==2015,]

#Test U para cobertura florestal
l_anos<- list(met_1985, met_1990, met_1995, met_2000, met_2005, met_2010, met_2015)
l_anos
wilcox_list<- function(x){
  x<- wilcox.test(areaha_florprima ~ padrao, data = x, 
  alternative= "t", exact = T)
  }
u_cfanos<- lapply(l_anos, wilcox_list)
names(u_cfanos)<- paste0("Cobertura florestal ", seq(1985,2015, by=5))
u_cfanos
capture.output(u_cfanos, file = "testeU_cf.txt")

#Teste U para número de fragmentos florestais
wilcox_list2<- function(x){
  x<- wilcox.test(nfrag_total ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_nfrag_anos<- lapply(l_anos, wilcox_list2)
names(u_nfrag_anos)<- paste0("Número de fragmentos ", seq(1985,2015, by=5))
u_nfrag_anos
capture.output(u_nfrag_anos, file = "testeU_nfrag.txt")

#Teste U para ENN_AM
wilcox_list3<- function(x){
  x<- wilcox.test(ENN_AM ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_ENNAM_anos<- lapply(l_anos, wilcox_list3)
names(u_ENNAM_anos)<- paste0("ENN_AM ", seq(1985,2015, by=5))
u_ENNAM_anos
capture.output(u_ENNAM_anos, file = "testeU_ENNAM.txt")

#Teste U para ENN_AM sem outlier
wilcox_list4<- function(x){
  x<- wilcox.test(ENN_AM ~ padrao, data = x[c(-7,-11),], 
                  alternative= "t", exact = T)
}
u_ENNAM_out<- lapply(l_anos, wilcox_list4)
names(u_ENNAM_out)<- paste0("ENN_AM ", seq(1985,2015, by=5))
u_ENNAM_out

#Teste U para PAFARC
tab_metricas_prima$PAFRAC<- as.numeric(tab_metricas_prima$PAFRAC)
wilcox_list5<- function(x){
  x<- wilcox.test(PAFRAC ~ padrao, data = x, 
                  alternative= "t", exact = T)
}

u_PAFRAC_anos<- lapply(l_anos, wilcox_list5)
names(u_PAFRAC_anos)<- paste0("PAFRAC ", seq(1985,2015, by=5))
u_PAFRAC_anos
capture.output(u_PAFRAC_anos, file = "testeU_PAFRAC.txt")
