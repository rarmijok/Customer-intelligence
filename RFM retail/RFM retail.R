# loading libraries
library(dplyr)
library(reshape2)
library(zoo)
library(lubridate)
library(ggplot2)
options("scipen" = 10)
options()$scipen
source("~/Dropbox/Ripley/Analytics/RFMretail/fun.between.R")
setwd("~/Dropbox/Ripley/Analytics/RFMretail")

#Cargar detalles transaccionales
load(file="~/Dropbox/Ripley/Analytics/DATA/trx.rda")

#por tiempo de prcesamiento bajar fecha
today <- as.Date(max(trx$Fecha.TRX))
fechacorte <- today-months(12)
trx <- subset(trx,Fecha.TRX > fechacorte)


#Generacion de variables
trx$Precio.Articulo <- as.character(trx$Precio.Articulo)
trx$Precio.Articulo <- as.numeric(trx$Precio.Articulo)
RFM <- aggregate(Precio.Articulo~Comprador,trx,sum) #Sacar compras totales por RUT
RFM2 <- aggregate(Nro..TRX~Comprador,trx,length) #transacciones totales por RUT
RFM3 <- aggregate(Cod..Articulo~Comprador,trx,length) #transacciones totales por RUT
RFM4 <- aggregate(Fecha.TRX~Comprador,trx,max) #Fecha ultima transaccion
RFM5 <- aggregate(Fecha.TRX~Comprador,trx,min) #Fecha primera transaccion


#Cambio de nombre de variables
names(RFM)[2] <- "VENTAS"
names(RFM2)[2] <- "FREQ"
names(RFM3)[2] <- "TRX"
names(RFM4)[2] <- "ULTCOMPRA"
names(RFM5)[2] <- "PRIMCOMPRA"

#UNIR
RFM <- merge(RFM,RFM2,by="Comprador")
RFM <- merge(RFM,RFM3,by="Comprador")
RFM <- merge(RFM,RFM4,by="Comprador")
RFM <- merge(RFM,RFM5,by="Comprador")
rm(RFM2,RFM3,RFM4,RFM5) #Borrar los DF que no se usan


#VARIABLES DE COHORTE
RFM$cohorte <- format(RFM$PRIMCOMPRA, format='%Y-%m')

#GAP
RFM$RECENCIA <- as.numeric(today- RFM$PRIMCOMPRA)
RFM$GAP <- as.numeric(RFM$ULTCOMPRA- RFM$PRIMCOMPRA)
RFM$av.gap=round( as.numeric(RFM$ULTCOMPRA- RFM$PRIMCOMPRA)/RFM$FREQ, 0)

#Ticket medio
RFM$ticket <- round(RFM$VENTAS/RFM$FREQ,0)

# DEFINICION DE FRONTERAS
RFM <- RFM %>%
  mutate(segm.freq=ifelse(between(FREQ, 1, 1), '1',
                          ifelse(between(FREQ, 2, 2), '2',
                                 ifelse(between(FREQ, 3, 3), '3',
                                        ifelse(between(FREQ, 4, 4), '4',
                                               ifelse(between(FREQ, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(RECENCIA, 0, 7), '0-7 dias',
                         ifelse(between(RECENCIA, 8, 30), '8-30 dias',
                                ifelse(between(RECENCIA, 31, 60), '31-60 dias',
                                       ifelse(between(RECENCIA, 61, 120), '61-120 dias',
                                              ifelse(between(RECENCIA, 121, 180), '121-180 dias', '>180 dias')))))) %>%
  
  # creating last cart feature
  #mutate(cart=paste(ifelse(a!=0, 'a', ''),
  #                 ifelse(b!=0, 'b', ''),
  #                 ifelse(c!=0, 'c', ''), sep='')) %>%
  arrange(Comprador)

# DEFINICION DEL ORDEN DE FRONTERAS
RFM$segm.freq <- factor(RFM$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
RFM$segm.rec <- factor(RFM$segm.rec, levels=c('>180 dias','121-180 dias', '61-120 dias', '31-60 dias', '8-30 dias', '0-7 dias'))

#DATOS DEMOGRAFICOS CLIENTES RETAIL
load(file="~/Dropbox/Ripley/Analytics/DATA/demo_clientes.csv")
RFM <- merge(RFM,demo_clientes,by.x="Comprador",by.y="Rut_Cliente",all.x=TRUE)
rm(demo_clientes)
###DESDE ACA
#save(RFM,file="~/Dropbox/Ripley/Analytics/RFMretail/RFM.RDA")
#load(file="~/Dropbox/Ripley/Analytics/RFMretail/RFM.RDA")
today <- as.Date(max(RFM$ULTCOMPRA))

RFM$Sexo[RFM$Sexo=="N"] <- NA
RFM$Sexo[RFM$Sexo==""] <- NA
RFM$Sexo[RFM$Sexo==" "] <- NA
RFM$DCRM_GLS_GSE[RFM$DCRM_GLS_GSE=="SIN GSE"] <- NA


#GRID de ciclo de vida
lcg <- RFM %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()
lcg.matrix <- dcast(lcg, segm.freq ~ segm.rec, value.var='quantity', fun.aggregate=sum)
lcg$porc <- paste(round(lcg$quantity/sum(lcg$quantity)*100, 2), "%", sep="")

#GRID DE CICLO DE VIDA (Q)
ggplot(lcg, aes(x=client, y=quantity, fill=quantity)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=5) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle(paste("Grid de Ciclo de vida a",today ))

#GRID DE CICLO DE VIDA (PORC)
ggplot(lcg, aes(x=client, y=quantity, fill=quantity)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=porc), size=5) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle(paste("Grid de Ciclo de vida a",today ))

## VENTAS
lcg2 <- RFM %>%
  group_by(segm.rec,segm.freq) %>%
  summarise(ventas = mean(VENTAS,ra.rm=TRUE)) %>%
  mutate(client='client') %>%
  ungroup()
lcg2$ventas <- round(lcg2$ventas,digits=0)
lcg2$ventas2 <- paste('$',formatC(lcg2$ventas, big.mark=',', format = 'f',digits=0))
ggplot(lcg2, aes(x=client, y=ventas, fill=ventas)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(ventas)/2, label=ventas2), size=5) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("Grid de Ciclo de vida Por valor pasado")

## TICKET MEDIO
lcg2 <- RFM %>%
  group_by(segm.freq) %>%
  summarise(ticket = mean(ticket,ra.rm=TRUE)) %>%
  mutate(client='client') %>%
  ungroup()
lcg2$ticket <- round(lcg2$ticket,digits=0)
lcg2$ticket2 <- paste('$',formatC(lcg2$ticket, big.mark=',', format = 'f',digits=0))
ggplot(lcg2, aes(x=client, y=ticket, fill=ticket)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(ticket)/2, label=ticket2), size=5) +
  facet_grid(segm.freq ~ .) +
  ggtitle("Ticket promedio por frecuencia de compra")

#Sexo
lcg.sub <- RFM %>%
  group_by(Sexo, segm.rec) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()
lcg.sub <- lcg.sub[!is.na(lcg.sub$Sexo),]
lcg.sub2 <- lcg.sub %>%
  group_by(segm.rec) %>%
  summarise(TOT=sum(quantity)) %>%
  ungroup()
lcg.sub <- merge(lcg.sub,lcg.sub2,by="segm.rec",all.x = TRUE) 
lcg.sub$perc <- lcg.sub$quantity/lcg.sub$TOT*100
ggplot(na.omit(lcg.sub), aes(x=Sexo, y=perc, fill=Sexo)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity' , alpha=0.6) +
  facet_grid(. ~ segm.rec) +
  ggtitle("Sexo por Recencia de compra")

##### TICKET MEDIO COHORTE
lcg.coh <- RFM %>%
  group_by(cohorte) %>%
  summarise(quantity=n(),ticket=sum(ticket)) %>%
  mutate(client='client',ticket=round(ticket/quantity, 2)) %>%
  ungroup()
ggplot(lcg.coh, aes(x=cohorte, fill=cohorte)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=ticket), stat='identity', alpha=0.6) +
  geom_text(aes(y=ticket, label=round(ticket,0)), size=4) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("Ticket medio por Cohorte de Primera compra")



