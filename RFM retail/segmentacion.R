setwd("~/Dropbox/Ripley/Analytics/segmentacion")
#set.seed(321)
#libraries
library(plyr) 
library(bayesm)
library(NMF)
library(psych)
library(e1071)

#working directory

#load files
#baskethisstorico  <- read.csv("~/Dropbox/Ripley/Analytics/segmentacion/baskethistorico.csv")
#save(baskethisstorico,file="baskethistorico2.csv")
load(file="baskethistorico2.csv")
#baskethisstoricocatventas <- read.csv("/Volumes/UNTITLED/baskethisstoricocatventas.csv")
#save(baskethisstoricocatventas,file="baskethisstoricocatventas.csv")
load(file="baskethisstoricocatventas.csv")
categorias <- unique(baskethisstorico[,"NAME"])
# commerceIDRUT <- read.csv("~/Dropbox/Ripley/Analytics/segmentacion/commerceIDRUT.csv")
# save(commerceIDRUT,file="commerceIDRUT.csv")
load(file="commerceIDRUT.csv")
# demo_clientes <- read.csv("~/Dropbox/Ripley/Analytics/segmentacion/demo_clientes.csv")
# save(demo_clientes,file="demo_clientes.csv")
load(file="demo_clientes.csv")

#count each category
baskethisstorico2 <- count(baskethisstorico,c("MEMBER_ID","NAME"))
#long to wide
basketwide <- reshape(baskethisstorico2,
                      timevar = "NAME",
                      idvar = "MEMBER_ID",
                      direction = "wide")

basketwide <- basketwide[sample(nrow(basketwide), 50000), ]
basketwideid <- basketwide[c(1)]
basketwide <- basketwide[c(-1)]
#basketwide[!is.na(basketwide)] <- 1
basketwide[is.na(basketwide)] <- 0

#por depto
  # Determine Number of Factors to Extract
  library(nFactors)
  ev <- eigen(cor(basketwide)) # get eigenvalues
  ap <- parallel(subject=nrow(basketwide),var=ncol(basketwide), rep=100,cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  plotnScree(nS)
fa<-factanal(basketwide,factors=6,rotation="varimax")


#library(GPArotation)
#library(psych)
#fa(basketwide, nfactors=6 ,rotate="varimax",fm="ml")
#fa(basketwide, nfactors=6 ,rotate="oblimin",fm="ml")

nmfSeed()
nmfSeed("nndsvd")


#NMF
#fit.r<-nmf(basketwide, 2:10, method = "lee" ,nrun=40)
#save(fit.r,file="fit.r.rda")

#fit<-nmf(basketwide, 6, method = "lee",nrun=50)
#save(fit,file="fit.rda")
#fit2<-nmf(basketwide, 20, "lee", nrun=500)
#save(fit2,file="fit2.rda")

#load(file="fit.rda")ç
#load(file="fit2.rda")


#modelo
# fit(fit)
# fitted(fit)

#graficos
# coefmap(fit)
# basismap(fit)


#printing the two factor matrices
h<-coef(fit)
h <- t(round(h,3))
h <- as.data.frame(h)
categorias = c()
#name for dimensions
for(i in 1:ncol(h)){
  categorias[i] <- rownames(h)[which.max(h[,i])]
}
typ <- c(1:ncol(h))
segmentos = data.frame(typ,categorias)
w<-basis(fit)
# w2<-basis(fit2)
# w3<-basis(fit3)
# w4<-basis(fit4)
wp<-w/apply(w,1,sum)
head(round(wp,3))
wp2 <- as.data.frame(wp)
# hard clustering
type<-max.col(w)
# type2<-max.col(w2)
# type3<-max.col(w3)
# type4<-max.col(w4)
table(type)
# head(t(aggregate(basketwide, by=list(type), FUN=mean)))
fin <- cbind(basketwide,wp2,type)
fin <-merge(fin,segmentos,by.x="type",by.y="typ",all.x=TRUE)
write.table(table(fin$categoria),file="segmentos.csv")

member_segmento <-subset(fin,select=c("MEMBER_ID","categorias"))
write.csv(member_segmento,file="member_segmento.csv",row.names=FALSE)
#save(member_segmento,file="member_segmento.rda")










#transformar variables
demo_clientes$hombre[demo_clientes$Sexo=="M"] <- TRUE
demo_clientes$mujer[demo_clientes$Sexo=="F"] <- TRUE
demo_clientes$soltero[demo_clientes$Est_Civil=="SOLTERO"] <- TRUE
demo_clientes$casado[demo_clientes$Est_Civil=="CASADO"] <- TRUE
demo_clientes$GSEabc1A[demo_clientes$DCRM_GLS_GSE == "ABC1" & demo_clientes$Renta_Estimada > mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "ABC1"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEabc1B[demo_clientes$DCRM_GLS_GSE == "ABC1" & demo_clientes$Renta_Estimada < mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "ABC1"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEc2A[demo_clientes$DCRM_GLS_GSE == "C2" & demo_clientes$Renta_Estimada > mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "C2"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEc2B[demo_clientes$DCRM_GLS_GSE == "C2" & demo_clientes$Renta_Estimada < mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "C2"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEc3A[demo_clientes$DCRM_GLS_GSE == "C3" & demo_clientes$Renta_Estimada > mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "C3"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEc3B[demo_clientes$DCRM_GLS_GSE == "C3" & demo_clientes$Renta_Estimada < mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "C3"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEDA[demo_clientes$DCRM_GLS_GSE == "D" & demo_clientes$Renta_Estimada > mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "D"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEDB[demo_clientes$DCRM_GLS_GSE == "D" & demo_clientes$Renta_Estimada < mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "D"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEEA[demo_clientes$DCRM_GLS_GSE == "E" & demo_clientes$Renta_Estimada > mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "E"],na.rm=TRUE)] <-TRUE
demo_clientes$GSEEB[demo_clientes$DCRM_GLS_GSE == "E" & demo_clientes$Renta_Estimada < mean(demo_clientes$Renta_Estimada[demo_clientes$DCRM_GLS_GSE == "E"],na.rm=TRUE)] <-TRUE
#edad
anno <- as.numeric(format(Sys.Date(),format="%Y"))
demo_clientes$Fec_Nacimiento <- as.Date(demo_clientes$Fec_Nacimiento,format="%Y-%m-%d")
demo_clientes$Fec_Nacimiento <- anno - as.numeric(format(demo_clientes$Fec_Nacimiento,format="%Y"))
demo_clientes$edad_0_18[demo_clientes$Fec_Nacimiento <= 18] <- TRUE
demo_clientes$edad_19_25[demo_clientes$Fec_Nacimiento > 18 & demo_clientes$Fec_Nacimiento <= 25] <- TRUE
demo_clientes$edad_26_35[demo_clientes$Fec_Nacimiento > 25 & demo_clientes$Fec_Nacimiento <= 35] <- TRUE
demo_clientes$edad_36_45[demo_clientes$Fec_Nacimiento > 35 & demo_clientes$Fec_Nacimiento <= 45] <- TRUE
demo_clientes$edad_46_55[demo_clientes$Fec_Nacimiento > 45 & demo_clientes$Fec_Nacimiento <= 55] <- TRUE
demo_clientes$edad_56_65[demo_clientes$Fec_Nacimiento > 55 & demo_clientes$Fec_Nacimiento <= 65] <- TRUE
demo_clientes$edad_65[demo_clientes$Fec_Nacimiento > 65] <- TRUE

#arreglos
demo_clientes <- subset(demo_clientes,select=-c(Sexo,Est_Civil,Renta_Estimada,DCRM_GLS_GSE,Indicador_Hijos,Fec_Nacimiento))
demo_clientes[is.na(demo_clientes)] <- FALSE
ncol(demo_clientes)
for(i in 2:ncol(demo_clientes)){
  demo_clientes[[i]] <- as.factor(demo_clientes[[i]])
}

#Juntar datos de tablon con commerce
basketwideid2 <-merge(fin,commerceIDRUT,by.x="MEMBER_ID",by.y="USERS_ID",all.x=TRUE)
basketwideid2$LOGONID <- gsub(".$", "", basketwideid2$LOGONID) 
basketwideid2 <-merge(basketwideid2,demo_clientes,by.x="LOGONID",by.y="Rut_Cliente",all.x=TRUE)







ncol(fin)
#clasificador

names(basketwideid2)
names(basketwideid2[,c(69:89)])
basketwideid2$categorias <- as.factor(basketwideid2$categorias)
factor(basketwideid2$categorias)
clasificador <- naiveBayes(basketwideid2[,c(69:89)], basketwideid2$categorias,laplace=0)
clasificador2 <- naiveBayes(fin[,c(4:24)], fin$categorias,laplace=100)

clasificador$apriori
clasificador2$apriori
clasificador$tables
clasificador2$tables

####PRUEBA
# test_pred <- fin[,c(4:24,83)]
# test_pred <- test_pred[sample(nrow(test_pred), 3000), ]
# pred1 <- predict(clasificador,test_pred[,c(1:21)])
# pred2 <- predict(clasificador2,test_pred[,c(1:21)])
# table(pred1==test_pred$categorias)
# table(pred2==test_pred$categorias)

test_pred <- fin[,c(4:24,83)]
test_pred <- test_pred[sample(nrow(test_pred), 3000), ]
pred1 <- predict(clasificador,test_pred[,c(1:21)],type=c("raw"))
pred2 <- predict(clasificador2,test_pred[,c(1:21)],type=c("raw"))
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
pred1 <- colnames(pred1)[apply(pred1, 1, maxn(1))]
pred2 <- colnames(pred2)[apply(pred2, 1, maxn(1))]
table(pred1==test_pred$categorias)
table(pred2==test_pred$categorias)

################
names(demo_clientes[,c(2:22)])
a<-demo_clientes[,c(2:22)]
a <- a[sample(nrow(a), 5000), ]
b<- predict(clasificador,a,type=c("raw"))
#c <- colnames(b)[max.col(b)]
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

segmentos.clientes = data.frame(segmento1=colnames(b)[apply(b, 1, maxn(1))],segmento2=colnames(b)[apply(b, 1, maxn(2))],segmento3=colnames(b)[apply(b, 1, maxn(3))],segmento4=colnames(b)[apply(b, 1, maxn(4))])
table(segmentos.clientes$segment)
a<- data.frame(=segmentos.clientes$segmento1)
b<- data.frame(seg=segmentos.clientes$segmento2)
c<- data.frame(seg=segmentos.clientes$segmento3)
d<- data.frame(seg=segmentos.clientes$segmento4)
seg2 <-rbind(a,b)
table(seg2)


