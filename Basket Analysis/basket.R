library(DBI)
library(RMySQL)
library(arules)
library(arulesViz)

setwd("~/Dropbox/Ripley/Analytics/Basket Analysis")

retarget <- dbConnect(MySQL(),
                      user="big_data", password="analytics",
                      dbname="BigData", host="data.retargeting.cl")
stock <- dbGetQuery(retarget,"select sku from sku;")

data <- read.csv("~/Dropbox/Ripley/Analytics/Basket Analysis/basket.csv")
data$PARTNUM <-as.character(data$PARTNUM)
#data <- read.csv("~/Dropbox/Ripley/Analytics/segmentacion/baskethisstorico.csv")
#data2 <- data[c("MEMBER_ID","ORDERS_ID","UP_NAME")]
#data2 <- data2[which(data2$UP_NAME!="OTROS"),]
#data2 <- data2[which(data2$UP_NAME!="Servicios"),]
#data2 <- data2[which(data2$UP_NAME!="PROMOCIONES"),]
#data2 <- data2[which(data2$UP_NAME!="ACCESORIOS"),]

#data2$MEMBER_ID <- as.factor(data2$MEMBER_ID)
#data2$NAME <- as.factor(data2$NAME)
#str(data2)

write.table(data,file="compras.csv",sep=",",row.names=F)

trans <- read.transactions("compras.csv", format = "single", sep = ",", cols = c("ORDERS_ID", "PARTNUM"),rm.duplicates = TRUE)
#trans2<- read.transactions("compras.csv", format = "single", sep = ",", cols = c("ORDERS_ID", "UP_NAME"),rm.duplicates = TRUE)


#inspeccion
inspect(trans[1:50])
summary(trans)
itemFrequencyPlot(trans, topN = 10)
itemFrequencyPlot(trans, topN = 50)


#Reglas
transrules <- apriori(trans, parameter = list(support =0.0001, confidence = 0.01, maxlen=2,target="rules"))
inspect(head(sort(transrules, by ="lift"),20))
head(quality(transrules))
summary(transrules)
info(transrules)

transrules2 <- as(transrules, "data.frame")
transrules2$rules <- as.character(transrules2$rules)
#transrules2$rules <- gsub("\\}", "", transrules2$rules)
#transrules2$rules <- gsub("\\{", "", transrules2$rules)
transrules2$antecedent <- sub("=.*$", "\\1", transrules2$rules)
transrules2$antecedent <- substr(transrules2$antecedent, 1, nchar(transrules2$antecedent)-1)

transrules2$consequent <- sub(".*$>", "", transrules2$rules)
transrules2$consequent <- gsub("\\=> ", "", transrules2$consequent)
transrules2$consequent <-gsub( " .*$", "", transrules2$consequent )
transrules2 <- transrules2[which(transrules2$antecedent !="" & transrules2$consequent!=""), ]


#appearance = list(lhs=c("HouseOwnerFlag=0", "HouseOwnerFlag=1"))
#saveAsGraph(head(sort(transrules, by="lift"),100), file="rules.graphml")

write(transrules, file = "transrules.csv",sep = ",", quote = TRUE, row.names = FALSE)
plot(transrules, method="graph", control=list(type="items"),interactive=FALSE)


plot(transrules, method="matrix")
plot(transrules, method="grouped")
plot(transrules, method="graph",control=list(type="items"))






