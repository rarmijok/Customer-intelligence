library(DBI)
library(RMySQL)
library(arules)
setwd("~/Dropbox/Ripley/Analytics/Basket Analysis")

##
retarget <- dbConnect(MySQL(),
                      user="big_data", password="analytics",
                      dbname="BigData", host="data.retargeting.cl")
vistas <- dbGetQuery(retarget,"select p.sku, p.userID
                     from product_views p 
                     inner join sku s on p.sku = s.sku
                     WHERE p.timestamp > UNIX_TIMESTAMP(subdate(current_date, 20));")
productos <- dbGetQuery(retarget,"select s.sku, s.url, s.thumbnail, s.name
                        from sku s;")


apriorialg<- function (data, parameter = NULL, appearance = NULL, control = NULL) 
{
  data <- as(data, "transactions")
  items <- data@data
  if (is(appearance, "list")) 
    appearance <- as(c(appearance, list(labels = itemLabels(data))), 
                     "APappearance")
  appearance <- as(appearance, "APappearance")
  control <- as(control, "APcontrol")
  parameter <- as(parameter, "APparameter")
  if (control@verbose) {
    cat("\nParameter specification:\n")
    print(parameter)
    cat("\nAlgorithmic control:\n")
    print(control)
    cat("\n")
  }
  abs_supp <- as.integer(parameter@support * length(data))
  if (abs_supp < 2) 
    warning(sprintf("Numero de soporte muy bajo. Pueden ocurrir fallas de memoria", 
                    abs_supp), immediate. = TRUE)
  result <- .Call("rapriori", items@p, items@i, items@Dim, 
                  parameter, control, appearance, data@itemInfo, PACKAGE = "arules")
  call <- match.call()
  result@info <- list(data = call$data, ntransactions = length(data), 
                      support = parameter@support, confidence = parameter@confidence)
  if (is(result, "rules")) {
    validObject(result@lhs@data)
    validObject(result@rhs@data)
  }
  else {
    validObject(result@items@data)
  }
  result
}
write.table(vistas,file="vistas.csv",sep=",",row.names=F)
trans <- read.transactions("vistas.csv", format = "single", sep = ",", cols = c("userID", "sku"),rm.duplicates = TRUE)
transrules <- apriorialg(trans, parameter = list(support =0.0001, confidence = 0.0001, maxlen=2,target="rules"))
transrules2 <- as(transrules, "data.frame")
transrules2$rules <- gsub("\\}", "", transrules2$rules)
transrules2$rules <- gsub("\\{", "", transrules2$rules)
transrules2$rules <- as.character(transrules2$rules)
transrules2$antecedent <- sub("=.*$", "\\1", transrules2$rules)
transrules2$antecedent <- substr(transrules2$antecedent, 1, nchar(transrules2$antecedent)-1)
transrules2$consequent <- gsub(".*=>", "", transrules2$rules)
transrules2$consequent <- sub("\\ ","",transrules2$consequent)
transrules2 <- transrules2[which(transrules2$antecedent !="" & transrules2$consequent!=""), ]
transrules2$lift <- as.numeric(transrules2$lift)
transrules2 <- transrules2[order(-transrules2$lift),]
transrules3 <- transrules2[duplicated(transrules2$antecedent),]
transrules3 <- transrules3[!duplicated(transrules3$antecedent),]
transrules2 <- transrules2[!duplicated(transrules2$antecedent),]
transrules2 <- subset(transrules2,select=c("antecedent","consequent"))
transrules3 <- subset(transrules3,select=c("antecedent","consequent"))
transrules2 <- merge(transrules2,productos,by.x="consequent",by.y="sku")
transrules3 <- merge(transrules3,productos,by.x="consequent",by.y="sku")


##### REEMPLAZO EN pviews2
for(i in 1:length(pviews2$email)){
  if(is.na(pviews2$pviews.producto2[i]) & (pviews2$pviews.producto1[i] %in% transrules2$antecedent)){
    posicion2 <- which(transrules2$antecedent %in% pviews2$pviews.producto1[i])
    pviews2$thumbnail.producto2[i] <- transrules2$thumbnail[posicion2]
    pviews2$url.producto2[i] <- transrules2$url[posicion2]
    pviews2$name.producto2[i] <-transrules2$name[posicion2]
    print(posicion2)
  }
}

for(i in 1:length(pviews2$email)){
  if( is.na(pviews2$pviews.producto3[i]) & !is.na(pviews2$pviews.producto2[i]) & (pviews2$pviews.producto1[i] %in% transrules2$antecedent) ){
    posicion3 <- which(transrules2$antecedent %in% pviews2$pviews.producto1[i])
    pviews2$thumbnail.producto3[i] <- transrules2$thumbnail[posicion3]
    pviews2$url.producto3[i] <- transrules2$url[posicion3]
    pviews2$name.producto3[i] <-transrules2$name[posicion3]
    print(posicion3)}
}

for(i in 1:length(pviews2$email)){
  if( is.na(pviews2$pviews.producto3[i]) & is.na(pviews2$pviews.producto2[i]) & (pviews2$pviews.producto1[i] %in% transrules3$antecedent) ){
    posicion3 <- which(transrules3$antecedent %in% pviews2$pviews.producto1[i])
    pviews2$thumbnail.producto3[i] <- transrules3$thumbnail[posicion3]
    pviews2$url.producto3[i] <- transrules3$url[posicion3]
    pviews2$name.producto3[i] <-transrules3$name[posicion3]
    print(posicion3)}
}



