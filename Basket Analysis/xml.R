library(XML)
doc <- xmlTreeParse("transrules.xml",useInternalNodes=TRUE)
rootNode <- xmlRoot(doc)

rootNode[[3]]
rootNode[["AssociationModel"]][["AssociationRule"]]
rootNode[["AssociationModel"]][["AssociationRule"]][]

tweetid <- xpathSApply(rootNode,"/consequent",xmlValue)

