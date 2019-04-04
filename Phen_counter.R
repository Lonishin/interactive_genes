library(shiny)
library(ggplot2)
library("XML")
library(visNetwork)
library("RColorBrewer")
library(plyr)
library(DT)


dt <- xmlInternalTreeParse("disease_prod.xml")
# get nodes with names of the items
nodes <- xpathApply(dt, "//DiRooItem/Name")
# get text (names) from the same nodes
names <- xpathSApply(dt, "//DiRooItem/Name", xmlValue)
names<-gsub(" information", "", names)
xmltop = xmlRoot(dt)


get_nodes_caused_by <- function(name) {
  top = xmltop[[which(names[] == name)]]
  toptable=ldply(xmlToList(top[[4]]), data.frame)
  
  toptable_Name = toptable[,grepl("*Name",names(toptable))]
  
  toptable_cause <- subset(toptable_Name, toptable_Name$Name=="may be caused by or feature of +")
  stopifnot(nrow(toptable_cause)!=0)
  toptable_phen_leaves = as.data.frame(toptable_cause[,grepl("*Leaf3",names(toptable_cause))])
  toptable_phen_leaves <- toptable_phen_leaves[, complete.cases(t(as.data.frame(toptable_phen_leaves)))]
  
  to<- as.data.frame(toptable_phen_leaves)
  l<-as.character(unlist(to[1,]))
  l<-unique.default(l)
  l
}
count_phen <- function(name){
  count = 0
  causedby <-get_nodes_caused_by(name)
  for (i in 1:len(causedby)) {
    for (j in 1:len(phen_list)) {
       if (causedby[i] = phen_list[[j]]){
          count = count+1
       }
    }
  }
  count
  
}
phen_list <- read.delim("Phen_list.txt", header = FALSE)
name = as.character(my_data[[1]])
name[1]
get_nodes_caused_by(name[1])
count_phen(name[1])
