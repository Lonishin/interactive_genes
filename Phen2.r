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

#модифицированая функция для поиска связанных нодов. Так, чтобы даже в случае пустых списков ничего не падало. Возвращает список.

get_nodes_calculator <- function(x){
  tryCatch(
    expr = {
      return(get_nodes_caused_by(x))
      message("Successfully executed the log(x) call.")
    },
    error = function(e){
      message('Caught an error!')
    },
    warning = function(w){
      message('Caught an warning!')
    },
    finally = {
      message('All done, quitting.')
    }
  )    
}



phen_list <- read.delim("Phen_list.txt", header = FALSE)
name = as.character(phen_list[[1]])
str(name[2])

#если мы хотим применить функцию поиска нодов для вектора с именами, прочитанными из txt. На выходе лист из листов. Если предки есть - то они в виде вектора, если нет - то NULL

list_parents<-sapply(name, function (x){get_nodes_calculator(x)})

# убрать те листы, где нет родителей. Это видимо, терминальные ветки.
list_parents_1<-Filter(Negate(is.null), list_parents)

#если мы хотим найти общие элементы в листах предков всех
Reduce(intersect, list_parents_1)

#если хотим отдельные симптомы сравнить по наличию у них общих предков, то тогда их и пихаем в функцию
Reduce(intersect, list_parents_1[c(1, 2)])

phen_list_2<-c("Hepatic failure", "Obesity")

list_parents_et<-sapply(phen_list_2, function(x){get_nodes_calculator(x)})

Reduce(intersect, list_parents_1[a[[1]]])

library(tidyr)
names(list_parents_et)
a<-combn(names(list_parents_1), 2, simplify = F)

list_parents_1[a[[1]]]

sapply(c(1:length(a)), function(x){Reduce(intersect, list_parents_1[a[[x]]])})
