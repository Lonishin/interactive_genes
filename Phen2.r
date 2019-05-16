library(shiny)
library(ggplot2)
library("XML")
library(visNetwork)
library("RColorBrewer")
library(plyr)
library(DT)
library(tidyr)


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

recur_parent_list <- function(name){
  list_parents<-sapply(name, function (x){get_nodes_calculator(x)})
  print(list_parents)
  list_parents_1<-Filter(Negate(is.null), list_parents)
  reduce_parents_list <- Reduce(intersect, list_parents_1)
  combinations<-combn(names(name), 2, simplify = F)
  common = sapply(c(1:length(combinations)), function(x){Reduce(intersect, list_parents_1[combinations[[x]]])})
  common
}

recur_parent_list(name)

#если хотим отдельные симптомы сравнить по наличию у них общих предков, то тогда их и пихаем в функцию
Reduce(intersect, list_parents_1[c(1, 2)])

phen_list_2<-c("Hepatic failure", "Obesity")

recur_parent_list(phen_list_2)
number = 3 # how many generations
get_all_parent_list <- function(name){
  list_parents_1 <- sapply(name, sapply, get_nodes_calculator)
  list_parents_1<-Filter(Negate(is.null), list_parents_1)
  list_parents_2 <- sapply(list_parents_1, sapply, get_nodes_calculator)
  list_parents_2<-Filter(Negate(is.null), list_parents_2)
  list_parents_3 <- sapply(list_parents_2, sapply, get_nodes_calculator)
  list_parents_3<-Filter(Negate(is.null), list_parents_3)
  list_parents_3
}
# лист листов связей
parents_all <-get_all_parent_list(name)  

parents<- get_all_parent_list(parents_all)

#через unlist

list_parents_1 <- sapply(name, get_nodes_calculator)
list_parents_1<-Filter(Negate(is.null), list_parents_1)
s <-unlist(list_parents_1)
list_parents_2 <- sapply(list_parents_1, sapply, get_nodes_calculator)
list_parents_2<-Filter(Negate(is.null), list_parents_2)
s_1 <-unlist(list_parents_2)
l <-recur_parent_list(s_1)
l<-Filter(Negate(is.null), l)
l[lapply(l,length)>0]
 
