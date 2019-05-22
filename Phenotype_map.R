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
xmltop[[1]]

get_nodes_may_cause <- function(name) {
  top = xmltop[[which(names[] == name)]]
  toptable=ldply(xmlToList(top[[4]]), data.frame)
  
  toptable_Name = toptable[,grepl("*Name",names(toptable))]
  
  toptable_cause <- subset(toptable_Name, toptable_Name$Name=="may cause or feature +")
  stopifnot(nrow(toptable_cause)!=0)
  toptable_phen_leaves = as.data.frame(toptable_cause[,grepl("*Leaf3",names(toptable_cause))])
  toptable_phen_leaves <- toptable_phen_leaves[, complete.cases(t(as.data.frame(toptable_phen_leaves)))]
  
  to<- as.data.frame(toptable_phen_leaves)
  l<-as.character(unlist(to[1,]))
  l<-unique.default(l)
  l
}
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

# Define UI for application that plots functions
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      
      # Select variable for functions
      textAreaInput("phenotypes", "Put your phenotypes", "Kluver-Bucy syndrome
Alzheimer disease
Frontotemporal dementia
Cerebrovascular accident")),
    
    # Outputs
    mainPanel(
      visNetworkOutput("network"),
      DT::dataTableOutput("table"),
      verbatimTextOutput("shiny_return")
      
    )
  )
)
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

recur_parent_list <- function(name){
  list_parents<-sapply(name, function (x){get_nodes_calculator(x)})
  print(list_parents)
  list_parents_1<-Filter(Negate(is.null), list_parents)
  reduce_parents_list <- Reduce(intersect, list_parents_1)
  combinations<-combn(names(name), 2, simplify = F)
  common = sapply(c(1:length(combinations)), function(x){Reduce(intersect, list_parents_1[combinations[[x]]])})
  common
}

get_all_parent_list <- function(name){
  list_parents_1 <- sapply(name, sapply, get_nodes_calculator)
  list_parents_1<-Filter(Negate(is.null), list_parents_1)
  list_parents_2 <- sapply(list_parents_1, sapply, get_nodes_calculator)
  list_parents_2<-Filter(Negate(is.null), list_parents_2)
  list_parents_3 <- sapply(list_parents_2, sapply, get_nodes_calculator)
  list_parents_3<-Filter(Negate(is.null), list_parents_3)
  list_parents_3
}

# Define server function required to create the plot
server <- function(input, output) {
  
  output$network <- renderVisNetwork({
    phen_list <- req(input$phenotypes)
    phen_list <- unlist(strsplit(phen_list, "[\n]"))
    list_parents_1 <- sapply(phen_list, get_nodes_calculator)
    print(list_parents_1)
    list_parents_1<-Filter(Negate(is.null), list_parents_1)
    s <-unlist(list_parents_1)
    list_parents_2 <- sapply(list_parents_1, sapply, get_nodes_calculator)
    list_parents_2<-Filter(Negate(is.null), list_parents_2)
    s_1 <-unlist(list_parents_2)
    l <-recur_parent_list(s_1)
    l<-Filter(Negate(is.null), l)
    l <-l[lapply(l,length)>0] 
    l<-unlist(l)
    tab <- table(l) 
    tab <- as.data.frame(tab)
    
    #maycause <- get_nodes_may_cause(input$phenotype)
    #causedby <-get_nodes_caused_by(input$phenotype)
    node <- data.frame( id = 1:length(tab$l), label = tab$l)
    #node <- data.frame(id = 1:(length(maycause)+length(causedby)+1), label = paste(c(input$phenotype,c(causedby), c(maycause))), group = c(rep("phenotype", 1), rep("causedby",length(causedby)), rep("maycause", length(maycause))))
    
    #edge <- data.frame(from = c(rep.int(1, length(causedby)),c(length(causedby)+2):((length(causedby)+length(maycause)+1))), to = c(2:(length(causedby)+1)),rep.int(1, length(causedby)), arrows = c("to"), shadow= c(TRUE), color = list(color = "blue", highlight = "red"))
    
    #edge <- data.frame(from = c(2:((length(causedby)+length(maycause)+1))), to = rep.int(1, length(maycause)+length(causedby)), arrows = c("to"), shadow= c(TRUE), color = list(color = "blue", highlight = "red"))
    visNetwork(node)%>% 
      visInteraction(multiselect = T)%>% 
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                ;}")%>%
      visLegend() %>%
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(collapse = TRUE)
      #for (i in 1:length(tab$l)){
      #  visGroups(groupname = tab$l[[i]], shape = "square", size = 10*tab$Freq[[i]])
      #}
      #visLayout(hierarchical = TRUE)%>%
    
}) 
  output$shiny_return <- renderPrint({
    visNetworkProxy("network") %>%
      l<- get_nodes_caused_by(input$phenotype)
      node <- data.frame(id = 1:(length(l)+1), label = paste(c(input$click,c(l))))
      selected_node = node[[input$click]]
      print(selected_node)
  })
  
  
  observe({
    input$click
    nodes <- data.frame(id = 1:15, 
                        group = sample(LETTERS[1:5], 15, replace = TRUE))
    edges <- data()$edges
    
    visNetworkProxy("network_proxy_update") %>%
      visUpdateNodes(nodes = nodes) %>%
      visUpdateEdges(edges = edges)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    phen_list <- req(input$phenotypes)
    phen_list <- unlist(strsplit(phen_list, "[\n]"))
    list_parents_1 <- sapply(phen_list, get_nodes_calculator)
    print(list_parents_1)
    list_parents_1<-Filter(Negate(is.null), list_parents_1)
    s <-unlist(list_parents_1)
    list_parents_2 <- sapply(list_parents_1, sapply, get_nodes_calculator)
    list_parents_2<-Filter(Negate(is.null), list_parents_2)
    s_1 <-unlist(list_parents_2)
    l <-recur_parent_list(s_1)
    l<-Filter(Negate(is.null), l)
    l <-l[lapply(l,length)>0] 
    l<-unlist(l)
    tab <- table(l) 
    tab <- as.data.frame(tab)
  }))
  }

# Create a Shiny app object
shinyApp(ui = ui, server = server)


 
