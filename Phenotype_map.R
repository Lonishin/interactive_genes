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

# function to understand, if this phenotype in Drugs, hormones and biological mediators group
is_pharmaceutical <- function(name){
  top = xmltop[[which(names[] == name)]]
  toptable=ldply(xmlToList(top[[4]]), data.frame)
  toptable_Name = toptable[,grepl("*Name",names(toptable))]
  toptable_cause <- subset(toptable_Name, toptable_Name[3]=="Drugs, hormones and biological mediators")
  if(nrow(toptable_cause) == 0){
    return(FALSE)
  } else{
    return(TRUE)
  }
}

# function to get all nodes from input phenotype which may cause this phenotype
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

# function to get all nodes from input phenotype which caused by this phenotype
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

# Define UI for application 
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      
      helpText("This is a phenotype map, where you can get all connections between input phenotypes.
               See the example below"),
      
      # Select variable for functions
      textAreaInput("phenotypes", "Put your phenotypes", "Kluver-Bucy syndrome
Alzheimer disease
Frontotemporal dementia
Cerebrovascular accident
Endocarditis
")
      # radioButtons("disp", "Settings",
      #              choices = c(Pharmaceutical = "pharm",
      #                          All = "all"),
      #              selected = "all")
      
    ),
    # Outputs
    mainPanel(
      visNetworkOutput("network"),
      DT::dataTableOutput("table"),
      verbatimTextOutput("shiny_return")
      
    )
  )
)

# function which find out if this phenotype has connections with other

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

# function to get all parent nodes from input phenotype in 1 generation

recur_parent_list <- function(name){
  list_parents<-sapply(name, function (x){get_nodes_calculator(x)})
  print(list_parents)
  list_parents_1<-Filter(Negate(is.null), list_parents)
  reduce_parents_list <- Reduce(intersect, list_parents_1)
  combinations<-combn(names(name), 2, simplify = F)
  common = sapply(c(1:length(combinations)), function(x){Reduce(intersect, list_parents_1[combinations[[x]]])})
  common
}

# function to get all parent nodes from input phenotype in 3 generation

get_all_parent_list <- function(name){
  list_parents_1 <- sapply(name, sapply, get_nodes_calculator)
  list_parents_1<-Filter(Negate(is.null), list_parents_1)
  list_parents_2 <- sapply(list_parents_1, sapply, get_nodes_calculator)
  list_parents_2<-Filter(Negate(is.null), list_parents_2)
  list_parents_3 <- sapply(list_parents_2, sapply, get_nodes_calculator)
  list_parents_3<-Filter(Negate(is.null), list_parents_3)
  list_parents_3
}
# function to get table of all connections from phenotypes

get_tab <- function(phen_list){
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
  
}

# Define server function required to create the app
server <- function(input, output) {
  
  
  output$network <- renderVisNetwork({
    phen_list <- req(input$phenotypes)
    tab = get_tab(phen_list = phen_list)
    node <- data.frame( id = 1:length(tab$l), label = tab$l)
        visNetwork(node)%>% 
      visInteraction(multiselect = T)%>% 
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                ;}")%>%
      visLegend() %>%
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(collapse = TRUE)
      
}) 
  output$table <- DT::renderDataTable(DT::datatable({
    phen_list <- req(input$phenotypes)
    tab = get_tab(phen_list = phen_list)
  }, options = list(orderClasses = TRUE)))
  }

# Create a Shiny app object
shinyApp(ui = ui, server = server)
 
