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
      selectInput(inputId = "phenotype", 
                  label = "Choose phenotype:",
                  choices = names)
      
    ),
    
    # Outputs
    mainPanel(
      visNetworkOutput("network"),
      DT::dataTableOutput("table"),
      verbatimTextOutput("shiny_return")
      
    )
  )
)

# Define server function required to create the plot
server <- function(input, output) {
  
  output$network <- renderVisNetwork({
    
    maycause <- get_nodes_may_cause(input$phenotype)
    causedby <-get_nodes_caused_by(input$phenotype)

    node <- data.frame(id = 1:(length(maycause)+length(causedby)+1), label = paste(c(input$phenotype,c(causedby), c(maycause))), group = c(rep("phenotype", 1), rep("causedby",length(causedby)), rep("maycause", length(maycause))))
    
    #edge <- data.frame(from = c(rep.int(1, length(causedby)),c(length(causedby)+2):((length(causedby)+length(maycause)+1))), to = c(2:(length(causedby)+1)),rep.int(1, length(causedby)), arrows = c("to"), shadow= c(TRUE), color = list(color = "blue", highlight = "red"))
    
    edge <- data.frame(from = c(2:((length(causedby)+length(maycause)+1))), to = rep.int(1, length(maycause)+length(causedby)), arrows = c("to"), shadow= c(TRUE), color = list(color = "blue", highlight = "red"))
    visNetwork(node, edge)%>% 
      visInteraction(multiselect = T)%>% 
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                ;}")%>%
      visGroups(groupname = "phenotype", shape = "square", size = 35)%>%
      visGroups(groupname = "causedby", color = "darkblue", size = 15)%>%
      visGroups(groupname = "maycause", color = "red", size = 15)%>%
      #visLayout(hierarchical = TRUE)%>%
      visLegend() %>%
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(collapse = TRUE)
    
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
    top = xmltop[[which(names[] == input$phenotype)]]
    toptable=ldply(xmlToList(top[[4]]), data.frame)
    toptable_Name = toptable[,grepl("*Name",names(toptable))]
    toptable_cause <- subset(toptable_Name, toptable_Name$Name=="may cause or feature +")
    toptable_phen_leaves = toptable_cause[,grepl("*Leaf3",names(toptable_cause))]
    toptable_phen_leaves
  }))
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

 
