library(shiny)
library(ggplot2)
library("XML")
library(visNetwork)
library("RColorBrewer")
library(DT)


dt <- xmlInternalTreeParse("disease_prod.xml")
# get nodes with names of the items
nodes <- xpathApply(dt, "//DiRooItem/Name")
# get text (names) from the same nodes
names <- xpathSApply(dt, "//DiRooItem/Name", xmlValue)
gsub(" information", "", names)
xmltop = xmlRoot(dt)
xmltop[[1]][[4]][[1]][[3]]

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
      DT::dataTableOutput("table")
      
    )
  )
)

# Define server function required to create the plot
server <- function(input, output) {
  
  output$network <- renderVisNetwork({
    
    top = xmltop[[which(names[] == input$phenotype)]]
    #top = xmltop[[which(names[] == "Kluyvera information")]]
    #top = xmltop[[1]]
    toptable=ldply(xmlToList(top[[4]]), data.frame)
    
    toptable_Name = toptable[,grepl("*Name",names(toptable))]

    toptable_cause <- subset(toptable_Name, toptable_Name$Name=="may cause or feature +")
    stopifnot(nrow(toptable_cause)!=0)
    toptable_phen_leaves = as.data.frame(toptable_cause[,grepl("*Leaf3",names(toptable_cause))])
    toptable_phen_leaves <- toptable_phen_leaves[, complete.cases(t(as.data.frame(toptable_phen_leaves)))]
    
    to<- as.data.frame(toptable_phen_leaves)
    l<-as.character(unlist(to[1,]))
    l<-unique.default(l)

    node <- data.frame(id = 1:(length(l)+1), label = paste(c(input$phenotype,c(l))))
    edge <- data.frame(from = c(2:((length(l)+1))), to = rep.int(1, length(l)), arrows = c("to"), shadow= c(TRUE), color = list(color = "blue", highlight = "red"))
    visNetwork(node, edge, width = "100%")
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

