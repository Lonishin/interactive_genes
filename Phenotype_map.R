library(shiny)
library(ggplot2)
library("XML")
library(visNetwork)

setwd("~/Project_database")

dt <- xmlInternalTreeParse("disease_prod.xml")
# get nodes with names of the items
nodes <- xpathApply(dt, "//DiRooItem/Name")
# get text (names) from the same nodes
names <- xpathSApply(dt, "//DiRooItem/Name", xmlValue)
gsub(" information", "", names)
xmltop = xmlRoot(dt)

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
      visNetworkOutput("network")
    )
  )
)

# Define server function required to create the plot
server <- function(input, output) {
  
  # Create the rplot object the plotOutput function is expecting
  output$network <- renderVisNetwork({
    
    # minimal example
    top = xmltop[[which(names[] == input$phenotype)]]
    toptable=ldply(xmlToList(top[[4]]), data.frame)
    n <- data.frame(id = 1:(nrow(toptable)+1), label =paste(c(as.list(levels(toptable$Name)),"Main disease")))
    # e <- data.frame(from = c(1,2), to = c(1,3))
    
    visNetwork(n)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
