
library(shiny)

shinyUI(bootstrapPage(
  headerPanel("Shine Mosquito !"), 
  
  sidebarPanel( 
    selectInput(inputId = "specie",
                label = "Select specie:",
                choices = levels(msq.long$specie)
    )

  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Average", plotOutput("plot1"))    )
  )
  
))
