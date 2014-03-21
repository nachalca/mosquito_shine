msq.long <- read.csv('msq_long.csv', header=T)

library(shiny)

shinyUI(bootstrapPage(
  headerPanel("Shine Mosquito !"), 
  
  
  sidebarPanel( 
    
    checkboxGroupInput(inputId = "specie",
                        label = "Select specie:",
                       choices = levels(msq.long$specie),selected='Aedes.albopictus'),
    
    checkboxGroupInput(inputId = "site",
                       label = "Select site:",
                       choices = levels(msq.long$site),selected='Cfall')
    
    
    

  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Average", plotOutput("plot1"))    )
  )
  
))
