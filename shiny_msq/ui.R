msq.long <- read.csv('msq_long.csv', header=T)

library(shiny)
cap1<-'Yearly species proportion per site. The red line represents the mean proportion for each species across sites'

shinyUI(fluidPage(
  titlePanel("Shine Mosquito !"), 
  
  fluidRow(
  sidebarLayout(
  sidebarPanel(width=3,
    
    conditionalPanel(condition="input.conditionedPanels==1",
    #checkboxGroupInput(inputId = "specie",
     #                   label = "Select specie:",
      #                 choices = levels(msq.long$specie),selected='Aedes.albopictus'),
    selectInput(inputId = "specie",
                        label = "Select specie:",
                      choices = levels(msq.long$specie),selected='Aedes.albopictus'),
     
    
    checkboxGroupInput(inputId = "site",
                       label = "Select site:",
                       choices = levels(msq.long$site),selected='Cfall')
    ),
    
    conditionalPanel(condition="input.conditionedPanels==2",
    checkboxGroupInput(inputId = "site",
                       label = "Select site:",
                       choices = levels(msq.long$site),selected='Cfall'))
    
    
    

  ),
  
  mainPanel(
  
    tabsetPanel(
  
      tabPanel("Average",p(cap1), plotOutput("plot1"),value=1),    
    tabPanel("Location", plotOutput("plot2"),value=2), 
    id="conditionedPanels" )
    )))
  
))
