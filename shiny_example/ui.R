
library(shiny)
bird.tot <- read.csv('bird_yeartotal.csv', header=T)
  
shinyUI(bootstrapPage(
  titlePanel("Bird Count Trend"), 
  fluidRow(
    sidebarLayout(
  sidebarPanel(width=3, 
    selectInput(inputId = "specie",
                label = "Select Bird specie:",
                choices = levels( bird.tot$abbrev )
    )

  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Average", plotOutput("plot1")),
      tabPanel("Total", plotOutput("plot2")),
      tabPanel("Summary", tableOutput("summary"))
      
    )
  )
 
    ))
))
