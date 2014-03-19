msq.long <- read.csv('msq_long.csv', header=T)

library(shiny)

shinyUI(bootstrapPage(
  headerPanel("Shine Mosquito !"), 
  
  
  sidebarPanel( 
    #selectInput(inputId = "specie",
     #           label = "Select specie:",
      #          choices = levels(msq.long$specie)
    #)

    radioButtons("Specie", "Species:",
                 list("DegreeDay1843" = "DegreeDay1843",
                      "Abundance" = "Abundance",
                      "DegreeDayMinus2" = "DegreeDayMinus2",
                      "DominanceBP" = "DominanceBP")),
    br(),
    
    
    radioButtons("Site", "Site:",
                 list("Cfall" = "Cfall",
                      "Edale"="Edale",
                      "Ewing" = "Ewing",
                      "Gildea" = "Gildea",
                      "Gvalley"="Gvalley",
                      "Union" = "Union",
                      "Wloo"="Wloo",
                      "WLP"="WLP"
                      ))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Average", plotOutput("plot1"))    )
  )
  
))
