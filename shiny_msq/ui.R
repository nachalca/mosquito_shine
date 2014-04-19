
library(shiny)
library(reshape2)
source('datasources.R')
shinyUI(bootstrapPage(
#navbarPage("MSQ", id="barnavi",  theme = "bootstrap.min.css",
  titlePanel("Shine Mosquito shine!"), 
    fluidRow(
      sidebarLayout(
        sidebarPanel(width=3,
          conditionalPanel(condition="input.conditionedPanels==1",
                     img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300), 
                     #checkboxGroupInput(inputId = "specie",
                     #     label = "Select specie:",
                     #    choices = levels(msq.long$specie),selected='Aedes.albopictus'),
                    selectInput(inputId = "specie",
                            label = "Select specie:",
                            choices = levels(msq.long$specie),selected='Aedes.vexans'),
     
                    checkboxGroupInput(inputId = "site",
                            label = "Select site:",
                            choices = levels(msq.long$site),selected='Cfall')
                          ),
    
          conditionalPanel(condition="input.conditionedPanels==2",
                           img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),                            
                     checkboxGroupInput(inputId = "specie2",
                                 label = "Select specie:",
                                 choices = levels(msq.long$specie),selected='Aedes.vexans')
                          ),
    
          conditionalPanel(condition="input.conditionedPanels==3",
                           img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),                            
                     checkboxGroupInput(inputId = "index",
                                        label = "Select index:",
                                        choices = lab.index,selected='Simpson'),
                     checkboxGroupInput(inputId = "site3",
                                        label = "Select site:",
                                        choices = levels(msq.long$site),selected='Cfall')
                          ),

          conditionalPanel(condition="input.conditionedPanels==4",
                           img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300), 
                           sliderInput("q", "Quantile", 
                                       min=0, max=100, value=90,step=1),
            selectInput(inputId = "index.X",
                         label = "Select x axis variable",
                         choices = lab.index,selected='PrecipationExact'),
                           
            selectInput(inputId = "index.Y",
                        label = "Select y axis variable",
                        choices = lab.index,selected='DegreeDayExact')
            
          ),
          conditionalPanel(condition="input.conditionedPanels==5",
                           img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300) 
          )
          
                  ), # close sidebarPanel
  
    mainPanel(
        tabsetPanel(
            tabPanel("Average",p(cap1), plotOutput("plot1"),tableOutput("tab1"),value=1),    
            tabPanel("Location", plotOutput("plot2"),value=2), 
            tabPanel("Indexes", plotOutput("plot3"),value=3),
            #tabPanel("Rare Comunities", plotOutput("plot4.1"),plotOutput("plot4.2"),value=4),
            tabPanel("Rare Comunities", p(cap4),plotOutput("plot4"),value=4),
              tabPanel("MDS",p(cap5), ggvis_output("my_plot"),value=5),
            id="conditionedPanels" )
        
            ) # close mainPanel
    
          ) # close sidebarLayout
        ) # close fluidRow
      ) # close fluidPage 
#)for web link 
  ) # close shinyUI


