#msq<- read.csv('shiny_msq/msqdata.csv', header=T)
#msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]
msq.long <- read.csv('msq_long.csv', header=T)
msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]


library(shiny)
cap1<-'Yearly species proportion per site. The red line represents the mean proportion for each species across sites'

shinyUI(fluidPage(
  titlePanel("Shine Mosquito shine!"), 
  
  
  fluidRow(
  sidebarLayout(
  
  sidebarPanel(width=3,
              
    conditionalPanel(condition="input.conditionedPanels==1",
                     img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300), 
                     #checkboxGroupInput(inputId = "specie",
     #                   label = "Select specie:",
      #                 choices = levels(msq.long$specie),selected='Aedes.albopictus'),
    selectInput(inputId = "specie",
                        label = "Select specie:",
                      choices = levels(msq.long$specie),selected='Aedes.vexans'),
     
    
    checkboxGroupInput(inputId = "site",
                       label = "Select site:",
                       choices = levels(msq.long$site),selected='Cfall')
    ),
    
    conditionalPanel(condition="input.conditionedPanels==2",
                     checkboxGroupInput(inputId = "specie2",
                                 label = "Select specie:",
                                 choices = levels(msq.long$specie),selected='Aedes.vexans')
                     ),
    
    conditionalPanel(condition="input.conditionedPanels==3",
                     checkboxGroupInput(inputId = "site3",
                                        label = "Select site:",
                                        choices = levels(msq.res$site),selected='Cfall')
    
    

  )),
  
  mainPanel(
    
    tabsetPanel(
          tabPanel("Average",p(cap1), plotOutput("plot1"),value=1),    
    tabPanel("Location", plotOutput("plot2"),value=2), 
    tabPanel("Simpson", plotOutput("plot3"),value=3),
    id="conditionedPanels" )
       
    
    )
  
    )
 
))
)
