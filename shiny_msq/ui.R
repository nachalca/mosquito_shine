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
                         img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),
                         selectInput(inputId = "site5",
                                              label = "Select site:",
                                              choices = levels(msq.long$site),selected='Cfall'),
                         selectInput(inputId = "year5",
                                     label = "Select year:",
                                     choices = unique(msq$year), 
                                     selected=NULL),
                         
                         selectInput(inputId = "dist5",
                                     label = "Select distance:",
                                     choices = c("euclidean", "canberra", "bray","jaccard","horn"), 
                                     selected="euclidean")
                         
          
                           ), 
          
         conditionalPanel(condition="input.conditionedPanels==6",
                              img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),                            
                              checkboxGroupInput(inputId = "geno",
                                                 label = "Select genotype:",
                                                 choices = unique(msq.long$geno),selected='Aedes'),
                                checkboxGroupInput(inputId = "site6",
                                              label = "Select site:",
                                              choices = levels(msq.long$site),selected='Cfall')
          ),
         conditionalPanel(condition="input.conditionedPanels==7",
                          img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),                            
                          checkboxGroupInput(inputId = "specie7",
                                             label = "Select specie:",
                                             choices = levels(mean.w$variable),selected='Aedes.vexans.F'),
                          checkboxGroupInput(inputId = "site7",
                                             label = "Select site:",
                                             choices = levels(mean.w$location),selected="PK-Ewing")
         )
          
          
                  ), # close sidebarPanel
  
    mainPanel(
        tabsetPanel(
            tabPanel("Species-site",p(cap1), plotOutput("plot1"),tableOutput("tab1"),p(cap1.1),value=1),    
            tabPanel("Genotype-site",p(cap6), plotOutput("plot6"),value=6),
            tabPanel("Location", plotOutput("plot2"),value=2), 
            tabPanel("Rare Comunities", p(cap4),plotOutput("plot4"),value=4),
            tabPanel("MDS",p(cap5), plotOutput("plot5"),value=5),
            tabPanel("Week",p(cap7), plotOutput("plot7"),value=7),
            
            id="conditionedPanels" )
        
            ) # close mainPanel
    
          ) # close sidebarLayout
        ) # close fluidRow
      ) # close fluidPage 
#)for web link 
  ) # close shinyUI


