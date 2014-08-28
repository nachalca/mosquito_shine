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
                                       min=50, max=100, value=90,step=1),
                           selectInput(inputId = "k4",
                                       label = "Select dimension:",
                                       choices = c("2", "3",'4','5','6','7','8','9','10'), 
                                       selected="2"),
                           
                           selectInput(inputId = "s4",
                                       label = "Select Max.number of random starts:",
                                       choices = c("50", "100",'200','500'), 
                                       selected="2"),
                           
                           selectInput(inputId = "dist4",
                                       label = "Select distance:",
                                       choices = c("euclidean", "canberra", "bray","jaccard","horn"), 
                                       selected="horn"),   
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
                         
                         selectInput(inputId = "k5",
                                     label = "Select dimension:",
                                     choices = c("2", "3",'4','5','6','7','8','9','10'), 
                                     selected="2"),
                         
                         selectInput(inputId = "s5",
                                     label = "Select Select Max.number of random starts:",
                                     choices = c("50", "100",'200','500'), 
                                     selected="2"),
                         
                         selectInput(inputId = "dist5",
                                     label = "Select distance:",
                                     choices = c("euclidean", "canberra", "bray","jaccard","horn"), 
                                     selected="horn")
                         
          
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
                          checkboxGroupInput(inputId = "site7",
                                             label = "Select site:",
                                             choices = levels(mean.w$location),selected="PK-Ewing"),
                          checkboxGroupInput(inputId = "specie7",
                                             label = "Select specie:",
                                             choices = levels(mean.w$variable),selected='Aedes.vexans')
         )
          
          
                  ), # close sidebarPanel
  
    mainPanel(
        tabsetPanel(
            tabPanel("Week",p(cap7),plotOutput("plot7"),value=7),
            tabPanel("Species-site",p(cap1), plotOutput("plot1"),tableOutput("tab1"),p(cap1.1),value=1),    
            tabPanel("Genotype-site",p(cap6), plotOutput("plot6"),value=6),
            #tabPanel("Location", plotOutput("plot2"),value=2), 
            tabPanel("Rare Comunities", p(cap4),plotOutput("plot4"), plotOutput('plot_rf'),value=4),
            tabPanel("MDS",p(cap5), ggvisOutput("my_plot"),value=5),  
            id="conditionedPanels" )
        
            ) # close mainPanel
    
          ) # close sidebarLayout
        ) # close fluidRow
      ) # close fluidPage 
#)for web link 
  ) # close shinyUI


