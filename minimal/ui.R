

source('datasources.R')
# # In this code we read data set and create the data to make every plot
# # set packages we need
# library(vegan)
# library(ggplot2)
# library(reshape2)
# library(plyr)
# library(gridExtra)
# #library(shinyAce)
# library(shiny)
# library(maps)
# #library(ggvis)
# #library(animint)
# 
# # load data sets we use
# msq<- read.csv('msqdata.csv', header=T)
# msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]
# 
# msq.long <- read.csv('msq_long.csv', header=T)
# 
# # create subregion variable : match the county
# msq.long$secie2<-as.character(msq.long$specie)
# msq.long$subregion <- msq.long$site
# levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )
# 
# # create yearly proportion for each species (red line) 
# msq.spyr <- ddply(.data=msq.long,.variables=c('year','specie'),function(x) data.frame(x , prop.spyr=mean(x$prop.spst)) )
# msq.ind.aux <- melt(msq[,c(1,2,40:46)],id.vars=c('site', 'year'))
# colnames(msq.ind.aux)[3:4] <- c('Index', 'Index.val')
# 
# 
# #separate the genotype of each specie
# aux.geno <- strsplit(msq.long$secie2,'\\.')
# geno<-NULL
# for(i in 1:length(aux.geno)){
#   geno[i]<-aux.geno[[i]][1]
# }
# msq.long$geno<-geno
# 
# # compute proportion geno proportoin for each year-site
# #x <- subset(msq.long, year==1994 & site=='Cfall')
# msq.geno <- ddply(.data=msq.long,.variables=c('year','site') ,function(x) tapply(x$count, x$geno, sum)/sum(x$count) )
# msq.geno <- melt(msq.geno, id.vars=c('year','site'), variable.name='geno', value.name='prop.geno')
# 
# ia.c <- map_data('county', 'iowa')
# ia.s <- map_data('state', 'iowa')
# ia2 <- subset(ia.c, subregion %in% unique(msq.long$subregion) )
# msq.ia <- merge(msq.spyr,ia2, by= 'subregion')
# 
# # only for UI
# lab.index<-c("Abundance","SpeciesRichness", "DominanceBP","Simpson",  "Shannon"  , "Evenness","AevexansRatio","DegreeDayExact",
#              "PrecipationExact" ,"DegreeDayMinus2","PrecipationMinus2")
# cap1 <-'Yearly species proportion per site. The red line represents the mean proportion for each species across sites'
# cap1.1 <- 'Where Prop is the proportion of a species across all the years, Max.prop is the maximum proportion across all the years
# and Sum.above is the total of years with a proportion above the mean'
# cap4 <-'The right panel shows the density of the distance to the mean community, the red line is the quatile.
# In the left panel each point is a site-year and the red points are the extreme communities.'
# cap5<-'MDS for communities'
# cap6<- 'Compare mosquito species across all years for several sites'
# #density plot for rare communities id
# msq.sp <- colnames(msq)[-c(1:2, 39:52)]
# env   <- colnames(msq)[c(39:52)]
# # Compute species proportion on each site*year and the mid-community
# prop <- (msq[,msq.sp]) /apply(msq[,msq.sp],1,sum, na.rm=T)
# mid.comu <- apply(prop, 2, mean)
# dist.out <- as.matrix(vegdist( rbind(prop,mid.comu),dist='euclidean') ,nrow=161) 
# msq$distout <- dist.out[-161,161]
# dat.den<-density(c(-msq$distout,msq$distout),from=0)
# 
# 
# load('mds.Rdata')
# 
# # get the poinst to plot it
# mds1 <- mds.pr3
# mds2 <- data.frame(msq[,1:2], mds1$points, dist=msq$distout)
# 
# # where are the mosquito columns? 
# msq.da <- colnames(msq)[-c(1:2, 39:52)]
# env <- colnames(msq)[c(39:52)]
# 
# # Compute species proportion on each site*year and the mid-community
# prop <- (msq[,msq.da]) /apply(msq[,msq.da],1,sum, na.rm=T)
# mid.comu <- apply(prop, 2, mean)
# 
# # compute distance from each site*year to the mid-community
# dist.out <- as.matrix(vegdist( rbind(prop,mid.comu),dist='euclidean' ) ,nrow=161) 
# msq$distout <- dist.out[-161,161]
# prop2 <- cbind(msq[,c('year','site', env, 'distout')], prop )
# 
# # compute quantiles for that distancs, Q90 is the cutoff 
# qs  <- round(quantile(prop2$distout, probs=seq(0,1,.1)),3)
# q90 <- quantile(prop2$distout, probs=.9) 
# mds2$rare <- mds2$dist > q90






#======================================================================================
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
                          )
          #,
    
#           conditionalPanel(condition="input.conditionedPanels==2",
#                            img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),                            
#                      checkboxGroupInput(inputId = "specie2",
#                                  label = "Select specie:",
#                                  choices = levels(msq.long$specie),selected='Aedes.vexans')
#                           ),
#           
#           conditionalPanel(condition="input.conditionedPanels==4",
#                            img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300), 
#                            sliderInput("q", "Quantile", 
#                                        min=0, max=100, value=90,step=1),
#             selectInput(inputId = "index.X",
#                          label = "Select x axis variable",
#                          choices = lab.index,selected='PrecipationExact'),
#                            
#             selectInput(inputId = "index.Y",
#                         label = "Select y axis variable",
#                         choices = lab.index,selected='DegreeDayExact')
#             
#            ),
#            conditionalPanel(condition="input.conditionedPanels==5",
#                          img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),
#                          selectInput(inputId = "site5",
#                                               label = "Select site:",
#                                               choices = levels(msq.long$site),selected='Cfall'),
#                          selectInput(inputId = "year5",
#                                      label = "Select year:",
#                                      choices = unique(mds2$year), 
#                                      selected=NULL)
#                          
#           
#                            ) 
          
#   conditionalPanel(condition="input.conditionedPanels==6",
#                               img(src="Mosquito_da_Dengue_by_Lukemaciel.png", height = 100, width = 300),                            
#                               checkboxGroupInput(inputId = "geno",
#                                                  label = "Select genotype:",
#                                                  choices = unique(msq.long$geno),selected='Aedes'),
#                                 checkboxGroupInput(inputId = "site6",
#                                               label = "Select site:",
#                                               choices = levels(msq.long$site),selected='Cfall')
#           )
#           
          
                  ), # close sidebarPanel
  
    mainPanel(
        tabsetPanel(
            tabPanel("Species-site",p(cap1), plotOutput("plot1"),tableOutput("tab1"),p(cap1.1),value=1),    
            #tabPanel("Genotype-site",p(cap6), plotOutput("plot6"),value=6),
            #tabPanel("Location", plotOutput("plot2"),value=2), 
            #tabPanel("Rare Comunities", p(cap4),plotOutput("plot4"),value=4),
            #tabPanel("MDS",p(cap5), ggvisOutput("my_plot"),value=5),
            
            id="conditionedPanels" )
        
            ) # close mainPanel
    
          ) # close sidebarLayout
        ) # close fluidRow
      ) # close fluidPage 
#)for web link 
  ) # close shinyUI


