
library(shiny)
library(ggplot2)
library(plyr)

shinyServer(
  function(input, output) {
  msq.long <- read.csv('msq_long.csv', header=T)
  msq.spyr<- ddply(.data=msq.long,.variables=c('year','specie'),function(x) data.frame(x , prop.spyr=mean(x$prop.spst)) )
  d  <- reactive( {subset(msq.spyr, (specie %in%input$specie) & (site%in%input$site) )})
  
  #output$summary <- renderTable({ summary(d1())
    
  output$plot1 <- reactivePlot(function() {    
    print( ggplot(data=d(), aes(x=year,y=prop.spst),color=site)+geom_point(size=4)+geom_line()+geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_grid(facets=specie~site, scales='free')
      )
    # print(qplot(data=d(), x=year,y=count,color=site,facet=site~.))
    
    })
})