
library(shiny)
library(ggplot2)
library(plyr)

shinyServer(
  function(input, output) {
  msq.long <- read.csv('msq_long.csv', header=T)
 
  sp.yr.fun<-function(x){
    prop.sp.yr<-ifelse(sum(x$count)>0,x$count/sum(x$count),0)
  
  dat<-cbind(x,prop.sp.yr)
  }
  
    prop.sp.yr<- ddply(.data=msq.long,.variables=c('site','specie'),.function=sp.yr.fun)
  d  <- reactive( {subset(msq.long, (specie %in%input$specie)&(site%in%input$site) )})

  #output$summary <- renderTable({ summary(d1())

    
  output$plot1 <- reactivePlot(function() {    
    print(ggplot(data=d(), aes(x=year,y=prop),color=site)+geom_point(size=4)+geom_line() +facet_grid(facets=specie~site))
     
    # print(qplot(data=d(), x=year,y=count,color=site,facet=site~.))
    
    })
})