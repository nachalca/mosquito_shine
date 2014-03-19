
library(shiny)
library(ggplot2)


shinyServer(
  function(input, output) {
  msq.long <- read.csv('msq_long.csv', header=T)
  
  
  #cond<-levels(msq.long$specie)[1:3]
  #sp<-reactive({cond[c(input$Aedes.albopictus,input$Aedes.atropalpus,input$Aedes.canadensis)]})
  d  <- reactive( {subset(msq.long, (specie %in%input$specie)&(site%in%input$site) )})

  #output$summary <- renderTable({ summary(d1())

    
  output$plot1 <- reactivePlot(function() {    
    print(ggplot(data=d(), aes(x=year,y=count),color=site)+geom_point(size=4)+geom_line() +facet_grid(facets=specie~site))
     # print(qplot(data=d(), x=year,y=count,color=site,facet=site~.))
    
    })
})