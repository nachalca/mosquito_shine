
library(shiny)
library(ggplot2)


shinyServer(
  function(input, output) {
  msq <- read.csv('msq_long.csv', header=T)
  d  <- reactive( {subset(msq, specie == input$specie)} )
  #output$summary <- renderTable({ summary(d1())

    
  output$plot1 <- reactivePlot(function() {    
    print(ggplot(data=d(), aes(x=year,y=count,color=site))+geom_point(size=4)+geom_line() )
  })
})