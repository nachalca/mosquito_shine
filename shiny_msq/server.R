
library(shiny)
library(ggplot2)


shinyServer(
  function(input, output) {
  msq.long <- read.csv('msq_long.csv', header=T)
  d  <- reactive( {subset(msq.long, specie == input$specie&site==input$site)} )
  
  #output$summary <- renderTable({ summary(d1())

    
  output$plot1 <- reactivePlot(function() {    
    print(ggplot(data=d(), aes(x=year,y=count))+geom_point(size=4)+geom_line() +facet_grid(specie~site))
  })
})