#prueba 2  pal shyny

install.packages('shiny')
library(shiny)
runApp('shiny_msq')

library(reshape)

#  msq 

msq<- read.csv('shiny_msq/msqdata.csv', header=T)


msq.long.aux <- melt(msq[,1:38], id.vars=c('site', 'year') )
colnames(msq.long.aux)[3:4] <- c('specie', 'count')

msq.long<-ddply(.data=msq.long.aux,.variables=c('site','year'),function(x) cbind(x,prop=x$count/sum(x$count)))
write.csv(msq.long, file='shiny_msq/msq_long.csv', row.names=FALSE)

#data in proporton 









library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  bird.tot <- read.csv('bird_yeartotal.csv', header=T)
  bird.tot$forest <- as.factor(as.character(bird.tot$forest))
  d  <- reactive( {subset(bird.tot, abbrev == input$specie)} )
  d1 <- reactive( {subset(bird.tot, abbrev == input$specie, select=c('ave','count') )} )
  
  output$summary <- renderTable({
    summary(d1())
  })
  
  output$plot1 <- reactivePlot(function() {    
    print(ggplot(data=d(), aes(x=year, y=ave,color=forest))+geom_point(size=4)+geom_line()+geom_smooth()+facet_grid(facets=forest~.) )
  })
  output$plot2 <- reactivePlot(function() {    
    print(ggplot(data=d(), aes(x=year, y=count,color=forest))+geom_point(size=4) +geom_line()+facet_grid(facets=forest~.) )
  })
})




