
library(shiny)
library(ggplot2)
library(plyr)
library(maps)
library(ggplot2)

shinyServer(
  function(input, output) {
    # load data sets we use
    msq<- read.csv('msqdata.csv', header=T)
    msq.long <- read.csv('msq_long.csv', header=T)
    msq.long$subregion <- msq.long$site
    levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )
    msq.spyr <- ddply(.data=msq.long,.variables=c('year','specie'),function(x) data.frame(x , prop.spyr=mean(x$prop.spst)) )
    
    ia.c <- map_data('county', 'iowa')
    ia.s <- map_data('state', 'iowa')
    ia2 <- subset(ia.c, subregion %in% unique(msq.long$subregion) )
    msq.ia <- merge(msq.long,ia2, by= 'subregion')
    
    # data for plot 1
    d1  <- reactive( {subset(msq.spyr, (specie %in%input$specie) & (site%in%input$site) )})
    
    # data for plot 2
    d2 <- reactive( { subset(msq.ia, specie %in% input$specie) } )
  
  output$plot1 <- reactivePlot(function() {    
    print( ggplot(data=d1(), aes(x=year,y=prop.spst),color=site)+geom_point(size=4)+geom_line()+geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_wrap(facets=~site, scales='free')
      )
  })
  output$cap1<-renderText({'description of plot'}) 
  
  output$plot2 <- reactivePlot(function() {        
  p <- ggplot(data=d2(),aes(long,lat))+facet_wrap(~specie) + geom_polygon(aes(group=group, order=order, fill=prop.spst) ) 
  print(p + geom_path(data=ia.s, aes(x=long, y=lat, group=group) ) + scale_fill_gradient2( low='black', high='red', midpoint=0.05) )
  })
  
})