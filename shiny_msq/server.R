
library(shiny)
library(ggplot2)
library(plyr)
library(maps)
library(ggplot2)

shinyServer(
  function(input, output) {
    # load data sets we use
    msq<- read.csv('msqdata.csv', header=T)
    msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]
    msq.long <- read.csv('msq_long.csv', header=T)
    msq.long$subregion <- msq.long$site
    levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )
    msq.spyr <- ddply(.data=msq.long,.variables=c('year','specie'),function(x) data.frame(x , prop.spyr=mean(x$prop.spst)) )
    msq.ind.aux <- melt(msq[,c(1,2,40:46)],id.vars=c('site', 'year'))
    colnames(msq.ind.aux)[3:4] <- c('Index', 'Index.val')
    
    ia.c <- map_data('county', 'iowa')
    ia.s <- map_data('state', 'iowa')
    ia2 <- subset(ia.c, subregion %in% unique(msq.long$subregion) )
    msq.ia <- merge(msq.spyr,ia2, by= 'subregion')
    
    
    # data for plot 1
    d1  <- reactive( {subset(msq.spyr, (specie %in%input$specie) & (site%in%input$site) )})
    
    # data for plot 2
    d2 <- reactive( { subset(msq.ia, specie %in% input$specie2) } )
  
    # data for plot 3
    d3 <- reactive( { subset( msq.ind.aux, (site %in% input$site3) & (Index%in%input$index)) } )
    
  output$plot1 <- reactivePlot(function() {    
    print( ggplot(data=d1(), aes(x=year,y=prop.spst),color=site)+geom_point(size=4)+geom_line()+geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_wrap(facets=~site, scales='free')
      )
  })
  output$cap1<-renderText({'description of plot'}) 
  
  output$plot2 <- reactivePlot(function() {        
    ggplot() + geom_polygon(data=ia.c, aes(x=long,y=lat,group=group) )
  
    p <- ggplot() + geom_polygon(data=d2(), aes(x=long,y=lat,group=group, order=order, fill=((prop.spst-prop.spyr)/prop.spyr) ) ) +
       theme_bw() + theme(axis.text=element_blank(), axis.title=element_blank(),
                       axis.line=element_blank(),
                       axis.ticks=element_blank(),
                       panel.border=element_blank(),
                       panel.grid=element_blank(),
                       aspect.ratio=1/1.5)
  print(p + geom_path(data=ia.s, aes(x=long, y=lat, group=group) ) + facet_wrap(~specie) + scale_fill_gradient2( low='black', high='red') )
  })
  
  output$plot3 <- reactivePlot(function() {    
    print( ggplot(data=d3(), aes(x=year,y= Index.val))+
             geom_point(size=4)+geom_line()+geom_line(aes(x=year,y=Index.val), color=I('red'))+facet_grid(facets=Index~site, scales='free')
  )
  })
  
 
})