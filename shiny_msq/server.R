
library(shiny)
library(ggplot2)
library(plyr)
library(maps)
library(ggplot2)
library(reshape2)
source('datasources.R')

shinyServer(
  function(input, output) {
      
    # data for plot 1
    d1  <- reactive( {subset(msq.spyr, (specie %in%input$specie) & (site%in%input$site) )})
    
    # data for plot 2
    d2 <- reactive( { subset(msq.ia, specie %in% input$specie2) } )
  
    # data for plot 3
    d3 <- reactive( { subset( msq.ind.aux, (site %in% input$site3) & (Index%in%input$index)) } )
    
    # data for plot 4
    d4.1 <- reactive( { quantile(msq$distout, probs=input$q/100)} )
    d4.2 <- reactive({ data.frame(lax = msq[,input$index.X], lay=msq[,input$index.Y],rare=as.factor( msq$distout > d4.1() ) ) })
    #d4.2 <- reactive({ msq[,c(input$index.X,input$index.Y)] })
    #d4.3 <- reactive({ rare = as.numeric( msq$distout > d4.1() )   })
#==============================================
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
  
  output$plot4.1 <- reactivePlot(function() {    
    print( qplot(x=dat.den$x, y=dat.den$y,geom='line')+geom_vline(xintercept=d4.1(),color=I('red'))
    )
  
    })
  
#     output$tab4.2=renderDataTable({
#       summary(d4.2())  
#       })
    output$plot4.2 <- renderPlot({
      print( qplot(data=d4.2(), lax,lay, color=rare))
        #par(mar = c(5.1, 4.1, 0, 1))
        #plot(d4.2(), col = d4.3()$rare,pch = 20, cex = 3)
    })
    output$plot5 <- renderPlot({
        print( grid.arrange( qplot(x=rnorm(40), y=rnorm(40)),qplot(x=rnorm(40), y=rnorm(40)),ncol=2 ) )
        #par(mar = c(5.1, 4.1, 0, 1))
        #plot(d4.2(), col = d4.3()$rare,pch = 20, cex = 3)
    })
  
})