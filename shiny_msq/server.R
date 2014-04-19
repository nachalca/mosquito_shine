
library(shiny)
library(ggplot2)
library(plyr)
library(maps)
library(ggplot2)
library(reshape2)
library(ggvis)
library(animint)
source('datasources.R')

shinyServer(
  function(input, output,session) {
      
    # data for plot 1
    d1  <- reactive( {subset(msq.spyr, (specie %in%input$specie) & (site%in%input$site) )})
    #d1.1  <- reactive( {subset(msq.spyr[,c('site','specie','prop.spyr')], (specie %in%input$specie) & (site%in%input$site) )})
    
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
#     output$tab1 <- renderTable({
#       dataset <- d1()
#       data.frame( dim=dim(dataset)[1], min.cnt=dataset$count, min.pr=min(dataset$prop.spyr) )
#   })
output$tab1 <- renderTable({
  summary(d1())
})

  #output$tab1<-renderTable({
    #data.frame(dim(d1()))
   # ddply(d1(), .(factor(site)), summarise, mean=mean(prop.spyr), min=min(prop.spyr))
  #  })

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
  
#   output$plot4.1 <- reactivePlot(function() {    
#     print( qplot(x=dat.den$x, y=dat.den$y,geom='line')+geom_vline(xintercept=d4.1(),color=I('red'))
#     )
#   
#     })  
#     output$plot4.2 <- renderPlot({
#       print( qplot(data=d4.2(), lax,lay, color=rare))
#     })
    output$plot4 <- renderPlot({
      p1 <- qplot(x=dat.den$x, y=dat.den$y,geom='line',size=I(1.5)) + 
        geom_vline(xintercept=d4.1(),color=I('red')) + ylab('') + xlab('Distance to Average Community')
      p2 <- qplot(data=d4.2(), lax,lay, color=rare)  + scale_color_manual(values=c('black', 'red')) +
            xlab(as.character(input$index.X)) + ylab(as.character(input$index.Y)) + theme(legend.position='bottom')
        print( grid.arrange( p1, p2 ,ncol=2 ) )
    })
  
  
# aux<-stressplot(mds.pr3)
# p5.1<-qplot(data=data.frame(aux),x,y)
#p5.2<-qplot(data=mds2, MDS1, MDS2, color=rare)
 # print( grid.arrange( p5.1, p5.2 ,ncol=2 ) )
 
# This function controls the label when a point is clicked


all_values <- function(x) {
  if(is.null(x) || length(x) == 0) return(NULL)
  paste(mds2[mds2$MDS1==x$MDS1&mds2$MDS2==x$MDS2], ": ",subset(mds2$site,mds2$MDS1==x$MDS1),round(x$MDS1,2), ", " , round(x$MDS2,2))
 
 # aux<-which(vals$dataset$MDS1==x[1]& vals$dataset$MDS2==x[2])
  #paste0('site:',vals$dataset$site[aux],collapse=",")
}

# all_values <- function(x) {
#   if (is.null(x)) return(NULL)
#   if (is.null(x$site)) return(NULL)
# tuti_values <- lab()
# lab <- tuti_values[tuti_value$site == x$site, ]
# 
# paste0("<b>", lab$site, "</b><br>",
#        lab$year, "<br>",format(lab$site, big.mark = ",", scientific = FALSE)
# )
# } 

# all_values <- function(x) {
#     idx <- which(mds2$site==x[1] & mds2$year==x[2])
#     hstring <-   paste(mds2$name[idx], collapse=",")
#   hstring
# }

gv<- reactive({
  # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
  # but since the inputs are strings, we need to do a little more work.
  
  # Lables for axes
 
aaa<-qvis(mds2, ~MDS1, ~MDS2, 
               fill.hover := "red", stroke.hover := "black", size.hover := 200, 
               layers = "point") + 
  click_tooltip(all_values)

aaa

})
      
  output$controls <- renderControls(gv)
  observe_ggvis(gv, "my_plot", session)               
  

})