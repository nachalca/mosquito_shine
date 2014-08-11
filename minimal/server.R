

#source('datasources.R')

shinyServer(
  function(input, output,session) {
      
    # data for plot 1
    msq.spyr$pr.up <- msq.spyr$prop.spst>msq.spyr$prop.spyr
    d1  <- reactive( { subset(msq.spyr, (specie %in%input$specie) & (site%in%input$site)) })
    
    # data for table below plot
    d.aux <- ddply(msq.spyr, .(specie,site), summarise, Prop. = mean(prop.spst),Max.prop=max(prop.spst),Sum.above=sum(pr.up))
    
    d1.1  <- reactive({ subset(d.aux, (specie %in%input$specie) & (site%in%input$site)) })
    
    # data for plot 2
#     d2 <- reactive( { subset(msq.ia, specie %in% input$specie2) } )
#       
#     # data for plot 4
#     d4.1 <- reactive( { quantile(msq$distout, probs=input$q/100)} )
#     d4.2 <- reactive({ data.frame(lax = msq[,input$index.X], lay=msq[,input$index.Y],rare=as.factor( msq$distout > d4.1() ) ) })
#     #d4.2 <- reactive({ msq[,c(input$index.X,input$index.Y)] })
#     #d4.3 <- reactive({ rare = as.numeric( msq$distout > d4.1() )   })
#     
#     #data for plot 5
#     d5 <-reactive({ data.frame(mds2,sitecol=as.factor(mds2$site==input$site5),yearcol=as.factor(mds2$year==input$year5) )})
#     # x <-  msq[,as.character(input$mds.color)]
#     # color.var <- cut(x, breaks=quantile(x, probs=c(0,.1,.9,1)), include.lowest=T, labels=c('low','mid','high'))
#     
#     # data for plot 6
#     d6  <- reactive( { subset(msq.geno, (geno %in%input$geno) & (site%in%input$site6)) })
#     
    #==============================================
  output$plot1 <- reactivePlot(function() {    
    print( ggplot(data=d1(), aes(x=year,y=prop.spst),color=site)+geom_point(size=4)+geom_line()+geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_wrap(facets=~site, scales='free')
           + scale_x_continuous("Year") +scale_y_continuous("Proportion of specie"))
  })
    output$tab1 <- renderTable({
      head(d1.1())
   })

  output$cap1<-renderText({'description of plot'})   
#   output$plot2 <- reactivePlot(function() {        
#     ggplot() + geom_polygon(data=ia.c, aes(x=long,y=lat,group=group) )
#   
#     p <- ggplot() + geom_polygon(data=d2(), aes(x=long,y=lat,group=group, order=order, fill=((prop.spst-prop.spyr)/prop.spyr) ) ) +
#        theme_bw() + theme(axis.text=element_blank(), axis.title=element_blank(),
#                        axis.line=element_blank(),
#                        axis.ticks=element_blank(),
#                        panel.border=element_blank(),
#                        panel.grid=element_blank(),
#                        aspect.ratio=1/1.5)
#   print(p + geom_path(data=ia.s, aes(x=long, y=lat, group=group) ) + facet_wrap(~specie) + scale_fill_gradient2( low='black', high='red') + theme(legend.title=element_blank()) ) 
#   })
#   
#     output$plot4 <- renderPlot({
#       p1 <- qplot(x=dat.den$x, y=dat.den$y,geom='line',size=I(1.5)) + 
#         geom_vline(xintercept=d4.1(),color=I('red')) + ylab('') + xlab('Distance to Average Community')
#       p2 <- qplot(data=d4.2(), lax,lay, color=rare)  + scale_color_manual(values=c('black', 'red')) +
#             xlab(as.character(input$index.X)) + ylab(as.character(input$index.Y)) + theme(legend.position='bottom')
#         print( grid.arrange( p1, p2 ,ncol=2 ) )
#     })
#   
#   
# 
# showSite <- function(x) {
#   if (is.null(x)) return(NULL) 
#   xx <- as.numeric(x)
#   xx <- round(xx, 4)
#   ss <-   mds2[ round(mds2$MDS1,4) == xx[3] & round(mds2$MDS2,4) == xx[4],]
#   paste0("<b>",'Site:',ss$site, "</b><br>",
#          'Year:',ss$year, "<br>",
#          'Distance:', round(ss$dist, 3)) 
# }

#aux <- data.frame(mds2, sitecol= as.factor(mds2$site ==  'Gvalley'),color.var=msq[,'Aedes.vexans'] )
#aux2 <- subset(aux, color.var > 2524)


# gv<- reactive({
#   #check<-input_select(unique((mds2$site))) 
#   p <- ggvis(d5() , ~MDS1, ~MDS2, shape=~yearcol ,fill=~sitecol,fill.hover := "red", size.hover := 200 ) 
#   p  %>% layer_points() %>%  add_tooltip(showSite) 
# })
# 
#   gv <- bind_shiny(gv, "my_plot")
#   #output$controls <- renderControls(gv)
#   #observe_ggvis(gv, "my_plot", session)               
#   
# 
# output$plot6 <- reactivePlot(function() {    
#   print( ggplot(data=d6(), aes(x=year,y=prop.geno))+geom_point(size=2)+geom_line(aes(color=site)) +facet_wrap(facets=~geno)
#          + scale_x_continuous("Year") +scale_y_continuous("Proportion of geno"))
# })
#  

})