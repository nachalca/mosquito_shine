#prueba 2  pal shyny

install.packages('shiny')
library(shiny)
library(reshape2)
runApp('shiny_msq')
runApp('shiny_example')

library(reshape)
library(plyr)
#  msq 

msq<- read.csv('shiny_msq/msqdata.csv', header=T)
msq.long$subregion <- msq.long$site
levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )


msq.long.aux <- melt(msq[,1:38], id.vars=c('site', 'year') )
colnames(msq.long.aux)[3:4] <- c('specie', 'count')

msq.long<-ddply(.data=msq.long.aux,.variables=c('site','year'),function(x) cbind(x,prop.spst=x$count/sum(x$count)))
jaja<-ddply(.data=msq.long,.variables='specie',function(x) mean(x$prop.spst,na.rm=TRUE))

write.csv(msq.long, file='shiny_msq/msq_long.csv', row.names=FALSE)

#data in proporton 

library(maps)
library(ggplot2)

ia.c <- map_data('county', 'iowa')
ia.s <- map_data('state')


ia2 <- subset(ia.c, subregion %in% unique(msq$subregion) )
msq.ia <- merge(msq.long,ia2, by= 'subregion')


m <- mean(msq$Culex.pipiens.group)

msq.ia2 <- subset(msq.ia, specie %in% c('Culex.pipiens.group', 'Aedes.dorsalis'))
ggplot(data=msq.ia2 ,aes(long,lat)) +facet_wrap(~specie) + geom_polygon(aes(group=group, order=order, fill=prop.sps) ) +
geom_path(data=ia.s, aes(x=long, y=lat, group=group) )
  


geom_point(data=ia3, aes(long,lat,color=Culex.pipiens.group ))
geom_polygon(data=ia3, aes(group=group, order=order, fill=Culex.pipiens.group) ) +
scale_fill_gradient2( low='black', high='red', midpoint=m)




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



ggplot(data=msq.long, aes(x=year,y=prop.spst),color=site)+geom_point(size=4)+geom_line()+geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_wrap(facets=~site, scales='free')
qplot(data=msq.long,x=year,y=prop.spst,color=specie)

