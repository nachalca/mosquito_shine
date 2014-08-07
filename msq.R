#prueba 2  pal shyny

#install.packages('shiny')
library(shiny)
library(reshape2)
library(ggplot2)
runApp('shiny_msq')
runApp('shiny_example')
library(devtools)
devtools::install_github(c("hadley/testthat", "rstudio/shiny", "rstudio/ggvis"))

library(reshape)
library(plyr)
#=========================================================
#   WEEKLY DATA FOR 8 SITES

# 1) downlad data from website 
# location codes for the 8 sites we have: 
site.info <- data.frame( 
  namme = c("Cfall","Edale","Ewing","Gildea","Gvalley","Union","Wloo","WLP"),
  code = c(828, 823, 868, 819, 837, 827 ,824, 861) )

# f1 donwload data for all years in one location
f1 <- function(loc) {
  library(XML)
  url <- paste("http://mosquito.ent.iastate.edu/browse_location2.php?locID=",as.character(loc),sep='')
  doc <- htmlParse(url)
  root <- xmlRoot(doc)
  links <- getNodeSet(root, "//a[@href]")
  linksDF <- ldply(links, function(x) xmlAttrs(x)["href"])
  linksDF <- linksDF[ agrep("browse_year2_dl", linksDF$href), , drop=FALSE]
  linksDF$href <- paste('http://mosquito.ent.iastate.edu/',linksDF$href,sep='')
  mdply(linksDF, function(href) read.csv(href, header=T) )
}
# d <- data.frame(loc=site.info$code)
# dd <- mdply(d, f1, .progress=progress_text('-'))
# # save raw data
# save(dd, file='weeklydata.Rdata')

# f2 donwload data from all locations for one year
f2 <- function(y) {
  url <- paste('http://mosquito.ent.iastate.edu/browse_year2_dl.php?y=',as.character(y),sep='')
  read.csv(url, header=T)
}
aux <- f2(1991)  
years <- data.frame(y=1969:2014)
weekdata <- mdply(years, f2, .progress=progress_text('-'))

with(weekdata, table(location, WeekRef))
save(weekdata, file='weeklydata.Rdata')

# 2) Cleaning weekly data
load('weeklydata.Rdata')

#2.1 remove male counts since female is the one who care
f3<- function(nam) {
  nam <- as.character(nam)
  x <-  strsplit(nam , '\\.')[[1]]
  l <- length(x)
  x[l]=='F'
} 
cnd <- mdply(data.frame( nam = colnames(weekdata)) , f3 )
week.fem <- cbind(weekdata[,1:4],weekdata[,cnd$V1])

# 2.2 Retain only relevant spcecies (the noes identified by Mike)
nam <- paste(colnames(msq)[3:38], 'F', sep='.')
cnd <- colnames(week.fem) %in% nam
week.fem <- cbind(week.fem[,1:4],week.fem[,cnd])

#remove uncorrect  locations
week.fem<-week.fem[,-c(1,3,4,5,91,96)]

#=========================================================
#=========================================================
#  msq 
msq<- read.csv('shiny_msq/msqdata.csv', header=T)
msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]
msq.long$subregion <- msq.long$site
levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )

msq.long.index<-ddply(.data=msq.long.aux,.variables=c('site','year'),function(x) cbind(x,prop.spst=x$count/sum(x$count)))

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





