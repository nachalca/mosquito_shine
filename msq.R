#prueba 2  pal shyny

#install.packages('shiny')
library(shiny)
library(lubridate)
library(reshape2)
library(ggplot2)
library(plyr)

#runApp('shiny_msq')
#runApp('shiny_example')
#library(devtools)
#devtools::install_github(c("hadley/testthat", "rstudio/shiny", "rstudio/ggvis"))


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
msq<- read.csv('shiny_msq/msqdata.csv', header=T)
nam <- paste(colnames(msq)[3:38], 'F', sep='.')
cnd <- colnames(week.fem) %in% nam
week.fem <- cbind(week.fem[,1:4],week.fem[,cnd])

loc.aux<-c('<!-- this file was cached on Aug 06 2014-->','-','2','PE-#1 (1976','PE-#2 (1976','PE-#3 (1976-1977)',
           'PE-#1 (River Park 1978-2010)','<!-- this file was cached on Aug 07 2014-->','0','1','3','DE-W. 32nd St. (2009',
           'PE-#3 (Mosquito Creek 2009)','SY-Horticulture Research Station (2009','PE-#1 (Recycling Center 2011-2013)',
           'SY-State Forest Nursery (shed 2009)')

# obtain 8 locations in the 'reduced' data set

loc8 <- c('BK-Cedar Falls (Hartman Reserve)','BK-Evansdale','PK-Ewing','ST-Gildea','WY-Green Valley','PK-Union','BK-Waterloo','ST-West Lake Park (WLP)')

week.msq <- subset(week.fem, y %in% 1994:2013 & location %in% loc8)

vacio<-function(x) { 
  if ( is.character(x) ) {
        x[x=='-'] <- 'NA' 
        x <- as.numeric(x)
    }
  x
}
weekly.aux<-data.frame(week.msq[,1:4],apply(week.msq[,-(1:4)],2,vacio))

date.lu<-ymd(as.character(weekly.aux$Date))
week.lu<-week(date.lu)
weekly.aux<-data.frame(date.lu,week.lu,weekly.aux)
# weekly: has the total weekly count of each mosquito species in each site-year-week
weekly<-ddply(weekly.aux,.(y,location,week.lu),function(x) apply(x[,-(1:6)],2,sum,na.rm=TRUE))





#3) Ploting weekly data


d <- melt(data=weekly,id.vars=c("y","location","week.lu"))
qplot(data=subset(d,variable=="Aedes.vexans.F"), x=week.lu, y=log(value),geom='line',group=y, facets=~location)

qplot(data=subset(d,variable=="Aedes.vexans.F"), x=week.lu, y=value,geom='boxplot', group=week.lu, facets=~location) + scale_y_log10()


mean.w<-ddply(d,.(location,week.lu,variable),function(x) mean(x$value,na.rm=TRUE))
qplot(data=subset(mean.w,variable=="Psorophora.columbiae.F"), x=week.lu, y=V1,geom='line', facets=~location) + scale_y_log10()
mean.w$location <- factor(mean.w$location)
levels(mean.w$variable) <- gsub(levels(mean.w$variable), patter='.F', replace='')
mean.w$variable <- with(mean.w, reorder(variable, V1, function(x) -mean(x) ))

write.csv(mean.w,file='shiny_msq/meanw.csv',row.names=FALSE)

#=========================================================
#=========================================================
#msq.long data that we use in shiny
#  msq 
msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]
msq.long$subregion <- msq.long$site
levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )

msq.long.aux <- melt(msq[,1:38], id.vars=c('site', 'year') )
colnames(msq.long.aux)[3:4] <- c('specie', 'count')

#msq.long.index <- ddply(.data=msq.long.aux,.variables=c('site','year'),function(x) cbind(x,prop.spst=x$count/sum(x$count)))
#jaja           <- ddply(.data=msq.long,.variables='specie',function(x) mean(x$prop.spst,na.rm=TRUE))
msq.long<-ddply(.data=msq.long.aux,.variables=c('site','year'),function(x) cbind(x,prop.spst=x$count/sum(x$count)))

msq.long$specie <- with( msq.long, reorder(specie, V1, function(x) -sum(x,na.rm=T) ) )

write.csv(msq.long, file='shiny_msq/msq_long.csv', row.names=FALSE)
#=======================================================
#=======================================================          

#####data to run MDS###
#Create a data.frame with MDS's for different distance selection.
library(vegan)
#count mosquito data

# load data sets we use
#setwd("/Users/nataliadasilva//Documents/mosquito_shine/shiny_msq")
msq<- read.csv('msqdata.csv', header=T)
msq.sp <- msq[-c(1:2, 39:53)]
metaMDS(msq.sp, distance = "euclidean",k=3)

f1<-function(k,d){
converge<-FALSE; t<-100
  while(converge==FALSE & t<=1000){
  aux<-metaMDS(msq.sp, distance=d,k=k,autotransform=FALSE, trymax=t) 
  converge<-aux$converged
  t<-t+100
  }
data.frame(stress=aux$stress,conv=aux$converged,aux$points)
}

dis.aux<-expand.grid(d=c("euclidean","canberra", "bray", "jaccard", "horn"),k=2:3)
dis.aux[,1]<-as.character(dis.aux[,1])

mdss<-mdply(dis.aux,f1)

# distances are all the same, I think is because there are many 
# species with very small abundance and only a few species 
# with very high one
# sp.num <- ddply(count, .(year, site), function(x) sum(x[msq] > 0) )
# qplot(data=sp.num,x=1 ,y=V1/length(msq) ,geom='boxplot')                        
# 
# sp.tot <- adply(count[,msq], 2, function(x) data.frame(abu=sum(x),prec=sum(x>0)) )
# sp.tot <- sp.tot[order(sp.tot$abu),]                
# qplot(data=sp.prec,y=V1/160)    
# msq1 <- with(sp.tot, X1[abu>30])
# dist.bc <- vegdist(count[,msq1])
# dist.ho <- vegdist(count[,msq], dist='horn')
# dist.ja <- vegdist(count[,msq], dist='jacard')
# dist.eu <- vegdist(count[,msq], dist='euclidean')
# pairs(dist.bc,dist.ja)

# run mds
#mds.k3  <- metaMDS(count[,msq], k=3,autotransform=FALSE, trymax=200) 
#mds.k2  <- metaMDS(decostand(count[,msq],method='hellinger'),autotransform=FALSE, k=2, trymax=100)                      
#mds.pr2  <- metaMDS(prop,dist='euclidean', k=2,autotransform=FALSE, trymax=100)            
# 
# mds.pr3  <- metaMDS(prop ,dist='euclidean', k=2, autotransform=FALSE, trymax=100) 
# save(mds.pr3,file='mds.RData')
# #load('mds_k3euc.Rdata')
# mds1 <- mds.pr3
# stressplot(mds1)
#
# get the poinst to plot it
# load('mds.Rdata')
# mds1 <- mds.pr3
# mds2 <- data.frame(msq[,1:2], mds1$points, dist=msq$distout)
# write.csv(mds2, file='shiny_msq/mdspoints.csv', row.names=FALSE)
