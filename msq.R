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
levels(msq.long$specie) <- gsub(levels(msq.long$specie), patter='.F', replace='')

msq.long$specie <- with( msq.long, reorder(specie, count, function(x) -sum(x,na.rm=T) ) )


write.csv(msq.long, file='shiny_msq/msq_long.csv', row.names=FALSE)
#=======================================================
#=======================================================          

#####data to run MDS###
#Create a data.frame with MDS's for different distance selection.
library(vegan)
#count mosquito data

# load data sets we use
#setwd("/Users/nataliadasilva//Documents/mosquito_shine/shiny_msq")
msq<- read.csv('shiny_msq/msqdata.csv', header=T)

msq.sp <- msq[-c(1:2, 39:53)]
prop <- msq.sp /apply(msq.sp,1,sum, na.rm=T)


metaMDS(msq.sp, distance = "bray",trymax=50,k=50,trace=0)

f1<-function(k,d,t){
  if (d!='euclidean') aux <- metaMDS(msq.sp, distance=d,k=k,autotransform=FALSE, trymax=t,trace=0)
  if (d=='euclidean') aux <- metaMDS(prop, distance=d,k=k,autotransform=FALSE, trymax=t,trace=0)
  data.frame(stress=aux$stress,conv=aux$converged,aux$points)
}

#expand grid with thre levels distance, dimensions and size of the run.
dis.aux<-expand.grid(d=c("euclidean","canberra", "bray", "jaccard", "horn"),k=2:10,t=c(50,100,200,500))
dis.aux[,1]<-as.character(dis.aux[,1])

mdss<-mdply(dis.aux,f1,.progress='text')
write.csv(mdss, file='mdss.csv', row.names=FALSE)

mds.summ <- ddply(mdss, .(d,k,t), summarise, stress=mean(stress), conv = mean(conv))
qplot(data=mds.summ,t,stress, color=as.factor(conv), facets=d~k )

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

# ===================================================
# Stuff for the poster 
# set packages we need
library(vegan)
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(shiny)
library(maps)
library(ggvis)

# load data sets we use
#setwd("/Users/nataliadasilva//Documents/mosquito_shine/shiny_msq")
msq     <- read.csv('shiny_msq/msqdata.csv', header=T)
msq.res <-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]
msq.long <- read.csv('shiny_msq/msq_long.csv', header=T)
mean.w<-read.csv('shiny_msq/meanw.csv',header=T)
mean.w$variable <- with(mean.w, reorder(variable, V1, function(x) -mean(x) ))
mdss <- read.csv('shiny_msq/mdss.csv', header=T)

# create yearly proportion for each species (red line) 
msq.long$subregion <- msq.long$site
levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )
msq.spyr <- ddply(.data=msq.long,.variables=c('year','specie'),function(x) data.frame(x , prop.spyr=mean(x$prop.spst)) )
msq.ind.aux <- melt(msq[,c(1,2,40:46)],id.vars=c('site', 'year'))
colnames(msq.ind.aux)[3:4] <- c('Index', 'Index.val')
msq.spyr$pr.up <- msq.spyr$prop.spst>msq.spyr$prop.spyr

# for maps
ia.c <- map_data('county', 'iowa')
ia.s <- map_data('state', 'iowa')
ia2 <- subset(ia.c, subregion %in% unique(msq.long$subregion) )    
msq.ia <- merge(msq.spyr,ia2, by= 'subregion')


# a map for identify locations                
p <- ggplot() + geom_polygon(data=ia.c, aes(x=long,y=lat,group=group, order=order),colour=I('black'),fill=I('white') )  + 
       theme_bw() + theme(axis.text=element_blank(), axis.title=element_blank(),
                       axis.line=element_blank(),
                       axis.ticks=element_blank(),
                       panel.border=element_blank(),
                       panel.grid=element_blank(),
                       aspect.ratio=1/1.5) 

# counties where there are data in general and counties where the 8 locations are. 
ia.c$global <- ia.c$subregion %in% unique(ia.c$subregion)[c(7,14,17,31,52,57,64,77,78,80,82,84,85,90,97)]
locations <- ddply(msq.ia, .(subregion,site), summarize, 
                   long=mean(long), 
                   lat=mean(lat),
                   order=mean(order)
                   )
locations$long2 <- with(locations, long)
locations$lat2 <- with(locations, lat) + c(-.18,0,.12, -.15,.15, -.15,.15,0)

pdf('figs/mapiowa.pdf')
p + geom_path(data=ia.s, aes(x=long, y=lat, group=group) ) + 
  geom_polygon(data=subset(ia.c,global), aes(x=long,y=lat,group=group,order=order),fill=I('grey') ) +
  geom_text(data=locations, aes(x=long2, y=lat2,label=site ),size=I(6), color=I('red'))
dev.off()




# An average spcecies composition ...
d  <- ddply(msq.long, .(specie),summarize, prop=mean(prop.spst) )
pdf('figs/meanprop.pdf')
qplot(data=subset(d,prop>.01), x=specie, y=prop,size=I(5))  + scale_y_log10() + 
  ylab('Proportion (log10 scale)') + theme(axis.text.x = element_text(angle=45, vjust=1))
dev.off()

# Within species: Vexans and Pippens
d <- subset(msq.spyr, specie=='Aedes.vexans')
pdf('figs/vexans.pdf')
ggplot(data=d, aes(x=year,y=prop.spst),size=I(5),color=site) + geom_point(size=2)+geom_line() + 
  geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_wrap(facets=~site, ncol=4) +
 scale_x_continuous("Year") +scale_y_continuous("Proportion of specie")
dev.off()

d <- subset(msq.spyr, specie=='Culex.pipiens.group')
pdf('figs/pipiens.pdf')
ggplot(data=d, aes(x=year,y=prop.spst),size=I(5),color=site) + geom_point(size=2)+geom_line() + 
  geom_line(aes(x=year,y=prop.spyr), color=I('red')) +facet_wrap(facets=~site, ncol=4) +
  scale_x_continuous("Year") +scale_y_continuous("Proportion of specie")
dev.off()

# Between Species .... 


d7 <- subset(mean.w, (variable%in% c("Culiseta.inornata","Culex.pipiens.group","Aedes.trivittatus") & (location %in% "PK-Union")) )
pdf('figs/weekplot.pdf')
qplot(data=d7, x=week.lu, y=V1, color=variable,size=I(5)) +  geom_line() +  xlab('Week') + ylab('Total count (in log scale)' ) +
  scale_y_log10() + theme(legend.position='bottom', legend.title=element_blank(),
                          axis.text.y = element_text(size=rel(1)),
                          axis.title.y = element_text(size=rel(2)),
                          axis.text.x = element_text(size=rel(1)),
                          axis.title.x = element_text(size=rel(2))) 
dev.off()

# Community level 
dst <- ddply(mdss, .(d,k,t), summarize, stress= mean(stress))

# mds using horn distance .... 

pdf('figs/strdim.pdf')
qplot(data=subset(dst,d=='horn' & t==100 & k<6), k,stress,size=I(7))+geom_line() + 
  xlab('Dimension') + theme(axis.text.y = element_text(size=rel(1)),
                            axis.title.y = element_text(size=rel(2)),
                            axis.text.x = element_text(size=rel(1)),
                            axis.title.x = element_text(size=rel(2))) 
dev.off()

m <- metaMDS(msq.sp, distance='horn',k=2, trymax=100,trace=0,autotransform=F)
s <- stressplot(m)

pdf('figs/stresshorn.pdf')
qplot(s$x, s$y, size=I(1),ylab='Ordination distance', xlab='Observed Dissimilarity')
dev.off()

# Determine mean comunity, distance to the mean and rare communities

mid.comu <- ddply(mdss,.(d,k,t), function(x) apply(x[,-c(1:5)],2,mean,na.rm=T))
v.dis<-function(dat1){ 
  n<-dim(dat1)[1]
  data.frame(distout = as.matrix(vegdist(dat1,method='euclidean',na.rm=T),nrow=n)[-n,n])
}
dist.out<-ddply(rbind(mdss[,-c(4:5)],mid.comu),.(d,k,t), v.dis)

dd <- data.frame(scores(m),distout=subset(dist.out, d=='horn' & k==2 &t==100)$distout)
dd$rare <- dd$distout >= quantile(dd$distout,.9)

pdf('figs/mdsplot.pdf')
qplot(data=dd, NMDS1, NMDS2,color=rare,size=I(5)) + 
  theme(legend.position='none', axis.text.x=element_text(size=rel(2)),axis.text.y=element_text(size=rel(2)) ) + scale_color_manual(values=c('black', 'red'))
dev.off()

# use RF to clasify communities

rf.dat <- data.frame(msq,rare=as.factor(dd$distout >= quantile(dd$distout,.9) ))

library(randomForest) 
rf.ind <-  randomForest(rare ~ . , data=rf.dat[,c(40:46,53)], importance=T)
v <- data.frame(varImpPlot(rf.ind, main='Species and genus indices',type=1))
v$variable <- reorder(as.factor(rownames(v)),v$MeanDecreaseAccuracy)
pdf('figs/rfindi.pdf')
qplot(data=v, MeanDecreaseAccuracy,variable, size=I(6),ylab='',xlab='Mean Decrease Accuracy') + 
  theme(axis.text.y = element_text(size=rel(2)),axis.title.x = element_text(size=rel(2)))
dev.off()

rf.abi <-  randomForest(rare ~ . , data=rf.dat[,c(1:2,47:53)], importance=T)
varImpPlot(rf.abi, main='Abiotic factors',type=1)

# Ilustrate shiny interaction ..... 
ddd <- data.frame(msq, rare = dd$distout >= quantile(dd$distout,.9))
pdf('figs/interactive.pdf')
qplot(data=ddd, x=DominanceBP,y=DegreeDayMinus2,color=rare,size=I(5)) + scale_color_manual(values=c('black', 'red')) +
  theme(legend.position='none', axis.text.y = element_text(size=rel(1)),axis.title.y = element_text(size=rel(2)),
        axis.text.x = element_text(size=rel(1)),axis.title.x = element_text(size=rel(2))) 
dev.off()

# table comparing rare and common communities
msq$rare <- dd$distout >= quantile(dd$distout,.9)
with(msq, table(site, rare))

ddd <- melt(msq[,c("Aedes.vexans","Culex.pipiens.group","Abundance" ,"SpeciesRichness","DegreeDayMinus2","PrecipationMinus2",'rare')], id='rare')

comparando <- function(xx) {
  rare <-  with(xx, variable[rare])
  common <-with(xx, variable[!rare])
  tt <- t.test(rare,common)
  #out <- 
    data.frame(c(tt$estimate,tt$p.value))
  #rownames(out) <-c('rare','common','pval')
  #return(out)
}

aux<- ddply(ddd, .(variable), comparando)



