
# In this code we read data set and create the data to make every plot
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
msq<- read.csv('msqdata.csv', header=T)
msq.res<-msq[,c('site','year','Abundance','SpeciesRichness','DominanceBP', 'Simpson', 'Shannon', 'Evenness', 'AevexansRatio')]

msq.long <- read.csv('msq_long.csv', header=T)
mean.w<-read.csv('meanw.csv',header=T)
mean.w$variable <- with(mean.w, reorder(variable, V1, function(x) -mean(x) ))

# create subregion variable : match the county
msq.long$secie2<-as.character(msq.long$specie)
msq.long$subregion <- msq.long$site
levels(msq.long$subregion) <- c("black hawk", "black hawk", "polk","scott","woodbury","polk","black hawk","scott" )

# create yearly proportion for each species (red line) 
msq.spyr <- ddply(.data=msq.long,.variables=c('year','specie'),function(x) data.frame(x , prop.spyr=mean(x$prop.spst)) )
msq.ind.aux <- melt(msq[,c(1,2,40:46)],id.vars=c('site', 'year'))
colnames(msq.ind.aux)[3:4] <- c('Index', 'Index.val')


#separate the genotype of each specie
aux.geno <- strsplit(msq.long$secie2,'\\.')
geno<-NULL
for(i in 1:length(aux.geno)){
geno[i]<-aux.geno[[i]][1]
}
msq.long$geno<-geno

# compute proportion geno proportoin for each year-site
#x <- subset(msq.long, year==1994 & site=='Cfall')
msq.geno <- ddply(.data=msq.long,.variables=c('year','site') ,function(x) tapply(x$count, x$geno, sum)/sum(x$count) )
msq.geno <- melt(msq.geno, id.vars=c('year','site'), variable.name='geno', value.name='prop.geno')
colnames(msq.geno) <- c('year','site','geno','prop.geno')

ia.c <- map_data('county', 'iowa')
ia.s <- map_data('state', 'iowa')
ia2 <- subset(ia.c, subregion %in% unique(msq.long$subregion) )
msq.ia <- merge(msq.spyr,ia2, by= 'subregion')

# only for UI
lab.index<-c("Abundance","SpeciesRichness", "DominanceBP","Simpson",  "Shannon"  , "Evenness","AevexansRatio","DegreeDayExact",
"PrecipationExact" ,"DegreeDayMinus2","PrecipationMinus2")
cap1 <-'Yearly species proportion per site. The red line represents the mean proportion for each species across sites'
cap1.1 <- 'Where Prop is the proportion of a species across all the years, Max.prop is the maximum proportion across all the years
and Sum.above is the total of years with a proportion above the mean'
cap4 <-'The right panel shows the density of the distance to the mean community, the red line is the quatile.
        In the left panel each point is a site-year and the red points are the extreme communities.'
cap5<-'MDS for communities'
cap6 <- 'Compare mosquito species across all years for several sites'
cap7 <- 'Weekly count in logs for several sites'

#density plot for rare communities id
msq.sp <- colnames(msq)[-c(1:2, 39:52)]
env   <- colnames(msq)[c(39:52)]

# Compute species proportion on each site*year and the mid-community
prop <- (msq[,msq.sp]) /apply(msq[,msq.sp],1,sum, na.rm=T)
mid.comu <- apply(prop, 2, mean)
dist.out <- as.matrix(vegdist( rbind(prop,mid.comu),dist='euclidean') ,nrow=161) 
msq$distout <- dist.out[-161,161]
dat.den<-density(c(-msq$distout,msq$distout),from=0)

### MDS
#mds2 <- read.csv('mdspoints.csv', header=T)

# where are the mosquito columns? 
msq.da <- colnames(msq)[-c(1:2, 39:53)]
env <- colnames(msq)[c(39:52)]

# Compute species proportion on each site*year and the mid-community
prop <- (msq[,msq.da]) /apply(msq[,msq.da],1,sum, na.rm=T)
mid.comu <- apply(prop, 2, mean)

# compute distance from each site*year to the mid-community
dist.out <- as.matrix(vegdist( rbind(prop,mid.comu),dist='euclidean' ) ,nrow=161) 
msq$distout <- dist.out[-161,161]
prop2 <- cbind(msq[,c('year','site', env, 'distout')], prop )

# compute quantiles for that distancs, Q90 is the cutoff 
qs  <- round(quantile(prop2$distout, probs=seq(0,1,.1)),3)
q90 <- quantile(prop2$distout, probs=.9) 
#mds2$rare <- mds2$dist > q90
#write.csv(mds2, 'mdscc.csv', row.names=F)
#save(mds1, file='mds_k3euc.Rdata')           

# Regress Enviromental variables on the MDS 
#msq.env <- envfit(mds2[,c(3,4)], msq[,env],choices=c(1:3) ,999)
#msq.surf <- ordisurf(mds2[,c(3,4)], msq[,env[4]])
#b<-gam(dist~s(mds2$MDS1)+s(mds2$MDS2), data=mds2)
#plot(b,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)
#plot(b,pages=1,seWithMean=TRUE)

# env <- data.frame(msq.env$vectors$arrows,r2=msq.env$vectors$r,pval=msq.env$vectors$pval)
# xt <- xtable(env, caption='Enviromaental Fit on MDS 3 axis solution')
# print(xt, caption.placement='top')
#plot(mds1)
#plot(msq.env, p.max=0.05)
 
### new for poster

# Modelling occurrence of a rare community: 
# 1) Response: Maybe binary (Rare vs Common) or continous (distance from center)
# 2) Exp. Variables: we can use all variables toghether, or we can separate into 
#    groups (g1: indices like,  simpson, shanon; g1: species proportions; g3: site,year,precipitation)
# 3) Stat model: We can use a RF since is really flexible with no assumptions but it also 
# give us a inportance measure for each variable. We can also model with some GLM to obtain 
# numerical measure for the effect of each variables and posibly a characterization of the 
# 90% quantile. 


# rf.dat<-merge(msq[,c(1,2,39:53)],mds2[,c(1,2,6)],by=c('site','year'))
# rf.geno<-dcast(msq.geno,year+site~geno)
# 
# rf.dat<-merge(rf.dat,rf.geno,by=c('site','year'))
# rf.dat$rare<-as.factor(rf.dat$rare)
# library(randomForest)
# 
# rf <- randomForest(rare ~ . -distout, data=rf.dat, importance=T)
# varImpPlot(rf, n.var=10, main='Variable importance for predicting Rare occurrence')
# 
# rf.cont <- randomForest(distout ~ . -rare, data=rf.dat, importance=T)
# varImpPlot(rf.cont , n.var=10, main='Variable importance for predicting Rare occurrence')
# 
# data.frame(round(cor(rf.dat[,-c(1,18)]),2)) 
