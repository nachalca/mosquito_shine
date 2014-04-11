
# In this code we read data set and create the data to make every plot
# set packages we need
library(vegan)
library(ggplot2)
library(reshape2)
library(plyr)

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

# only for UI
lab.index<-c("Abundance","SpeciesRichness", "DominanceBP","Simpson",  "Shannon"  , "Evenness","AevexansRatio","DegreeDayExact",
"PrecipationExact" ,"DegreeDayMinus2","PrecipationMinus2")
cap1 <-'Yearly species proportion per site. The red line represents the mean proportion for each species across sites'


#density plot for rare communities id
msq.sp <- colnames(msq)[-c(1:2, 39:52)]
env   <- colnames(msq)[c(39:52)]
# Compute species proportion on each site*year and the mid-community
prop <- (msq[,msq.sp]) /apply(msq[,msq.sp],1,sum, na.rm=T)
mid.comu <- apply(prop, 2, mean)
dist.out <- as.matrix(vegdist( rbind(prop,mid.comu),dist='euclidean') ,nrow=161) 
msq$distout <- dist.out[-161,161]
dat.den<-density(c(-msq$distout,msq$distout),from=0)

