
# MDS from mosquito data: identify rare communities and understand why are rare. 

# set packages we need
library(vegan)
library(ggplot2)
library(reshape2)
library(plyr)
library(MASS)
library(xtable)

count  <- read.csv('shiny_msq/msqdata.csv', header=T)

# where are the mosquito columns? 
msq <- colnames(count)[-c(1:2, 39:52)]
env <- colnames(count)[c(39:52)]

# Compute species proportion on each site*year and the mid-community
prop <- (count[,msq]) /apply(count[,msq],1,sum, na.rm=T)
mid.comu <- apply(prop, 2, mean)

# compute distance from each site*year to the mid-community
dist.out <- as.matrix(vegdist( rbind(prop,mid.comu),dist='euclidean' ) ,nrow=161) 
count$distout <- dist.out[-161,161]
prop2 <- cbind(count[,c('year','site', env, 'distout')], prop )

# compute quantiles for that distancs, Q90 is the cutoff 
qs  <- round(quantile(prop2$distout, probs=seq(0,1,.1)),3)
q90 <- quantile(prop2$distout, probs=.9) 

# The data consist in 152 observations from 8 Sites along 19 years, for each site-year we have the total count of 38 of mosquito different species. 
# The goal now, is to identify rare or outliers observations in terms of the mosquito population. To do this we will first describe the mean population composition in the data and then we will compute distance from each observation to that mean. 
# We want to consider differences among sites based on the species composition, however most of the distance measures are sensitive to the total abundance (two sites with different total count will be consider different even if they present the same species composition structure). To avoid this, we should work with proportions instead of total counts. 

n <- factor(names(mid.comu))
nam <- reorder(n, mid.comu)
mid <- data.frame(mid.comu, nam)
qplot(data=subset(mid, mid.comu > .001 & mid.comu<.5),y=nam, x=mid.comu, size=I(3), xlab='Mean Proportion', ylab='')
#qplot(data=mid,y=nam, x=mid.comu, size=I(3), xlab='Mean Proportion', ylab='')

# The ''Mean community'' is obtained averaging the proportion of each species acrross all observations. The results shows the mean population is very concentrated among a couple of species. In particular the AEDES.VEXAN mean proportion is 81\%. 
# Figure \ref{midplot} presents the proportions for the more abundant speceis after Vexans (vexan is not in the plot to appreciate the differences among the rest). 
# Also without considering the vexan the population is really concnetrated, among the resting 37 species, only 5 represent more than 1\% on average. The second more abundant species is Culex.pipens with an average proportion of 8\%. 
# We would like to identify whcih year-sites observations are far from the mean comunity structure. So we compute the euclidean distance among all sites with respect to the middle point (euclidean is ok since we are using proportions not the raw counts.). 
# We plot a estimated density for the distances from the mean in figure \ref{dens}, the estiamted density is clarlly skewed to right. This is telling us that most of the points are arround the mean value (arround a .2 distance from the center) but there are some few year-sites observations appart from the rest. 

d <- density(c(-count$dist.out[-153,153],count$dist.out[-153,153]),from=0,bw=.04)
qplot(x=d$x, y=d$y, geom='line', xlab='Distance', ylab='Density')

qplot(data=count, DegreeDayMinus2,PrecipationMinus2)

#qplot(data=count,x=factor(year),y=distout, geom='boxplot')
#qplot(data=count,x=factor(site),y=distout, geom='boxplot') + geom_hline(yintercept=q90)

#The main interes is to identify those year-sites. we can also take a look to a box plot (figure \ref{box}) where we observe the same pattern. 

xtable(t( data.frame(qs) ), caption='Quantile for Distance from center')

  qplot(x='x',y=dist.out[-153,153], geom=c('boxplot'), xlab='', ylab='Distance' )+  coord_flip()

#Finally, we compute the quantile for the distance. The 90\% quantile is .25 we choose this value as cut point, this implies that the 'rare' communities are the 10\% furthest from the "mean community" across all site-years observations. The table bellow show some descriptives values for these communities. 

#s <- subset(prop2, distout >q90, select=c(1:2,5:10,29,37))
  # colnames(s)[c(3,4,7,10)] <- c('Abnd','Rich','AVrat','Culex') 
  #xtable(s, caption='Rare community descriptions')
  # Plot showing the population structure by Rare vs Common groups
mid.comu <- data.frame( apply(prop2[prop2$distout>q90,-c(1:16)], 2, mean),                                      apply(prop2[prop2$distout<=q90,-c(1:16)], 2, mean))
colnames(mid.comu) <- c('rare','common') 
n <- factor(row.names(mid.comu))
nam <- reorder(n, mid.comu$common)
mid <- data.frame(mid.comu, nam, row.names=NULL)
mid2 <- melt(subset(mid,rare>.0001), id.vars='nam')
qplot(data=mid2, y=nam, x=value, size=I(3),color=variable,shape=variable, xlab='Mean Proportion', ylab='')

compare <- function(x,dis,q) {
    x1 <- x[dis>q,]
    x2 <- x[dis<=q,]
    aux <- t.test(x1,x2)
    data.frame(rare=aux$estimate[1],common=aux$estimate[2], 
               t.stat=aux$statistic, pval=aux$p.value, row.names=NULL) 
  }
env.comu <-adply(.data=prop2[,3:15],.margins=2, .fun=compare, dis=prop2$distout, q=q90)
xt<-xtable(env.comu, digits=c(0,0,2,2,2,4), caption='Means comparison for Rare and Common comunities')
print(xt, caption.placement='top', include.rownames=FALSE)

# All the 'rare' sites are populations where the Aedes.Vexans represent a very low proportion compared with the mean across all sites, and at the same time the Culex species is very high portion of the whole community. 
# Also, there are 3 sites that are asociated with these types of population structure: Green Valley, Gildea and Evensdale, together are 13 of the 16 rare communities. 

d <- melt(data=count, id.vars=c('site', 'year'), measure.vars=env)
aux <- ddply(d ,.(variable),function(x) anova(lm(value~site+year,data=x)))
data.frame(var=env, site=round(with(aux, aux[Df==7,6] ),3),year=round(with(aux, aux[Df==1,6] ),3)    
            
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
           
           mds.pr3  <- metaMDS(prop,dist='euclidean', k=3,autotransform=FALSE, trymax=100) 
           mds.pr3
           
  load('mds_k3euc.Rdata')
  stressplot(mds1)
  mds1
           
  # get the poinst to plot it
  #mds1 <- mds.pr3
  mds2 <- data.frame(count[,1:2], mds1$points, dist=count$distout)
  mds2$rare <- mds2$dist > q90
  qplot(data=mds2, MDS1, MDS2, color=rare)
           
           #write.csv(mds2, 'mdscc.csv', row.names=F)
           #save(mds1, file='mds_k3euc.Rdata')           
           
           # Regress Enviromental variables on the MDS 
           msq.env <- envfit(mds1, count[,env],choices=c(1:3) ,999)
           env <- data.frame(msq.env$vectors$arrows,r2=msq.env$vectors$r,pval=msq.env$vectors$pval)
           xt <- xtable(env, caption='Enviromaental Fit on MDS 3 axis solution')
           print(xt, caption.placement='top')
           plot(mds1)
           plot(msq.env, p.max=0.05)