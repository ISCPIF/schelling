
library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/sugarscape'))
stdtheme= theme(axis.title = element_text(size = 22), 
                axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),
                strip.text = element_text(size=15),
                legend.text=element_text(size=15), legend.title=element_text(size=15))



# single phase diagram

res <- as.tbl(read.csv('exploration/2017_03_28_11_57_40_GRID_FIXED.csv'))

sres = res %>% group_by(population,minSugar,maxSugar)%>%summarise(
  gini=mean(mwgini)
)

g=ggplot(sres,aes(x=population,y=minSugar,fill=gini))
g+geom_raster()+facet_wrap(~maxSugar)
ggsave(filename = 'res/fixed_phase_diagram.png',width = 30,height=20,units = 'cm',dpi = 200)


# varying configs

#res <- as.tbl(read.csv('exploration/2017_03_28_11_59_36_GRID_SYNTHPATTERN.csv'))
res <- as.tbl(read.csv('exploration/grid_synth_test.csv'))
sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(
  gini=mean(mwgini)
)
g=ggplot(sres[sres$id%in%1:10,],aes(x=population,y=minSugar,fill=gini))
g+geom_raster()+facet_grid(maxSugar~id)

# test morpho pca
#duplicated(res[res$id==0,c("spAlpha","spDiffsteps","spDiffusion","spGrowth","spPopulation")])
morph = res %>% group_by(id)%>%summarise(distance=mean(distance),entropy=mean(entropy),moran=mean(moran),slope=mean(slope))

for(j in 2:ncol(morph)){morph[,j]<-(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
#summary(prcomp(morph))
pca=prcomp(morph[,2:ncol(morph)])
rot = data.frame(as.matrix(morph[,2:ncol(morph)])%*%pca$rotation)
g=ggplot(rot,aes(x=PC1,y=PC2))
g+geom_point()

q1 = quantile(rot$PC1,seq(from=0.25,to=1,by=0.25))
binpc1 = sapply(rot$PC1,function(x){which(x<=q1&x>c(0,q1[1:(length(q1)-1)]))})
names(binpc1)=morph$id
q2 = quantile(rot$PC2,seq(from=0.25,to=1,by=0.25))
binpc2 = sapply(rot$PC2,function(x){which(x<=q2&x>=c(min(rot$PC2),q2[1:(length(q2)-1)]))})
names(binpc2)=morph$id

res$binpc1=binpc1[as.character(res$id)]
res$binpc2=binpc2[as.character(res$id)]

sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(
  gini=mean(mwgini),binpc1=mean(binpc1),binpc2=mean(binpc2)
)

g=ggplot(sres[sres$maxSugar==110,],aes(x=population,y=minSugar,fill=gini))
g+geom_raster()+facet_grid(binpc1~binpc2)



#####
# Meta phase diagram with EMD

source(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/Scripts/functions.R'))

sres <- as.tbl(read.csv(file='exploration/20170328_gridsynth_sum.csv'))

ref <- as.tbl(read.csv('exploration/2017_03_28_11_57_40_GRID_FIXED.csv'))
sref = ref %>% group_by(population,minSugar,maxSugar)%>%summarise(gini=mean(mwgini))

dists=distancesToRef(simresults=sres,reference=sref,parameters=c('population','minSugar','maxSugar'),idcol="id")
  
morph <- as.tbl(read.csv('exploration/20170328_gridsynth_morpho.csv'))
names(morph)[2]<-"id"

sres$binpc1=binpc1[as.character(sres$id)]
sres$binpc2=binpc2[as.character(sres$id)]
sres$dist=dists[as.character(sres$id)]

metaphase = sres %>% group_by(binpc1,binpc2)%>%summarise(emd=mean(dist))

metaparams = sres %>% group_by(id)%>%summarise(alpha=mean(spAlpha),diffSteps=mean(spDiffsteps),diffusion=mean(spDiffusion),growth=mean(spGrowth))

g=ggplot(metaphase,aes(x=binpc1,y=binpc2,fill=emd))
g+geom_raster()
ggsave(file='res/emd_raster.png',width=12,height=10,units = 'cm')



### plot typical phase diagrams

dists[dists<1.1*min(dists)]
dists[dists>0.9*max(dists)]

g=ggplot(sres[sres$id==27&sres$maxSugar==110,],aes(x=population,y=minSugar,fill=gini))
g+geom_raster()+stdtheme
ggsave(file='res/phasediagram_id27_maxSugar110.png',width=18,height=15,units = 'cm')
data.frame(sres[sres$id==27&sres$maxSugar==110,])

g=ggplot(sres[sres$id==0&sres$maxSugar==110,],aes(x=population,y=minSugar,fill=gini))
g+geom_raster()+stdtheme
ggsave(file='res/phasediagram_id0_maxSugar110.png',width=18,height=15,units = 'cm')
data.frame(sres[sres$id==0&sres$maxSugar==110,])





