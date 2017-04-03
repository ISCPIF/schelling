
library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/spacematters/sugarscape'))

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





