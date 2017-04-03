library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/spacematters/sugarscape'))

res <- as.tbl(read.csv('exploration/2017_03_28_11_59_36_GRID_SYNTHPATTERN.csv'))

morph = res %>% group_by(id)%>%summarise(distance=mean(distance),entropy=mean(entropy),moran=mean(moran),slope=mean(slope))
for(j in 2:ncol(morph)){morph[,j]<-(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca=prcomp(morph[,2:ncol(morph)])
rot = data.frame(as.matrix(morph[,2:ncol(morph)])%*%pca$rotation)

write.csv(data.frame(morph$id,rot),file='exploration/20170328_gridsynth_morpho.csv')

sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(
  gini=mean(mwgini)
)

write.csv(sres,file='exploration/20170328_gridsynth_sum.csv')

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

for(maxSugar in unique(sres$maxSugar)){
  g=ggplot(sres[sres$maxSugar==maxSugar,],aes(x=population,y=minSugar,fill=gini))
  g+geom_raster()+facet_grid(binpc1~binpc2)
  ggsave(file=paste0('res/phasediagrams_pca_maxSugar',maxSugar,'.png'),width = 30,height=28,units = 'cm',dpi = 200)
}

