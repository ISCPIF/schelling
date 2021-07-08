library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/sugarscape'))

source('../Scripts/functions.R')

#res <- as.tbl(read.csv('exploration/2017_03_28_11_59_36_GRID_SYNTHPATTERN.csv'))
# a few runs failed -> filter null rows using cleanData.sh in exploration
res <- as.tbl(read.csv('exploration/2017_03_28_gridsynthpattern_nonull.csv'))

# morphological indicators
morph = res %>% group_by(id)%>%summarise(distance=mean(distance),entropy=mean(entropy),moran=mean(moran),slope=mean(slope))


for(j in 2:ncol(morph)){morph[,j]<-(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
pca=prcomp(morph[,2:ncol(morph)])
rot = data.frame(as.matrix(morph[,2:ncol(morph)])%*%pca$rotation,morph)

save(pca,file='pca.RData')

write.csv(pca$rotation,file='exploration/20170328_gridsynth_rotation.csv',row.names = T,col.names = T)

write.csv(data.frame(morph$id,rot),file='exploration/20170328_gridsynth_morphopca.csv')


## convergence statistics
sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(giniSharpe = mean(gini)/sd(gini))
summary(sres$giniSharpe)

## types of distribs

# ks
sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(distrib=getDistribType(gini)$name)
table(sres$distrib)
#gamma  geom lnorm  norm  unif 
#1090  9794 16212 14263 13201 
# aic
sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(distrib=getDistribType(gini,criteria = 'aic')$name)
table(sres$distrib)
# exp gamma lnorm  norm 
# 1451  9707 22825 20577 

# sort by morphology ? does not have classes here, 
# looks at correlates with morpho indicators !
#sresmorph = left_join(sres,rot)
#sresmorph$distrib=as.numeric(as.factor(sresmorph$distrib))
#cor(sresmorph[,c("distrib","PC1","PC2")]) #-> should convert distrib to numeric, anyway not rigorous
#kruskal.test(distrib~distance,sresmorph) 
#kruskal.test(distrib~moran,sresmorph)
#kruskal.test(distrib~id,sresmorph)

## aggregation
sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(
  gini=mean(mwgini),spAlpha=mean(spAlpha),spDiffsteps=mean(spDiffsteps),spDiffusion=mean(spDiffusion),spGrowth=mean(spGrowth),spPopulation=mean(spPopulation)
)

write.csv(sres,file='exploration/20170328_gridsynth_sum.csv')


# q1 = quantile(rot$PC1,seq(from=0.25,to=1,by=0.25))
# binpc1 = sapply(rot$PC1,function(x){which(x<=q1&x>c(0,q1[1:(length(q1)-1)]))})
# names(binpc1)=morph$id
# q2 = quantile(rot$PC2,seq(from=0.25,to=1,by=0.25))
# binpc2 = sapply(rot$PC2,function(x){which(x<=q2&x>=c(min(rot$PC2),q2[1:(length(q2)-1)]))})
# names(binpc2)=morph$id
# 
# res$binpc1=binpc1[as.character(res$id)]
# res$binpc2=binpc2[as.character(res$id)]
# 
# sres = res %>% group_by(id,population,minSugar,maxSugar)%>%summarise(
#   gini=mean(mwgini),binpc1=mean(binpc1),binpc2=mean(binpc2)
# )
#
#for(maxSugar in unique(sres$maxSugar)){
#  g=ggplot(sres[sres$maxSugar==maxSugar,],aes(x=population,y=minSugar,fill=gini))
#  g+geom_raster()+facet_grid(binpc1~binpc2)
#  ggsave(file=paste0('res/phasediagrams_pca_maxSugar',maxSugar,'.png'),width = 30,height=28,units = 'cm',dpi = 200)
#}

