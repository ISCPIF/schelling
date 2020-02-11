# schelling phase diagram distance

library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/Schelling/schelling-netlogo'))
stdtheme= theme(axis.title = element_text(size = 22), 
                axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),
                strip.text = element_text(size=15),
                legend.text=element_text(size=15), legend.title=element_text(size=15))

# functions
source(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/Scripts/functions.R'))


res <- as.tbl(read.csv('exploration/SOBOL_ALLGRIDS.csv'))


## convergence
sres = res %>% group_by(greenRatio,redRatio,similarWanted,gridId) %>% summarise(
  dissimilaritySharpe=abs(mean(dissimilarity)/sd(dissimilarity)),
  entropySharpe=abs(mean(entropy)/sd(entropy)),
  moranSharpe=abs(ifelse(mean(greenRatio)>mean(redRatio),mean(moranGreen)/sd(moranGreen),mean(moranRed)/sd(moranRed)))
)
summary(sres$dissimilaritySharpe)
summary(sres$entropySharpe)
summary(sres$moranSharpe)



sres = res %>% group_by(greenRatio,redRatio,similarWanted,gridId) %>% summarise(
  #dissimilarity=median(dissimilarity),entropy=median(entropy),
  #moran=ifelse(mean(greenRatio)>mean(redRatio),median(moranGreen),median(moranRed)),
  dissimilarity=mean(dissimilarity),entropy=mean(entropy),
  moran=ifelse(mean(greenRatio)>mean(redRatio),mean(moranGreen),mean(moranRed)),
  vacancyRate = 1 - mean(greenRatio) - mean(redRatio),
  minorityIndex = abs(mean(greenRatio)-mean(redRatio))
)


# examples of phase diag
#sres$greenRatioF = cut(sres$greenRatio,10)
#g=ggplot(sres,aes(x=greenRatio+redRatio,y=similarWanted,color=moran))
#g+geom_point()+facet_wrap(~greenRatioF)
# -> cannot provide similar grids because of sampling
#g=ggplot(sres,aes(x=greenRatio+redRatio,y=similarWanted,color=moran))
#g+geom_point()

#g=ggplot(res[res$gridId%in%0:5,],aes(x=greenRatio+redRatio,y=similarWanted,color=entropy))
#g+geom_point()+facet_wrap(~gridId)


# distances

#for(alpha in c(1,2,3,10,1000)){
#  show(paste0('alpha = ',alpha))
alpha=2
dists=distancesToRef(simresults=sres,
                     reference=sres[sres$gridId==0,],
                     parameters=c('similarWanted','vacancyRate','minorityIndex'),
                     indicators=c("dissimilarity","moran","entropy"),
                     idcol="gridId",
                     distfun=function(x,y){(sum(abs(x-y)^alpha)/length(x))^(1/alpha)}
                     #distfun='emd'
                     )
#show(summary(dists))
#}

# emd dists -> not very interesting as cannot be directly compared to other distances
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.01093 0.02619 0.03678 0.05357 0.10939 

# get grid morphologies
grids <- as.tbl(read.csv('Grids/15gridsPerClass.csv',stringsAsFactors = F))

gridData = data.frame()
for(i in 1:nrow(grids)){
  currentid = strsplit(grids[[1]][i],'_',fixed = F)[[1]][1];currentdata=read.csv(paste0('Grids/quantGrids/',currentid,'_params.csv'))
  gridData=rbind(gridData,cbind(currentdata,id=i-1,class=grids[[2]][i]))
}

pca = morphoPCA(gridData[,c("id","distance","entropy","moran","slope")])

g = ggplot(data.frame(pca$morph,distance=dists),aes(x=PC1,y=PC2,color=distance))
g+geom_point(size=2.5)+stdtheme+scale_colour_gradient2(low='darkgreen',mid='grey',high='red',midpoint = 1.0)
ggsave(file='res/schelling-relativedistance_morphspace_mean.png',width=18,height=15,units = 'cm')

g=ggplot(data.frame(gridData,distance=dists),aes(x=alphalocalization,y=diffusion,color=distance))
g+geom_point(size=2.5)+stdtheme+xlab(expression(alpha))+ylab(expression(beta))+scale_colour_gradient2(low='darkgreen',mid='grey',high='red',midpoint = 1.0)
ggsave(file='res/schelling-relativedistance_metaparams_mean.png',width=18,height=15,units = 'cm')









