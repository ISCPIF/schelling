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

sres = res %>% group_by(greenRatio,redRatio,similarWanted,gridId) %>% summarise(
  dissimilarity=median(dissimilarity),entropy=median(entropy),
  moran=ifelse(mean(greenRatio)>mean(redRatio),median(moranGreen),median(moranRed)),
  vacancyRate = 1 - mean(greenRatio) - mean(redRatio),
  minorityIndex = abs(mean(greenRatio)-mean(redRatio))
)

dists=distancesToRef(simresults=sres,reference=sres[sres$gridId==0,],parameters=c('similarWanted','vacancyRate','minorityIndex'),indicators=c("dissimilarity","moran","entropy"),idcol="gridId")

# get grid morphologies
grids <- as.tbl(read.csv('Grids/15gridsPerClass.csv',stringsAsFactors = F))

gridData = data.frame()
for(i in 1:nrow(grids)){
  currentid = strsplit(grids[[1]][i],'_',fixed = F)[[1]][1];currentdata=read.csv(paste0('Grids/quantGrids/',currentid,'_params.csv'))
  gridData=rbind(gridData,cbind(currentdata,id=i-1,class=grids[[2]][i]))
}

pca = morphoPCA(gridData[,c("id","distance","entropy","moran","slope")])

g = ggplot(data.frame(pca$morph,distance=dists),aes(x=PC1,y=PC2,color=distance))
g+geom_point(size=2.5)+stdtheme+scale_colour_gradient2(low='red',mid='grey',high='darkgreen',midpoint = 0.5)
ggsave(file='res/schelling-relativedistance_morphspace.pdf',width=18,height=15,units = 'cm')

g=ggplot(data.frame(gridData,distance=dists),aes(x=alphalocalization,y=diffusion,color=distance))
g+geom_point(size=2.5)+stdtheme+scale_colour_gradient2(low='red',mid='grey',high='darkgreen',midpoint = 0.5)+xlab(expression(alpha))+ylab(expression(beta))
ggsave(file='res/schelling-relativedistance_metaparams.pdf',width=18,height=15,units = 'cm')









