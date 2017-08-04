
library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/sugarscape'))
stdtheme= theme(axis.title = element_text(size = 22), 
                axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),
                strip.text = element_text(size=15),
                legend.text=element_text(size=15), legend.title=element_text(size=15))


source(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/Scripts/functions.R'))


# simulation results, already aggregated by repetitions
sres <- as.tbl(read.csv(file='exploration/20170328_gridsynth_sum.csv'))

# reference simulations
ref <- as.tbl(read.csv('exploration/2017_03_28_11_57_40_GRID_FIXED.csv'))
# aggregation of reference on repetitions
sref = ref %>% group_by(population,minSugar,maxSugar)%>%summarise(gini=mean(mwgini))

# compute the distance to reference
dists=distancesToRef(simresults=sres,reference=sref,parameters=c('population','minSugar','maxSugar'),indicators=c("gini"),idcol="id")



##
# position of file setup within morpho space
#moran = 0.2563323814971014"
#observer: "entropy = 0.9640148151339423"
#observer: "slope = -0.3742297460724673"
#observer: "slope-r2 = 0.6328386256319237"
#observer: "distance = 0.8290054369272968"
rot<-read.csv('20170328_gridsynth_rotation.csv',row.names = 1 )
matrix(data=c(0.8290054369272968,0.9640148151339423,0.2563323814971014,-0.3742297460724673),nrow=1)%*%as.matrix(rot)

# get morphological measures
morph <- as.tbl(read.csv('exploration/20170328_gridsynth_morpho.csv'))
names(morph)[2]<-"id"


# plots

g = ggplot(data.frame(morph,distance=dists),aes(x=PC1,y=PC2,color=distance))
g+geom_point(size=2.5)+geom_point(x=1.130869,y=0.09586869,col='black',size=3)+
  geom_point(x=morph$PC1[morph$id==27],y=morph$PC2[morph$id==27],pch=0,size=4,col=3)+
  geom_point(x=morph$PC1[morph$id==0],y=morph$PC2[morph$id==0],pch=0,size=4,col=4)+stdtheme+
  scale_colour_gradient2(low='red',mid='grey',high='darkgreen',midpoint = 1)
ggsave(file='res/relativedistance_morphspace.pdf',width=18,height=15,units = 'cm')

g=ggplot(data.frame(metaparams,distance=dists),aes(x=alpha,y=diffusion,color=distance))
g+geom_point(size=2.5)+stdtheme+scale_colour_gradient2(low='red',mid='grey',high='darkgreen',midpoint = 1)+
  xlab(expression(alpha))+ylab(expression(beta))
ggsave(file='res/relativedistance_metaparams.pdf',width=18,height=15,units = 'cm')


g=ggplot(data.frame(metaparams,dist=dists),aes(x=growth,y=diffusion,color=dist))
g+geom_point(size=2)

g=ggplot(data.frame(metaparams,dist=dists),aes(x=diffSteps,y=diffusion,color=dist))
g+geom_point(size=2)

