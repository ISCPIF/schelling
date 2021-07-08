
# schelling phase diagram distance

library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/Schelling/'))
stdtheme= theme(axis.title = element_text(size = 22), 
                axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),
                strip.text = element_text(size=15),
                legend.text=element_text(size=15), legend.title=element_text(size=15))

# functions
source(paste0(Sys.getenv('CS_HOME'),'/SpaceMatters/Models/spacematters/Scripts/functions.R'))

# aggregate results for each grid
#sres = data.frame()
#for(file in list.files('simu')[2:length(list.files('simu'))]){
#  show(file)
#  s = strsplit(file,split = "_");id = as.numeric(strsplit(s[[1]][length(s[[1]])],split='.',fixed = T)[[1]][1])
#  currentres <- as.tbl(read.csv(file = paste0('simu/',file)))
#  currentres$similarWanted = cut(currentres$similarWanted,10);currentres$freeSpace = cut(currentres$freeSpace,10)
#  currentsres = currentres %>% group_by(similarWanted,freeSpace)%>%summarise(dissimilarity=median(dissimilarity),moran=median(moran),entropy=median(entropy))
#  sres = rbind(sres,data.frame(currentsres,id=rep(id,nrow(currentsres))))
#}
#save(sres,file='simu/test/aggreg_anas.RData')
load('simu/test/aggreg_anas.RData')

# test visu one phase diagram
#g=ggplot(currentsres,aes(x=similarWantedDiscr,y=freeSpaceDiscr,fill=dissimilarity))
#g+geom_raster()


# get distances to one reference (take arbitrary first grid)
dists=distancesToRef(simresults=sres,reference=sres[sres$id==1,],parameters=c('similarWanted','freeSpace'),indicators=c("dissimilarity","moran","entropy"),idcol="id")


# get grid morphologies
library(readODS)
rawmorph = readODS::read_ods(path='Grids/quantGrids/reportAnas.ods',sheet = 2,col_names = T)
morph = data.frame(id=rawmorph$gridAnas,rawmorph[,c("distance","entropy","moran","slope")])
pca = morphoPCA(morph)
morph = pca$morph;rownames(morph)=morph$id

# test metaphasediag plot
g = ggplot(data.frame(morph[names(dists[dists<10]),],pddist=dists[dists<10]),aes(x=PC1,y=PC2,color=pddist))
g+geom_point(size=2.5)+stdtheme+scale_colour_gradient2(low='red',mid='grey',high='darkgreen',midpoint = 1)
ggsave(file='simu/test/relativedistance_morphspace.pdf',width=18,height=15,units = 'cm')



