#############################
#
# Sensitivity analysis of 
# Schelling with urban grids
# Clémentine August 23, 2015
#
#############################

#path = "~/Documents/spacematters/simu/"
path = "~/Documents/clementine/other/spacematters/"

pathGrids = "~/Dropbox/Biblio Spatial Models/Results_analysis/"

library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(lattice)

###################### Functions

plotGrid = function(longTable, varToPlot, title = "", colorGradient = "default"){
  if (colorGradient == "default") colorGradient <- colorRampPalette(c("white", "dodgerblue3"))(n = 299)
  extract <- melt(longTable,
                  id.vars=c("X","Y"),
                  measure.vars=varToPlot)
  tmp <- dcast(extract, X~Y)  
  mat <- as.matrix(tmp[,-1])
  size <- dim(mat)[[1]]
  rownames(mat) <- tmp[,1]
  p = levelplot(mat, col.regions=colorGradient, colorkey=T,
                xlab="", ylab="", cex.axis=0.1,
                main = title,
                scales=list(x=list(at=c(0,size+1)), y=list(at=c(0,size+1)))
  )
  return(p)
}

readGrid = function(Id){
  if ( substr(Id, 1, 8) == "gridAnas") gridTable = read.csv(paste(pathGrids, "grids/", Id, sep=""), sep=",", dec=".") 
  if ( substr(Id, 1, 8) != "gridAnas") gridTable = read.csv(paste(pathGrids, "grids/", Id, sep=""), sep=",", dec=".") 
  return(gridTable)
}

giveGridType =  function(Id){
  gridToClass$id_grid = as.character(gridToClass$id_grid)
  gridline = subset(gridToClass, id_grid == Id)
  return(gridline)
}

aggregate_sims = function(gridID, fun=median){
  df = read.csv(paste(path, "res_", gridID, sep=""), sep=",", dec=".", header = T, stringsAsFactors=F)
  
  df$combi = paste(df$freeSpace, "_", df$similarWanted, sep="")
  
  ag_df = aggregate(df, by=list(df$combi), FUN = fun)
  return(ag_df)
  }

phase_space = function(df, metric = "dissimilarity"){
  df$segregationIndex = ag_df[,metric]
  ggplot(df, aes(x = similarWanted, y = freeSpace)) + 
    geom_point(aes(fill = segregationIndex, color = segregationIndex)) + 
    scale_fill_gradient(low="red", high="blue") + 
    scale_color_gradient(low="red", high="blue") +
    ggtitle(metric)
}

######################  load the grid to city type file

gridIdToClassId = read.csv(paste(pathGrids, "Grid_classes_JR_FL.csv", sep=""), 
                           header = T, sep=",", dec=".")
gridIdToClassId$id_class_f = as.factor(ifelse(gridIdToClassId$id_class == 1, "discontinuous",
                                              ifelse(gridIdToClassId$id_class == 2, "polycentric",
                                                     "compact")))
gridToClass = gridIdToClassId[,c("id_grid", "id_class_f")]


###################### Cut the initial simu files by gridID

# res = read.csv(paste(path, "results.csv", sep=""), sep=",", dec=".", header = F)
# tail(res)
# 
# colnames(res) = c("gridName", "seed", "step", "freeSpace", "similarWanted", 
#                   "population", "unsatisfied", "dissimilarity", "moran", 
#                   "entropy", "exposureRG", "isolation", "delta")
# 
# write.csv(res, paste(path, "results_with_colnames.csv", sep=""))

res = read.csv(paste(path, "results_with_colnames.csv", sep=""), sep=",", dec=".", header = T)

# res = res[-1,]
# head(res)
# colnames(res) = c("ID","gridName", "seed", "step", "freeSpace", "similarWanted", 
#                   "population", "unsatisfied", "dissimilarity", "moran", 
#                   "entropy", "exposureRG", "isolation", "delta")

grids = list(unique(res$gridName))[[1]]
str(grids)

for (i in grids){
  results = subset(res, gridName == i)
  assign(paste("res_", i, sep=""), results)
  write.csv(results, paste(path, "res_", i, sep=""))
}




###################### Phase Space by grid

# Grid Name
Id = "gridAnas_61"
gridId = paste(Id, ".csv", sep="")

# Grid Type and viz
g = readGrid(Id)
t = as.character(giveGridType(Id)[1,2])
plotGrid(longTable=g, varToPlot="Z", title=t)

# Viz phase space
tab = aggregate_sims(gridID = gridId)
phase_space(df = tab, metric = "entropy")

