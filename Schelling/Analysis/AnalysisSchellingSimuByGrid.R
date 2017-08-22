#############################
#
# Sensitivity analysis of 
# Schelling with urban grids
# Clémentine August 3rd, 2017
#
#############################


###################### Functions for JR grids

giveGridType =  function(Id){
  gridToClass$id = as.character(gridToClass$id)
  gridline = subset(gridToClass, id == Id)
  return(gridline)
}

aggregate_sims = function(gridID, fun=median){
  df = read.csv("SimulationRes/res_", gridID, sep=",", dec=".", header = T, stringsAsFactors=F)
  df$combi = paste(df$freeSpace, "_", df$similarWanted, sep="")
  ag_df = aggregate(df, by=list(df$combi), FUN = fun)
  return(ag_df)
}

phase_space = function(df, metric = "dissimilarity"){
  lim = c(0, 1)
  if (metric == "unsatisfied") lim = c(0,max(df$unsatisfied))
  df$segregationIndex = df[,metric]
  df$Tolerance_Rate = 1 - df$similarWanted
  df$Vacancy_Rate = df$freeSpace
  ggplot(df, aes(x = Vacancy_Rate , y = Tolerance_Rate)) + 
    geom_point(aes(fill = segregationIndex, color = segregationIndex)) + 
    scale_fill_gradient(low="floralwhite", high="red", limits=lim) + 
    scale_color_gradient(low="floralwhite", high="red", limits=lim) +
    ggtitle(metric) + guides(fill=FALSE, color=F)
}

SegregationSensitivity = function(cityType, segregationIndex, testType=F){
  cityType$Tolerance_Rate = 1 - cityType$similarWanted
  cityType$Vacancy_Rate = cityType$freeSpace
  cityType$Segregation_Rate = cityType[,segregationIndex]
  if (testType==F) model = lm(Segregation_Rate ~ Tolerance_Rate + Vacancy_Rate, data=cityType)
  if (testType==T) model = lm(Segregation_Rate ~ Tolerance_Rate + Vacancy_Rate + type, data=cityType)
  return(model)
}

modelResultsCol = function(model, name, testType=F){
  mod = summary(model)
  
  intercept = round(mod$coefficients[1,1],3)
  sign_intercept = round(mod$coefficients[1,4],3)
  Tolerance_Rate = round(mod$coefficients[2,1],3)
  sign_Tolerance_Rate = round(mod$coefficients[2,4],3)
  Vacancy_Rate = round(mod$coefficients[3,1],3)
  sign_Vacancy_Rate = round(mod$coefficients[3,4],3)
  if (testType==F)  {Discontinuous_City = NA
  sign_Discontinuous_City = NA
  Polycentric_City =NA
  sign_Polycentric_City = NA}
  if (testType==T) {Discontinuous_City = round(mod$coefficients[4,1],3)
  sign_Discontinuous_City = round(mod$coefficients[4,4],3)
  Polycentric_City = round(mod$coefficients[5,1],3)
  sign_Polycentric_City = round(mod$coefficients[5,4],3)}
  R2 = round(100 * mod$r.squared,2)
  n = round(mod$df[[1]] + mod$df[[2]],0)
  Line = data.frame(intercept, sign_intercept, Tolerance_Rate, sign_Tolerance_Rate, Vacancy_Rate, sign_Vacancy_Rate, 
                    Discontinuous_City, sign_Discontinuous_City, Polycentric_City, sign_Polycentric_City, R2, n)
  Col = t(Line)
  colnames(Col) = c(name)
  return(Col)
}
#############################
# generate grids from file
#############################

library(lattice)
setwd("~/Documents/spacemattersJUSTE")

gridsExample = c("6997988603", "-19438823361", "6829794361", "4849075441")
breakpoints = c(0, 0.25, 0.5, 1, 2, 5, 10, 20, 50, 100, 100000)
name = gridsExample[4]
gridToPlot = read.csv(paste0("Schelling/Grids/quantGrids/", name,"_config.csv"), dec=".", sep=",")
maxZ = mean(gridToPlot$z)
gridToPlot$z = gridToPlot$z / maxZ
head(gridToPlot)

my_palette <- colorRampPalette(c("white", "black"))(n = 299)
mapPop <- levelplot(z ~ x * y , gridToPlot, 
                    col.regions=my_palette, 
                    colorkey=T ,
                    xlab="", ylab="",
                    cex.axis=0.1, 
                    at=breakpoints)

png(filename=paste0("sugarscape/figures/grid_", name,"_config.png"), width = 495, height = 422)
mapPop
dev.off()






#############################
# analyse schelling results
#############################

sims = read.csv("Schelling/schelling_sims.csv", dec=".", sep=",")
dim(sims)
summary(sims)


gridIdToClassId = read.csv("Schelling/Analysis/15gridsPerClass.csv", header = T, sep=",", dec=".")
gridIdToClassId$id_class_f = as.factor(ifelse(gridIdToClassId$class == 1, "discontinuous",
                                              ifelse(gridIdToClassId$class == 2, "polycentric",
                                                     "compact")))
gridToClass = gridIdToClassId[,c("id", "id_class_f")]




###################### Plot Phase Space by grid


tab = aggregate_sims(gridID = gridId)
phase_space(df = tab, metric = "moran")


###################### Select all cities of same type

gridToClass$id = as.character(gridToClass$id)

polycentrics = subset(gridToClass, id_class_f == "polycentric"  & substr(id, 1,2) != "gr")
compacts = subset(gridToClass, id_class_f == "compact"  & substr(id, 1,2) != "gr")
discontinuouss = subset(gridToClass, id_class_f == "discontinuous"  & substr(id, 1,2) != "gr")

dim(polycentrics)
dim(compacts)
dim(discontinuouss)

###################### save density and phaseSpace plots for each city grid

segregation = c("unsatisfied", "dissimilarity", "moran", "entropy", "exposureRG", "isolation", "delta")

poly = list(polycentrics[,1])[[1]]
for (i in poly){
  gridId = i
  g = readGrid(gridId)
  t = as.character(giveGridType(gridId)[1,2])
  
  png(paste(path, "densityPlots/poly/density_", i, ".png", sep=""))
  print(plotGrid(longTable=g, varToPlot="z", title=t))
  dev.off() 
  tab = aggregate_sims(gridID = gridId)
  
  for (j in segregation){
    tosave = paste(path, "metricsPlots/poly/", j, "_", i, ".png", sep="")
    phase_space(df = tab, metric = j)
    ggsave(filename=tosave)
  }
}

comp = list(compacts[,1])[[1]]
for (i in comp){
  gridId = i
  g = readGrid(gridId)
  t = as.character(giveGridType(gridId)[1,2])
  
  png(paste(path, "densityPlots/compact/density_", i, ".png", sep=""))
  print(plotGrid(longTable=g, varToPlot="z", title=t))
  dev.off() 
  tab = aggregate_sims(gridID = gridId)
  
  for (j in segregation){
    tosave = paste(path, "metricsPlots/compact/", j, "_", i, ".png", sep="")
    phase_space(df = tab, metric = j)
    ggsave(filename=tosave)
  }
}

disc = list(discontinuouss[,1])[[1]]
for (i in disc){
  gridId = i
  g = readGrid(gridId)
  t = as.character(giveGridType(gridId)[1,2])
  
  png(paste(path, "densityPlots/disc/density_", i, ".png", sep=""))
  print(plotGrid(longTable=g, varToPlot="z", title=t))
  dev.off() 
  tab = aggregate_sims(gridID = gridId)
  
  for (j in segregation){
    tosave = paste(path, "metricsPlots/disc/", j, "_", i, ".png", sep="")
    phase_space(df = tab, metric = j)
    ggsave(filename=tosave)
  }
}


###################### Create one large table by city type (# only needed once)

# poly = list(polycentrics[,1])[[1]]
# polyTab = data.frame()
# for (i in poly){
#   gridId = i
#   df = read.csv(paste(path, "SimulationRes/res_", gridId, sep=""), sep=",", dec=".", header = T, stringsAsFactors=F)
#   polyTab = rbind(polyTab, df)
# }
# 
# dim(polyTab)
# polyTab$type = "polycentric"
# write.csv(polyTab, paste(path, "15_polycentric_results.csv", sep=""))
# 
polyTab = read.csv(paste(path, "15_polycentric_results.csv", sep=""), sep=",", dec=".")

# comp = list(compacts[,1])[[1]]
# compTab = data.frame()
# for (i in comp){
#   gridId = i
#   df = read.csv(paste(path, "SimulationRes/res_", gridId, sep=""), sep=",", dec=".", header = T, stringsAsFactors=F)
#   compTab = rbind(compTab, df)
# }
# dim(compTab)
# compTab$type = "compact"
# write.csv(compTab, paste(path, "15_compact_results.csv", sep=""))
compTab = read.csv(paste(path, "15_compact_results.csv", sep=""), sep=",", dec=".")

# 
# disc = list(discontinuouss[,1])[[1]]
# discTab = data.frame()
# for (i in disc){
#   gridId = i
#   df = read.csv(paste(path, "SimulationRes/res_", gridId, sep=""), sep=",", dec=".", header = T, stringsAsFactors=F)
#   discTab = rbind(discTab, df)
# }
# dim(discTab)
# discTab$type = "discontinuous"
# write.csv(discTab, paste(path, "15_discontinuous_results.csv", sep=""))
discTab = read.csv(paste(path, "15_discontinuous_results.csv", sep=""), sep=",", dec=".")
# 
# 
# allCityTab = rbind(polyTab, compTab, discTab)
# write.csv(allCityTab, paste(path, "45_cities_results.csv", sep=""))
allCityTab = read.csv(paste(path, "45_cities_results.csv", sep=""), sep=",", dec=".")


###################### Regression models on simulations by parameter value & city type
segregation = c("unsatisfied", "dissimilarity", "moran", "entropy", "exposureRG", "isolation", "delta")

for (segindex in segregation){
  ### Results for Polycentric cities
  m = SegregationSensitivity(cityType=polyTab, segregationIndex=segindex)
  polyCol = modelResultsCol(model = m, name = "polycentric")
  
  ### Results for compact cities
  m = SegregationSensitivity(cityType=compTab, segregationIndex=segindex)
  compCol = modelResultsCol(model = m, name = "compact")
  
  ### Results for discontinuous cities
  m = SegregationSensitivity(cityType=discTab, segregationIndex=segindex)
  discCol = modelResultsCol(model = m, name = "discontinuous")
  
  ### Results for all cities
  m1 = SegregationSensitivity(cityType=allCityTab, segregationIndex=segindex)
  m2 = SegregationSensitivity(cityType=allCityTab, segregationIndex=segindex, testType=T)
  allCol = modelResultsCol(model = m1, name = "AllCities")
  allCol2 = modelResultsCol(model = m2, name = "AllCities", testType=T)
  
  summaryTab = data.frame(polyCol, compCol, discCol, allCol, allCol2)
  write.csv(summaryTab, paste(path, "Regressions/",segindex,".csv", sep=""))
}

