#Step 1 : import macros
#May 2015, Florent Le Néchet


#July 2015 : classification
# NB : computation time can be gained for distance index.

# ---------------------------------------------------------------------------------
# ----------------            CREATE TABLE                   ----------------------
# ---------------------------------------------------------------------------------

createTable <- function(parametersValues,centreValues) {
  # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
  # This function reports a data.frame with three columns
  # X is the X-coordinate of the cell
  # Y is the Y-coordinate of the cell
  # Z is the Population within the cell
  # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
  # parameters for function createTable are : 
  # - a data.frame parametersValues containing 6 variables and 1 line: 
  # Xmin : minimum X coordinate for the table
  # Xmax : maximum X coordinate for the table
  # Ymin : minimum Y coordinate for the table
  # Ymax : maximum Y coordinate for the table
  # nu   : extent of the cell. Number of cells is (Xmax - Xmin) / nu   *  (Ymax - Ymin) / nu 
  # eta  : parameter used for the computation of the grid : the smaller it is the closest it is
  # from a real "Clark" distribution. During the double integration of the exponential, each grid-cell
  # of size nu is subdivided into mini-cells of size eta.
  # therefore eta should be small than nu, around a tenth of its value.
  # - a data.frame centreValues containing 4 variables and as many lines as centers: 
  # X0 : X coordinate of the center
  # Y0 : Y coordinate of the center
  # A  : A parameter (Clark distribution : density = A exp(-b r))
  # b  : b parameter (Clark distribution : density = A exp(-b r)))
  # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
  
  Xmin = parametersValues$Xmin[1]
  Ymin = parametersValues$Ymin[1]
  Xmax = parametersValues$Xmax[1]
  Ymax = parametersValues$Ymax[1]
  nombreCentres = nrow(centresValues)
  XXX = NULL
  YYY = NULL
  PPP = NULL
  i = 1
  xt = Xmin
  while (xt < Xmax) {
    yt = Ymin
    while (yt < Ymax) {
      XXX[i] = xt 
      YYY[i] = yt
      j = 1
      PPP[i] = 0
      while (j <= nombreCentres) {
        #print(j)
        PPP[i] = PPP[i] + reportPopulationCarre(xt,xt + parametersValues$nu[1],
                                                yt,yt + parametersValues$nu[1],
                                                centresValues$X0[j],
                                                centresValues$Y0[j],
                                                centresValues$A[j],
                                                centresValues$b[j],
                                                parametersValues$eta[1])
        j = j + 1
      }
      print(paste(xt,yt, sep = ";"))
      i = i + 1  
      yt = yt + parametersValues$nu[1]
    }
    xt = xt +  parametersValues$nu[1]
  }
  dataFin = data.frame(cbind(XXX,YYY,PPP))
  names(dataFin) = c("X","Y","Z")
  dataFin[dataFin$Z<0,] = 0
  dataFin
}

# ---------------------------------------------------------------------------------
# ----------------         REPORT POPULATION CARREE         ----------------------
# ---------------------------------------------------------------------------------


reportPopulationCarre <- function(x1,x2,y1,y2,X0,Y0,A,b,eta) {
  #compute the population of a Clark distribution of parameters A, b and center X0, Y0
  PopTemp = 0
  xx = x1
  while (xx + eta <= x2) {
    yy = y1
    while(yy + eta <= y2) {
      Rtemp = sqrt((X0 - xx - (1/2) * eta)^2+(Y0 - yy - (1/2) * eta)^2)
      PopTemp = PopTemp + eta ^ 2 * A * exp(- b * Rtemp)
      yy = yy + eta
      #         print(yy)
      #        flush.console()  # force the output
    }
    xx = xx + eta
  }
  PopTemp  
}


# ---------------------------------------------------------------------------------
# ----------------           DRAW IN TWO  DIMENSIONS         ----------------------
# ---------------------------------------------------------------------------------

afficher2d <- function(dataFin) {
  colorPalette <- terrain.colors(nrow(dataFin), alpha = 1)
  dataFin <- dataFin[order(dataFin$Z,decreasing = TRUE),]
  plot(dataFin$X,dataFin$Y,col = colorPalette, pch = 19, cex = 1)
}


# ---------------------------------------------------------------------------------
# ----------------           DRAW IN THREE   DIMENSIONS      ----------------------
# ---------------------------------------------------------------------------------


library(scatterplot3d)
afficher3d <- function(dataFin) {
  scatterplot3d(dataFin$X,dataFin$Y,dataFin$Z, main="3D Scatterplot", type = "h",highlight.3d=TRUE, pch = 19, xlab = "", ylab = "", zlab = "density", cex.symbols=0.1)}

# ---------------------------------------------------------------------------------
# ----------------      COMPUTE URBAN FORM INDICATORS        ----------------------
# ---------------------------------------------------------------------------------



reportPopulation <- function (table, cutoffdensity) {
  
  table <- subset(table, Z > cutoffdensity)
  sum(table$Z)  
}

reportDensity <- function (table, cutoffdensity) {
  cellExtent <- 0.25 #pas générique
  table <- subset(table, Z > cutoffdensity)
  pop = sum(table$Z)
  Ncellules = nrow(table)
  area = Ncellules * cellExtent^2
  density = pop / area
  density
}


reportDistance <- function (table, cutoffdensity) {

  #### table is made of three columns : X, Y, Z containing coordinates and 
  # Population in the corresponding cell
  #### unit is the unit of distance of the coordinates
  #### cutoffdensity is the cut off density used for the computation of all indicators
  table = subset(table, Z > cutoffdensity)
  table = table[order(-table$Z),]
  xx = table$X 
  yy = table$Y 
  zz = table$Z
  toadd = 0
  aa = 0
  bb = 0
  distance = 0
 # listDistance = NULL
#  listPop = NULL
  n = nrow(data.frame(zz))
 # print(n)
  #flush.console()
  i = 1
  compteur = 0
  while (i <= n) {
    j = 1
    toadd = 0
    while (j < i) {
      aa = xx[i]
      bb = yy[i]
      toadd = toadd + sqrt((aa - xx[j])^2+(bb - yy[j])^2) * zz[j]
      j = j + 1
    }
    distance = distance + toadd * zz[i]
 #   listDistance = c(listDistance, distance)
#    listPop = c(listPop, sum(zz[1:i]))
 #   compteur = compteur + 1
  #  if ((compteur / 1000) == floor(compteur / 1000)) {
   #   print(compteur * (compteur - 1) / (n * (n - 1)) * 100)
    #  flush.console() } # force the output
    i = i + 1
  }
  
  resultat = 2 * distance / sum(zz)^2
  #cbind(listDistance, listPop)
  resultat
}




reportEntropy <- function (table, cutoffdensity) {
  table = subset(table, Z > cutoffdensity)
  xx = table$X
  yy = table$Y
  zz = table$Z
  zT = sum(zz)
  toadd = 0
  n = nrow(data.frame(zz))
  for (i in 1:n) {
    toadd = toadd + zz[i] / zT * log(zz[i] / zT)
  }
  -1 * toadd / log(n)
}

reportRankSize <- function (table, cutoffdensity) {
  table = subset(table, Z > cutoffdensity)
  xx = table$X
  yy = table$Y
  zz = table$Z
  n = nrow(data.frame(zz[zz>0]))
  rang = log(1:n)
  taille = log(zz[zz>0][order(-zz[zz>0])])
  
  
  
  lmRT <- lm(taille~rang)
  RT = -summary(lmRT)$coeff[2]
  K = exp(summary(lmRT)$coeff[1])
  Rsquared = summary(lmRT)$r.squared
  
  resultat = cbind(RT,K,Rsquared)
  
  resultat
  
}





reportMoran <- function (table, cutoffdensity) {
  table = subset(table, Z > cutoffdensity)
  xx = table$X
  yy = table$Y
  zz = table$Z
  zT = as.numeric(sum(zz))
  n = nrow(data.frame(zz))
  toadd1 = 0
  toadd2 = 0
  j = 2
  #i = 1
  for (i in 1:n) {
    j = i + 1
    while (j <= n) {
      dij = sqrt((xx[i]-xx[j])^2+(yy[i]-yy[j])^2)
      toadd1 = toadd1 + (zz[i] - zT / n) * (zz[j] - zT / n) / dij
      toadd2 = toadd2 + 1 / dij
      j = j + 1
    }
  }
  toadd3 = 0
  for (i in 1:n) {
    toadd3 = toadd3 + (zz[i] - zT / n)^2
  }
  n * toadd1 / (2 * toadd2 * toadd3)
}




reportClasse <- function (gridCurrent, referenceGrids) {
  gridThreshold <- gridCurrent$Z[gridCurrent$Z > 2560]
  gridListe <- c(sum(gridCurrent$Z[gridCurrent$Z > 2560]) / length(gridThreshold) * 256 / 100 #densité nette 10 en hab / ha #astuce de calcul qui ne marche qu'avec les grilles de JR
                 ,reportDistance(gridCurrent,0)
                 ,reportEntropy(gridCurrent,0)
                 ,reportMoran(gridCurrent,0)
                 ,reportRankSize(gridCurrent,0)[1])
  
  nn <- ncol(referenceGrids) - 2
  print(nn)
  vectorDistances <- NULL
  vectorDistancesBis <- NULL
  
  i <- 1
  disti <- 0
  while (i <= nn) {
    disti <- sqrt(
      ((gridListe[1]-referenceGrids[1,i]) / (referenceGrids[1,5] - referenceGrids[1,4]))^2 + 
        ((gridListe[2]-referenceGrids[2,i]) / (referenceGrids[2,5] - referenceGrids[2,4]))^2 + 
        ((gridListe[3]-referenceGrids[3,i]) / (referenceGrids[3,5] - referenceGrids[3,4]))^2 + 
        ((gridListe[4]-referenceGrids[4,i]) / (referenceGrids[4,5] - referenceGrids[4,4]))^2 +
        ((gridListe[5]-referenceGrids[5,i]) / (referenceGrids[5,5] - referenceGrids[5,4]))^2
    )
    distiBis <- sqrt( 
      ((gridListe[2]-referenceGrids[2,i]) / (referenceGrids[2,5] - referenceGrids[2,4]))^2 + 
        ((gridListe[3]-referenceGrids[3,i]) / (referenceGrids[3,5] - referenceGrids[3,4]))^2 + 
        ((gridListe[4]-referenceGrids[4,i]) / (referenceGrids[4,5] - referenceGrids[4,4]))^2 +
        ((gridListe[5]-referenceGrids[5,i]) / (referenceGrids[5,5] - referenceGrids[5,4]))^2
    )
    
    vectorDistances <- cbind(vectorDistances, disti)
    vectorDistancesBis <- cbind(vectorDistancesBis, distiBis)
    i <- i + 1
  }
  
  #vectorDistances <- (c(dist1,dist2,dist3))
  
  distj <- min (vectorDistances)
  j <- which.min(vectorDistances)
  
  distjBis <- min (vectorDistancesBis)
  jBis <- which.min(vectorDistancesBis)
  c(gridListe,distj, j, distjBis, jBis)
}

#Step 2 : create "cities" on grid

# Exemple : création d'un centre
centresValues1 = c(6.25,6.25,20000,3)
centresValues = data.frame(t(centresValues1))
#centresValues = data.frame(t(cbind(centresValues1,centresValues2)))
names(centresValues) = c("X0","Y0","A","b")

parametersValues = data.frame(t(c(0,12.5,0,12.5,0.25,0.05)))
names(parametersValues) = c("Xmin","Xmax","Ymin","Ymax","nu","eta")
grid1 = createTable(parametersValues,centresValues)
afficher3d(grid1)


# 
# afficher3d(grid)
# 
# grid1 = grid



#Step 2 : compute indicators
#Density threshold 200 correspond to a minimum density of 3200 inh. / km2
# reportPopulation(grid1,0)
# reportPopulation(grid1,200)
# plot(grid1$Z)
# reportDensity(grid1,0)
# densite <- reportDensity(grid1,256)/2000
# 
# 
# sum(grid1$Z[grid1$Z > 2560]) / 400 * 256 / 100 #densité nette 10 en hab / ha
# distance <- reportDistance(grid1,0)
# # reportDistance(grid1,200)
# # reportEntropy(grid1,0)
# # reportEntropy(grid1,200)
# #reportMoran(grid1,0)
# moran <- reportMoran(grid1,0)
# ranksize <- reportRankSize(grid1,0)[1]
#reportRankSize(grid1,200)

#> partir de mon article cybergeo ou des grilles empiriques de Juste pour avoir des valeurs "r?alistes" des indicateurs sur des grilles 20*20
#justification en PJ

classe1 <-  c(40,10,0.75,0.05,1.4) #villes discontinues
classe2 <-  c(40,8,0.8,0.1,1.4) #villes polycentriques
classe3 <- c(60,6,0.7,0.1,1.7) #villes monocentriques
minInd <- c(30,4,0.5,0,1) # min utilisé pour normaliser en intervalle [0,1]
maxInd <- c(90,12,1,0.2,2) # max utilisé pour normaliser en intervalle [0,1]
referenceGrids <- cbind(classe1,classe2,classe3,minInd,maxInd)





#Génération de plusieurs centres y compris des "puits"
final = cbind(blah,blah)
#initialisation of the routine
j = 1
# recipient <- NULL


N = floor(runif(1,1,10))
tempTab = NULL
i = 1
for (i in 1:N) {
  x = c(runif(1,-19.5,39.5),runif(1,-19.5,39.5),10^runif(1,1,4)+runif(1,-100,0),runif(1,0,1))
  tempTab = cbind(tempTab,x)
}
centresValues = data.frame(t(tempTab))
names(centresValues) = c("X0","Y0","A","b")

parametersValues = data.frame(t(c(0.5,19.5,0.5,19.5,1,0.25)))
names(parametersValues) = c("Xmin","Xmax","Ymin","Ymax","nu","eta")
grid = createTable(parametersValues,centresValues)
sum(grid$Z)
grid$Z <- (400000 / sum(grid$Z)) * grid$Z
afficher3d(grid)



write.table(grid,paste("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/gridAnas_",j,sep=""), sep=",", dec = ".", row.names=FALSE)
#plot(grid$Z)
#min(grid$Z)
#recipient <- cbind(recipient,*
blah <- c(reportClasse(grid,referenceGrids),j)
final = cbind(final,blah)
 j = j + 1



write.table(final,"/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/reportAnas", sep=",", dec = ".", row.names=FALSE)




#Step3 : systematically import Grids from JR
#Step3bis : force population to 400,000
#Step 4: assess the city-iness of the grid
i <- 1
/home/flo/Dropbox/Boulot/PROJ-Schelling-2015

gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-2797320021_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-10023783501_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-10193601991_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-10193601992_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-10325968621_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-10644204881_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-10802723961_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1082997411_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1112726332_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-11159428422_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-11258480653_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-11966552433_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-12537598382_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1264657812_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1264657813_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-12835180821_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-12894292213_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-13453456642_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-13545381501_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1356081212_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-13748108753_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-13976619491_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-14141373302_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-14144224492_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-14696451042_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-14783475733_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-15015939242_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-15217897402_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-15319699791_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1551999211_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-15521851023_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-15972100301_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1646180873_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1715308971_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-17222241693_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-17342957091_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-17797929751_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-17964655302_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-18331011491_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-18394125103_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-18419985372_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-18652828473_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1879355941_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-18905927952_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-19438823361_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-1991608011_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-2012382101_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-20873709023_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-21047192511_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-21083256512_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-21203266422_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-21316479091_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-255738002_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-2797320021_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-294269353_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-3007472582_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-3288843812_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-3640078881_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-3738625282_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-3857270652_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-3934658461_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-425250383_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-4402628373_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-4799083741_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-50236341_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-5328387501_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-5345567991_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-537316711_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-5604635073_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-5694203882_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-5814156511_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-6223040471_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-6266391331_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-6416468631_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-6784683073_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-6822872921_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-7266963241_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-7352617343_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-777213671_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-8082686721_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-8153468482_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-8382520751_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-8618891001_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-9082690653_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-9510150192_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/-9595176101_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/10099110921_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/10248655693_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/10284030502_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/10503329411_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/10665245701_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/10703081612_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11360728453_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11438580621_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11532223111_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11559294023_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11708260872_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11727605552_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/11755667321_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/12129481641_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/12164327993_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/12294225871_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/12294555082_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/12588498172_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/13284182713_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/14440052101_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/14523208601_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/14525407863_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/14539866801_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/14587718181_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/14913241421_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/15482502232_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/1549485161_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/15633081551_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/1580825821_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/1621276503_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/16435732702_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/16528562122_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/16643977991_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/1717528912_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/17205261141_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/17340771471_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/18174676073_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/18290401422_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/18356937111_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/18867189022_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/1893712123_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/18939891641_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/19055430491_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/19280079903_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/19875405692_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/20032482691_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/20533943511_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/20697911971_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/20721264603_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/20926881113_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/21156392381_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/21156392382_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/21237848072_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/21276310431_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/21329614002_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/21452524641_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/2235453813_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/2724895301_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/2875275861_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/3707473041_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/4477439241_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/4849075441_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/5520346191_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/5546962351_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/5647008511_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/6027881373_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/6146548561_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/6435306151_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/6829794361_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/6997988603_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/7038047091_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/7356620371_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/7356620372_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/7476777861_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/7793085472_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/7955112411_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/8457680812_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/8616417833_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/866941941_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/8742228911_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/8799147322_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/884108911_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/9043528731_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/9314170552_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1
gridJR <- read.csv("/home/flo/Dropbox/Boulot/PROJ-Schelling-2015/processed/processed/960536221_config.csv"); names(gridJR) <- c("X","Y","Z"); gridJR$Z <- (400000 / sum(gridJR$Z)) * gridJR$Z; reportClasse (gridJR, referenceGrids); i <- i + 1

afficher3d(gridJR)




#> calculer les indicateurs de mon article Cybergeo pour voir la proximit? au barycentre de chacune des classes selon une m?thode de calcul de distance euclidienn
> afficher l'histogramme de la distribution de ces distances min ? une classe donn?e
> manuellement regarder la distribution et d?terminer des "seuils naturels" permettant de distinguer deux paquets : ceux qui sont raisonnablement proches d'une classe "r?aliste pour l'europe", et les autres
> affecter chaque grille candidate ? une classe pour les mod?les ? suivre sur Schelling
> faire une classification ascendante hi?rarchique des autres pour discuter des types de villes "non cr?dibles" obtenues