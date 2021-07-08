library(shiny)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)
library(lattice)

readgrids = function(x){
  density = read.delim(x, sep=",", dec=".", header=F)
  indices = as.data.frame(density[1,1:4])
  colnames(indices) = c("Rank-size slope","Moran's I","Average distance","Entropy")
  return(indices)
}
shinyServer(function(input, output) {
  values <- reactiveValues(ngrid= 'none')
  
  resultFile <- reactiveValues(pathmacro= "data/resultmacro.csv", pathmicro= "data/resultmicro.csv")
  resultTable <- reactiveValues(datamacro = NULL, datamicro = NULL)
  
  allGrids <- reactive({
    allgridfiles = list.files("data/densityGrids2",full=TRUE)
    allgrids = lapply(allgridfiles,  readgrids)
    gridTable = data.frame()
    for (i in 1:length(allgridfiles)) gridTable = rbind(gridTable,allgrids[[i]])
    gridTable = as.matrix(gridTable)
    gridTable[!is.finite(gridTable)] <- NA
    gridTable = as.data.frame(gridTable)
    cut = strsplit(allgridfiles, "[/]")
    ID = as.numeric(lapply(cut, '[[', 3))
    gridTable = cbind(ID, gridTable)
    gridTable <-   gridTable[order(gridTable[,"ID"]),]
    return(gridTable)
  })
  
  
  output$gridresults <- renderDataTable({
    tab = allGrids()
    tab = round(tab,3)
    return(tab)
  },options = list(pageLength = 10), )
  
  output$summarygrids <- renderTable({
    tab = allGrids()
    summary = summary(tab)[,-1]
    return(summary)
  })
  
  
  output$indicesgrid <- renderTable({
    ngrid =  paste("data/densityGrids2/",input$ngrid, sep="")
    indices = readgrids(ngrid)
    return(indices)
  })
  
  output$map_density <- renderPlot({
    density = read.delim(paste("data/densityGrids2/", input$ngrid, sep=""), sep=",", dec=".", header=F)
    grid = as.matrix(density[-1,])
    cap_palette <- colorRampPalette(c("white", "black"))(n = 299)
    size <- dim(grid)[[1]]
    
    mapDensity <- levelplot(grid, 
                        col.regions=cap_palette, 
                        colorkey=T ,
                        xlab="", ylab="",
                        cex.axis=0.1,
                        scales=list(x=list(at=c(0,size+1)), y=list(at=c(0,size+1))
                        )
    )
    
    return(mapDensity)
    })
  
  output$map_cell <- renderPlot({
    inMicroFile <- input$file1  
    if (is.null(inMicroFile)) {
      resultTable$datamicro <- read.csv(resultFile$pathmicro, sep=",", dec=".", header=F)
    }
    if (!is.null(inMicroFile)) {
      resultTable$datamicro <- read.csv(inMicroFile$datapath, header=input$header, sep=input$sep)
    }
  
    
   # result <- read.csv("data/result.csv", sep=",", dec=".", header=F)
    #summary(result)
   result <- resultTable$datamicro
    colnames(result) <- c("step", "x", "y", "capacity", "greens", "reds", "satisgreen", "satisred")
 result$totalPop <- result$greens + result$red
 result$pctgreens <- result$greens / result$totalPop * 100
 result$pctreds <- result$reds / result$totalPop * 100
 result$empty <- result$capacity - result$totalPop
 result$pctempty <- result$empty / result$capacity * 100
 result$satisfiedgreen <- ifelse(result$satisgreen == "false", result$greens, 0)
 result$satisfiedred <- ifelse(result$satisred == "false", result$reds, 0)
 result$satisfied <-  result$satisfiedgreen +  result$satisfiedred
 result$pctsatisfied <- result$satisfied / result$totalPop * 100
 result$pctunsatisfied <-  100 -  result$pctsatisfied
 
 resultTable$data  <- result
 
 Table <- as.data.frame(resultTable$data)
 
 currentstep <- subset(Table, step == input$step)[,-1]
 
 tempo <- melt(currentstep,
               id.vars=c("x","y"),
               measure.vars=input$var)
 tempo2 <- dcast(tempo, x~y)

 map <- as.matrix(tempo2[,-1])
 rownames(map) <- tempo2[,1]

size <- dim(map)[[1]]

if (input$var == "totalPop") my_palette <- colorRampPalette(c("white", "black"))(n = 299)
if (input$var == "pctgreens") my_palette <- colorRampPalette(c("white", "forestgreen"))(n = 299)
if (input$var == "pctreds") my_palette <- colorRampPalette(c("white", "firebrick1"))(n = 299)
if (input$var == "pctunsatisfied") my_palette <- colorRampPalette(c("white", "dodgerblue3"))(n = 100)

mapPop <- levelplot(map, 
          col.regions=my_palette, 
          colorkey=T ,
        xlab="", ylab="",
        cex.axis=0.1,
        scales=list(x=list(at=c(0,size+1)), y=list(at=c(0,size+1))
          )
        )
 
return(mapPop)
  })




output$paramtable <- renderTable({
  
  inMacroFile <- input$file2
  if (is.null(inMacroFile)) {
    resultTable$datamacro <- read.csv(resultFile$pathmacro, sep=",", dec=".", header=F)
  }
  if (!is.null(inMacroFile)) {
    resultTable$datamacro <- read.csv(inMicroFile$datapath, header=input$header, sep=input$sep)
  }
  
  param <- resultTable$datamacro[1,12:16]
  colnames(param) <-  c("Size","greenRatio","redRatio", "maxCapacity", "similarWanted")
   return(param)
})







output$measurestable <- renderTable({
  
   indexes <- resultTable$datamacro[,1:11]
  colnames(indexes) <-  c("step", "unsatisfied","dissimilarity", "moranRed","Entropy", "ExposureRed",
                          "ExposureGreen", "IsolationRed", "IsolationGreen",
                          "ConcentrationRed",  "ConcentrationGreen")
  rownames(indexes) <-   as.numeric(rownames(indexes)) - 1
  
  currentstep <- subset(indexes, step == input$step)[,-1]
 return(currentstep)
},digits = 3)

densityGrid <-reactive({
  grid <- read.csv(paste("data/densityGrids2/", nGrid, ".csv", sep=""), sep=",", dec=".", header=T)
  return(grid)
})

gridPlots <-reactive({
df <- densityGrid()
return(gridplot)
})


output$plotindexes <- renderPlot({
  df <- resultschelling()
  indexes <- df[,c("dissimilarity", "moran","entropy", "unsatisfiedRatio","exposureGreenRed", "exposureRedGreen",
                   "deltaGreenRed", "deltaRedGreen", "isolationGreenRed", "isolationRedGreen" )]
  p <- rquery.cormat(indexes, type="full")
  return(p)
})

output$plotindexes2 <- renderPlot({
  df <- resultschelling()
  indexes <- df[,c("dissimilarity", "moran","unsatisfiedRatio","exposureRedGreen")]
  p <- rquery.cormat(indexes, type="full")
  return(p)
})


# output$sensitivity1 <- renderPlot({
#   df <- sensitivityplots()
#   plotseg <- ggplot(df, aes(x=ToleranceLevel, y=segregationIndex, colour=VacancyRate)) + geom_point() 
#   return(plotseg)
# })

output$sensitivity <- renderDataTable({
  df <- sensitivityplots()
  return(df)
})
  
output$sensitivity2 <- renderPlot({
  df <- sensitivityplots()
  segregation <- summarySE(df, measurevar="segregationIndex", groupvars=c("Parameter")) 
  
  plotsegbins <- ggplot(segregation, aes(x=Parameter, y=segregationIndex)) +
    geom_errorbar(aes(ymin=segregationIndex-sd, ymax=segregationIndex+sd), 
                  width=.15, colour="dodgerblue3", size=1) + 
    geom_point(colour="dodgerblue3", size=3) 
    return(plotsegbins)
})

output$test <- renderTable({
  df <- sensitivityplots()
  segregation <- summarySE(df, measurevar="segregationIndex", groupvars=c("Parameter")) 
   return(segregation)
})
})

