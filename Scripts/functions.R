
#library(emdist)


#'
#' @description computes distances of phase diagrams to a reference, for given indicators
#'       
#' @param simresults : dataframe with grid ids, parameter values and indicators values
#' @param reference : dataframe with parameter values and indicator values for the reference grid
#' @param parameters : names of parameters
#' @param indicators : names of indicators
#' @param idcol : name of grid id column
#' 
distancesToRef <- function(simresults,reference,parameters,indicators,idcol){
  dists=c()
  names(parameters)=parameters
  for(id in unique(simresults[[idcol]])){
    d=left_join(reference,simresults[simresults[[idcol]]==id,],by=parameters)
    # remove nas -> do it at the indicator level
    #d = d[apply(d,1,function(r){length(which(is.na(r)))==0}),]
    currentdist=0
    for(indic in indicators){
      x = d[[paste0(indic,'.x')]][(!is.na(d[[paste0(indic,'.x')]]))&(!is.na(d[[paste0(indic,'.y')]]))]
      y = d[[paste0(indic,'.y')]][(!is.na(d[[paste0(indic,'.x')]]))&(!is.na(d[[paste0(indic,'.y')]]))]
      if(sd(x)+sd(y)>0){currentdist=currentdist+2*(sum((x-y)^2)/length(x))/(sd(x)^2+sd(y)^2)}
    }
    dists=append(dists,currentdist/length(indicators))
  }
  names(dists)=unique(sres[[idcol]])
  return(dists)
  
  #show(id)
  #dists=append(dists,emd(as.matrix(sref[,c(4,1:3)]),as.matrix(sres[sres$id==id,c(6,3:5)])))
  #show(length(sres$gini[sres$id==id]))
  #dists=append(dists,2*(sd(d$gini.x-d$gini.y,na.rm = T)^2)/(sd(d$gini.x)^2+sd(d$gini.y,na.rm = T)^2))
  
}


#'
#' @description computes principal morphological components
morphoPCA <- function(morph){
  # normalizes
  for(j in 2:ncol(morph)){morph[,j]<-(morph[,j]-min(morph[,j]))/(max(morph[,j])-min(morph[,j]))}
  pca=prcomp(morph[,2:ncol(morph)])
  morph=data.frame(as.matrix(morph[,2:ncol(morph)])%*%pca$rotation,morph)
  return(
    list(
       morph = morph[!duplicated(morph$id),],
       rotation = pca$rotation
    )
  )
}



