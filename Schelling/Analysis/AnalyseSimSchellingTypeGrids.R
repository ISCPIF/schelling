setwd("~/Documents/spacemattersJUSTE/Schelling/Analysis")
grid.file = read.csv("15gridsPerClass.csv")
result.file = read.csv("2017_08_10_09_42_02_LHS_ALLGRIDS.csv")

setwd("~/Documents/spacemattersJUSTE/Schelling/Grids/quantGrids")
quantgrids = data.frame()
for (g in grid.file$id){
  gp = read.csv(paste0(strsplit(as.character(g),"_")[[1]][1], "_params.csv"))
  if (dim(quantgrids)[1] == 0 ) {
    quantgrids = gp
  } else {
    quantgrids = rbind(quantgrids, gp)
  }
}

quantgrids = cbind(quantgrids, grid.file)
colnames(quantgrids) = paste0("g_", colnames(quantgrids))

results = data.frame(result.file, quantgrids[match(result.file$gridId, quantgrids$g_newID),])
results$vacancyRate = 1 - results$redRatio - results$greenRatio
results$redGreen = results$redRatio / results$greenRatio
results$greenRed = results$greenRatio / results$redRatio 
results$minorityRatio = ifelse(results$redGreen >= results$greenRed, results$redGreen, results$greenRed)
results$minorityDiff = abs(results$redRatio - results$greenRatio)

summary(results)
r = results

r$similarWanted2 = r$similarWanted * r$similarWanted
r$morpho = ifelse(r$g_classe == 2, "poly", 
                  ifelse(r$g_classe == 1, "disc", "compact"))

r$moranMinority = ifelse(r$greenRatio >= r$redRatio, r$moranRed, r$moranGreen)
# par(mfrow=c(2,2))
# plot(dissimilarity ~ vacancyRate, data = r)
# plot(dissimilarity ~ similarWanted, data = r)
# plot(dissimilarity ~ minorityRatio, data = r)
# plot(dissimilarity ~ g_classe, data = r)
r$seg = r$moranMinority
#param du modele
m.0.0 = lm(seg ~ vacancyRate + similarWanted + minorityDiff, data = r)
#param du modele + nonlinear
m.0.1 = lm(seg ~ vacancyRate + similarWanted + similarWanted2 + minorityDiff, data = r)
#modele + interactions
m.0.2 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + similarWanted * vacancyRate, data = r)

#param du modele + grid type
m.1.0 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + morpho, data = r)
#param du modele + nonlinear
m.1.1 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + morpho +  similarWanted2 , data = r)
#param du modele + interactions
m.1.2 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + morpho +  similarWanted * vacancyRate, data = r)

#param de la grille alpha + beta
m.2.0 = lm(seg ~ g_alphalocalization + g_diffusion, data = r)

#tout
m.3.0 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + g_alphalocalization + g_diffusion, data = r)
#tout + nonlinear
m.3.1 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + g_alphalocalization + g_diffusion +  similarWanted2, data = r)
#tout + interactions
m.3.2 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + g_alphalocalization + g_diffusion +  similarWanted * vacancyRate, data = r)

#tout_tout
m.4.0 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + g_alphalocalization + g_diffusion + morpho, data = r)
#tout_tout + nonlinear
m.4.1 = lm(seg ~ vacancyRate + similarWanted + minorityDiff + g_alphalocalization + g_diffusion + similarWanted2 + morpho, data = r)



summary(m.0.0)
summary(m.0.1)
summary(m.0.2)
summary(m.1.0)
summary(m.1.1)
summary(m.1.2)
summary(m.2.0)
summary(m.3.0)
summary(m.3.1)
summary(m.3.2)
summary(m.4.0)
summary(m.4.1)

min(AIC(m.0.0),
AIC(m.0.1),
AIC(m.0.2),
AIC(m.1.0),
AIC(m.1.1),
AIC(m.1.2),
AIC(m.2.0),
AIC(m.3.0),
AIC(m.3.1),
AIC(m.3.2),
AIC(m.4.0),
AIC(m.4.1))

m.4.1$coefficients
