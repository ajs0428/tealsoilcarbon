library(stats, dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(raster)
#library(terra)
library(rgdal)
library(corrplot)

#setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
#wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 




#Define dataframe
mash_check <- read.csv("MASHEL SOIL CARBON/MASHEL_SOIL_CARBON_SAMPLED2.csv")
names(mash_check) <- c('ID', 'sample', 'X', 'Y', 'lon', 'lat', 'carbon', 'DTW', 'SCA', 'SLP', 'TWI', 'NDVI', 'GEO', 'WIP', 'DEM')
#mash_check$PENN_LANDF<-as.factor(mash_check$PENN_LANDF)
head(mash_check)
to_keep <-  which(sapply(mash_check,is.numeric))
numeric_mash <- mash_check[ , to_keep]
typeof(mash_check$PENN_LANDF)

corrplot(cor( numeric_mash))

plot(mash_check$WIP, mash_check$carbon)

#Linear model for WIP relationship
WIP_lm <- lm(carbon ~ WIP, data = mash_check);summary(WIP_lm)

#linear mixed model with class as NDVI effect
lmer_mod <- lmer(carbon ~ log(DTW) + WIP + NDVI + log(SCA) +(1|GEO), data = mash_check);summary(lmer_mod)
anova(lmer_mod) #check on factors, none significant, not sure how to interpret


#check the predictioins of the model vs the actual 
plot(predict(lmer_mod), mash_check$carbon, col = mash_check$GEO, pch = 19)
legend("bottomright", legend = paste("Group",  1:5), col = 1:5, pch = 19, bty = "n")
# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
  res <- y-y.fitted
  1-sum(res^2)/sum((y-mean(y))^2)
}
r.sq(mash_check$carbon, fitted(lmer_mod)) 


SCA <- raster(paste0('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI_SCA.tif'))
DTW <- raster(paste0('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_DTW.tif'))
WIP <- raster(paste0( 'A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_WIP_clip.tif'))
NDVI <- raster(paste0('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_NDVI.tif'))
GEO <- raster(paste0("A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_geo_resamp.tif"))

extent(SCA) == extent(WIP)
extent(NDVI) == extent(WIP)
extent(NDVI)
crs(GEO)
crs(WIP)


#GEO_rpj <- projectRaster(GEO, WIP)
GEO_rspl <- raster::resample(GEO, WIP, method = 'ngb')
crs(GEO_rpj)
extent(GEO_rpj) == extent(WIP)

NDVI_rspl <- raster::resample(NDVI, WIP, method = 'ngb')
extent(NDVI_rspl) == extent(WIP)


#stack rasters
rs <- stack(log(DTW), WIP, log(SCA), NDVI_rspl, GEO_rspl)

#plot(GEO)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("DTW", "WIP", "SCA", "NDVI", "GEO")
rs
#looking at coefficients for fixed and random effects
betas <- fixef(lmer_mod) #fixed
rands <- ranef(lmer_mod) #random

plot(log(SCA))

#Predicting the model onto a new raster. 
# the old way of using just the coefs doesn't work because I don't know 
# how to apply a random effect
#Pred <- (DTW*betas[2]) + (TWI_rspl*betas[3] + (WIP*betas[4]) + betas[4])
Pred = predict(rs, lmer_mod) #real simple!
plot(Pred)

#write it out and save 
writeRaster(Pred, 'SPATIAL LAYERS/PREDICT_SOIL_CARBON/MASH_SOILC_PREDICT3.tif', 'GTiff')
