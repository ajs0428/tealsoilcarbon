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

################# Spatial mapping #########################
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/TWI_NAD83Z10.tif'))
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/MainChannelDTW.tif'))
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/riv_wet_up_clean.tif'))
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_con_pennDTW_flood_reclass.tif'))


TWI <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/TWI_NAD83Z10.tif'))
DTW <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/MainChannelDTW.tif'))
CLASS <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/riv_wet_up_clean.tif'))
LAND <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_con_pennDTW_flood_reclass.tif'))
WIP <- raster(paste0(wd, "SPATIAL LAYERS/Hoh_WIP_fill.tif"))
TREE <- raster(paste0(wd, "SPATIAL LAYERS/Hoh_TreeHeight.tif"))
GEO <- raster(paste0(wd, "SPATIAL LAYERS/hoh_100kgeo.tif"))

crs(TWI)
crs(DTW)
crs(CLASS)
crs(WIP) 
crs(LAND) 
crs(TREE)
crs(GEO)

#Check extents
#TWI is weird, might have to address in ArcGIS 
    #For now just match extent with nearest neighbor resampling 
extent(WIP)
extent(TWI)
extent(CLASS)
extent(LAND)
extent(GEO)
extent(CLASS) == extent(DTW)
extent(LAND) == extent(DTW)
extent(CLASS) == extent(TWI)
extent(GEO_rspl) == extent(WIP)
#extent(TWI) <- alignExtent(DTW, TWI, snap = 'near')
extent(LAND) <- alignExtent(DTW, LAND, snap = 'near')
extent(TREE) <- alignExtent(DTW, TREE, snap = 'near')
extent(GEO) <- alignExtent(GEO, WIP, snap = 'near')
#TWI_rspl <- raster::resample(TWI, DTW, method='ngb')
LAND_rspl <- raster::resample(LAND, DTW, method='ngb')
TREE_rspl <- raster::resample(TREE, DTW, method = 'ngb')
GEO_rspl <- raster::resample(GEO, WIP, method = 'ngb')
str(LAND_rspl)
str(TREE_rspl)
#writeRaster(TWI_rspl, "SPATIAL LAYERS/TWI_resample_z10.tif", "GTiff")
writeRaster(LAND_rspl, "SPATIAL LAYERS/PENN_LANDFORMS_resample_z10_R.tif", "GTiff", overwrite = TRUE)
plot(LAND_rspl)
writeRaster(GEO_rspl, "SPATIAL LAYERS/GEO_resample_z10_R.tif", "GTiff", overwrite = TRUE)

plot(GEO_rspl)

#Define dataframe
hoh_check <- read.csv("ANALYSIS/R/hoh_c_wip_check.csv")
#hoh_check$PENN_LANDF<-as.factor(hoh_check$PENN_LANDF)
head(hoh_check)
to_keep <-  which(sapply(hoh_check,is.numeric))
numeric_hoh <- hoh_check[ , to_keep]
typeof(hoh_check$PENN_LANDF)

corrplot(cor( numeric_hoh))

plot(hoh_check$DEV_300, hoh_check$carbon)

#Linear model for WIP relationship
WIP_lm <- lm(carbon ~ WIP_CHECK2, data = hoh_check);summary(WIP_lm)

#linear mixed model with class as random effect
lmer_mod <- lmer(carbon ~ DTW_CHECK + TWI_CHECK + WIP_CHECK2 + (1|RIV_WET_UP_CLEAN), data = hoh_check);summary(lmer_mod)
anova(lmer_mod) #check on factors, none significant, not sure how to interpret

#linear mixed model with PENN_LANDF as random effect
lmer_mod <- lmer(carbon ~  TWI_CHECK + (DTW_CHECK) + WIP_CHECK2  + (1|hoh_geo), data = hoh_check);summary(lmer_mod)
anova(lmer_mod)

#check the predictioins of the model vs the actual 
plot(predict(lmer_mod), hoh_check$carbon, col = hoh_check$hoh_geo, pch = 19)

plot(predict(lmer_mod), hoh_check$carbon, col = hoh_check$PENN_LANDF, pch = 19)
legend("bottomright", legend = paste("Group",  1:3), col = 1:3, pch = 19, bty = "n")

#checking other factors with carbon 
plot(hoh_check$WIP_CHECK2, hoh_check$carbon, col = hoh_check$RIV_WET_UP_CLEAN)
plot(hoh_check$SLP_CHECK, hoh_check$carbon, col = hoh_check$RIV_WET_UP_CLEAN)
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")

# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
r.sq(hoh_check$carbon, fitted(lmer_mod)) #0.4863715 <- not bad...

#stack rasters
rs <- stack(TWI_rspl, WIP, GEO_rspl)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("TWI_CHECK", "WIP_CHECK2", "hoh_geo")

#looking at coefficients for fixed and random effects
betas <- fixef(lmer_mod) #fixed
rands <- ranef(lmer_mod) #random

#Predicting the model onto a new raster. 
    # the old way of using just the coefs doesn't work because I don't know 
    # how to apply a random effect
#Pred <- (DTW*betas[2]) + (TWI_rspl*betas[3] + (WIP*betas[4]) + betas[4])
Pred = predict(rs, lmer_mod) #real simple!
plot(Pred)

#write it out and save 
writeRaster(Pred, 'SPATIAL LAYERS/PREDICT_SOIL_CARBON/HOH_SOILC_PENN.tif', 'GTiff')

predictC <- raster('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/SPATIAL LAYERS/PREDICT_SOIL_CARBON/HOH_SOILC_PENN.tif')
#hoh_geomorph <- raster("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling//WBT_geomorphology/hoh_geomorphons.tif")

#plot(hoh_geomorph)
