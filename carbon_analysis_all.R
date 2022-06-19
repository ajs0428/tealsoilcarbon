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
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS.csv")
all <- read.csv("ANALYSIS/ALL_SOILC_PTS_EE_BIN.csv")

head(all)
(names(all))
names(all) <- c("STUDY_AREA", "OID_", "FID_","lon","lat","AGE_LITHOL", "AGE_LITH_C", "B2","B3","B4",
                "B8","CARBON","DTM","DTW","EVI","GEO","GEOLOGIC_A", "GEOLOGIC_U", "GEO_CODE",   "LITHOLOGY",
                "NAMED_UNIT", "NDVI","NDWI","Precip","SCA","SHAPE_Area", "SHAPE_Leng", "SLP","TWI","WIP"  )

all <- all[c("STUDY_AREA","lon","lat","AGE_LITH_C", "B2","B3","B4",
             "B8","CARBON","DTM","DTW","EVI","GEO_bin", 
              "NDVI","NDWI","Precip","SCA", "SLP","TWI","WIP" )]
#all$GEO <- as.factor(all$GEO)
# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}

#Checking out data on correlation plot
to_keep <-  which(sapply(all,is.numeric))
numeric <- all[ , to_keep]

corrplot(cor(numeric), method = 'number')


#Linear model for WIP relationship
WIP_lm <- lm(CARBON ~ WIP, data = all);summary(WIP_lm)

#linear mixed model with class as random effect
lmer_mod <- lmer((CARBON) ~  DTW + SLP + log(SCA) + WIP  + NDVI + B2 + EVI + NDWI + Precip + (1|GEO) + (1|STUDY_AREA), data = all);summary(lmer_mod)
lmer_mod_nogee <- lmer((CARBON) ~  DTW + SLP + log(SCA) + WIP + Precip + (1|GEO), data = all);summary(lmer_mod_nogee)
lmer_mod_log <- lmer(log(CARBON) ~ DTW + SLP + log(SCA) + WIP  + NDVI +  B2 + EVI + NDWI + Precip + (1|GEO) + (1|STUDY_AREA), data = all);summary(lmer_mod_log)
    #check some assumptions
hist(resid(lmer_mod))
plot(predict(lmer_mod), resid(lmer_mod))
    #check some log assumptions
hist(resid(lmer_mod_log))
plot(predict(lmer_mod_log), resid(lmer_mod_log)) #This is better

#evaluate the nonlog model
anova(lmer_mod) #check on factors, none significant, not sure how to interpret
r.sq(all$CARBON, fitted(lmer_mod)) #0.3798566 <- not bad...

#evaluate the non_gee model
anova(lmer_mod_nogee) #check on factors, none significant, not sure how to interpret
r.sq(all$CARBON, fitted(lmer_mod_nogee)) #0.3779428 <- not bad...

#evaluate the log model
anova(lmer_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(log(all$CARBON), fitted(lmer_mod_log)) #0.4131246 

#check the predictions of the model vs the actual 
plot(predict(lmer_mod), all$CARBON, col = as.factor(all$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:6), col = 1:6, pch = 19, bty = "n")


#checking other factors with carbon  
plot(predict(lmer_mod), all$CARBON, col = as.factor(all$STUDY_AREA),  pch = 19)
plot(predict(lmer_mod), all$CARBON, col = as.factor(all$GEOLOGIC_A),  pch = 19)
legend("bottomright", legend = paste("Group", 1:6), col = 1:6, pch = 19, bty = "n")



#################### Let's break up into the three study areas to examine #######################################
hoh <- subset(all, STUDY_AREA == "HOH")
col <- subset(all, STUDY_AREA == "COL")
mas <- subset(all, STUDY_AREA == "MAS")

hoh_dtwgeomorph <- read.csv('SPATIAL LAYERS/hoh_dtw_twi_classes/hoh_DTW_binary_5_4_22.csv')
hoh_naip <- read.csv('SPATIAL LAYERS/hoh_NAIP_sample.csv')
mas_binary <- read.csv('A:/WA_tealcarbon/MASHEL_CARBON/csv/mashel_Csample_pts_sampleDTWbinary.csv')
col_binary <- read.csv('A:/WA_tealcarbon/COLVILLE_CARBON/csv/colville_DTWbinary_sample.csv')
hoh_slp <- read.csv('SPATIAL LAYERS/Hoh_Cpts_scales.csv')

#hoh$GEO <- recode_factor(hoh$GEO, "Pleistocene alpine glacial drift" = 3, "Quaternary alluvium" = 2, "Tertiary sedimentary rocks and deposits" = 1)
hoh$GEO <- as.factor(hoh_dtwgeomorph$Hoh_DTW_bi )
hoh$green <- hoh_naip$Green
hoh$blue <- hoh_naip$Blue
hoh$nir <- hoh_naip$NIR
hoh$SLP1000 <- hoh_slp$slp1000
hoh$SLP300 <- hoh_slp$slp300
hoh$SLP50 <- hoh_slp$slp50
mas$GEO <- as.factor(mas_binary$mashel_DTW)
col$GEO <- as.factor(col_binary$colville_D)

#### Hoh #####

###### Hoh linear mixed model with class as random effect
#hoh_mod <- lmer((CARBON) ~ DTW + SLP + log(SCA) + WIP + NDVI  +  B2 + EVI + NDWI + Precip + (1|GEO), data = hoh);summary(hoh_mod)
hoh_mod_nogee <- lmer((CARBON) ~  sqrt(SCA) + WIP + TWI  + SLP50 + (1|GEO_bin), data = hoh);summary(hoh_mod_nogee)
#hoh_mod_log <- lmer(log(CARBON) ~ DTW + SLP + log(SCA) + WIP + NDVI  +  B2 + EVI + NDWI + Precip + (1|GEO), data = hoh);summary(hoh_mod_log)
# ####### Hoh check some assumptions
# hist(resid(hoh_mod))
# plot(predict(hoh_mod), resid(hoh_mod))
# ####### Hoh check some log assumptions
# hist(resid(hoh_mod_log)) #THis looks worse
# plot(predict(hoh_mod_log), resid(hoh_mod_log))

# ####### Hoh evaluate the nonlog model
# anova(hoh_mod) #check on factors, none significant, not sure how to interpret
# r.sq(hoh$CARBON, fitted(hoh_mod)) #0.5357225 

####### Hoh evaluate the log model
anova(hoh_mod_nogee) #check on factors, none significant, not sure how to interpret
r.sq((hoh$CARBON), fitted(hoh_mod_nogee)) #0.3515128 

#check the predictions of the model vs the actual 
plot(predict(hoh_mod_nogee), hoh$CARBON, col = as.factor(hoh$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")

#### Mashel #####
 ########################### Way worse with added factors
#### Mashel linear mixed model with class as random effect
mas_mod <- lmer((CARBON) ~ sqrt(DTW) + sqrt(SCA) + WIP +  TWI + (1|GEO_bin), data = mas);summary(mas_mod)
mas_mod_log <- lmer(log(CARBON) ~ sqrt(DTW) + sqrt(SCA) + WIP +  TWI + (1|GEO_bin), data = mas);summary(mas_mod_log)
#### Mashel check some assumptions
hist(resid(mas_mod))
plot(predict(mas_mod), resid(mas_mod))
#### Mashel  check some log assumptions
hist(resid(mas_mod_log)) #Better
plot(predict(mas_mod_log), resid(mas_mod_log))

#### Mashel  evaluate the nonlog model
anova(mas_mod) #check on factors, none significant, not sure how to interpret
r.sq(mas$CARBON, fitted(mas_mod)) #0.4197816 LOOK at worse -> 0.3035841

#### Mashel  evaluate the log model
anova(mas_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(log(mas$CARBON), fitted(mas_mod_log)) #0.4090441 0.2578615


#### Colville #####

#### Colville linear mixed model with class as random effect
col_mod <- lmer((CARBON) ~  SLP + WIP  + TWI + (1|GEO_bin), data = col);summary(col_mod)
col_mod_log <- lmer(log(CARBON) ~ DTW + SLP + log(SCA) + WIP + NDVI  +  B2 + EVI + NDWI + Precip  + (1|GEO_bin), data = col);summary(col_mod_log)
#### Colville check some assumptions
hist(resid(col_mod))
plot(predict(col_mod), resid(col_mod))
#### Colville  check some log assumptions
hist(resid(col_mod_log)) #Better
plot(predict(col_mod_log), resid(col_mod_log))

#### Colville  evaluate the nonlog model
anova(col_mod) #check on factors, none significant, not sure how to interpret
r.sq(col$CARBON, fitted(col_mod)) #0.6299124 

#### Colville  evaluate the log model
anova(col_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(log(col$CARBON), fitted(col_mod_log)) #0.679118 

plot(predict(col_mod), col$CARBON, col = as.factor(col$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")



#defining a function to repeat the modeling process
#lmer_model <- function(carbon, a, b, c, d, e, f, g, h, i, j, k)
lmer_func <- function(data){
    lmer_mod <- lmer(CARBON ~ DTW + SLP + log(SCA) + WIP + NDVI  +  B2 + EVI + NDWI + Precip  + (1|GEO), data = data)
    lmer_mod_log <- lmer(log(CARBON) ~ DTW + SLP + log(SCA) + WIP + NDVI  +  B2 + EVI + NDWI + Precip  + (1|GEO), data = data)
    print(lmer_mod)
    print(lmer_mod_log)
    
    # return(lmer_mod)
    # return(lmer_mod_log)
    
    r.sq <- function(y,y.fitted){
        res <- y-y.fitted
        1-sum(res^2)/sum((y-mean(y))^2)
    }
    
    r2 <- r.sq(data$CARBON, fitted(lmer_mod))
    r2_log <- r.sq(log(data$CARBON), fitted(lmer_mod_log))
    
    print(r2)
    print(r2_log)
    
    # return(r2)
    # return(r2_log)
}

#################### Predictions to Rasters ############################


###### Hoh Predict ######

#raster layers of variables
    # sqrt(SCA) + WIP + TWI  + (1|GEO)
DTW <- raster('SPATIAL LAYERS/hoh_dtw_twi_classes/MainChannelDTW.tif')
SLP <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_slp_rsmpl.tif")
SCA <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_sca_rsmpl.tif")
WIP <- raster("SPATIAL LAYERS/Hoh_WIP_fill.tif")
GEO <- raster( "A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh_binary.tif")
GEO_OG <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_geo_og_rsmpl.tif")
TWI <- raster('SPATIAL LAYERS/hoh_dtw_twi_classes/TWI_resample_z10.tif')
#NDVI <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh_NDVI.tif')
#B2 <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh')
#EVI <- raster("")
#NDWI
#NAIPg <- raster( "SPATIAL LAYERS/HOH_R_RSMPL/hoh_NAIPgreen_rpj.tif")
#PRECIP <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_precip_rsmpl.tif")

    #next try to do this with the new clipped raster

#LAND <- raster('SPATIAL LAYERS/hoh_con_pennDTW_flood_reclass.tif')

#NAIPg_rpj <- projectRaster(NAIPg, (WIP))

#Check extents
#TWI is weird, might have to address in ArcGIS 
#For now just match extent with nearest neighbor resampling 
extent(WIP) == extent(DTW)
extent(WIP) == extent(SLP)
extent(WIP) == extent(SCA)
extent(WIP) == extent(TWI)
#extent(WIP) == extent(NAIPg_rpj)
#extent(NDVI) <- alignExtent(extent(NDVI), WIP, snap = 'near')
#and
#SLP_rspl <- raster::resample(SLP, WIP, method='ngb')
#SCA_rspl <- raster::resample(SCA, WIP, method='ngb')
#PRECIP_rspl <- raster::resample(PRECIP, WIP, method='ngb')
GEO_OG_rspl <- raster::resample(GEO_OG, WIP, method='ngb')
#NAIP_rspl <- raster::resample(NAIP_rpj, WIP, method = 'ngb')

plot(GEO_OG_rspl)

#write rasters if needed
#writeRaster(SLP_rspl, "SPATIAL LAYERS/HOH_R_RSMPL/hoh_slp_rsmpl.tif", "GTiff", overwrite = TRUE)
#writeRaster(SCA_rspl, "SPATIAL LAYERS/HOH_R_RSMPL/hoh_sca_rsmpl.tif", "GTiff", overwrite = TRUE)
#writeRaster(PRECIP_rspl, "SPATIAL LAYERS/HOH_R_RSMPL/hoh_precip_rsmpl.tif", "GTiff", overwrite = TRUE)
raster::writeRaster(GEO_OG_rspl, "SPATIAL LAYERS/HOH_R_RSMPL/hoh_geo_og_rsmpl.tif", "GTiff", overwrite = TRUE)
#writeRaster(NAIPg_rpj, "SPATIAL LAYERS/HOH_R_RSMPL/hoh_NAIPgreen_rpj.tif", "GTiff", overwrite = TRUE)
#stack rasters from model see below
            #(CARBON) ~  log(SCA) + WIP + TWI  + green + (1|GEO)
rs <- stack(sqrt(SCA), WIP, TWI, GEO)

#############
#So slope is a factor that fucks up most of our prediction
#############

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("SCA", "WIP", "TWI",  "GEO")

#looking at coefficients for fixed and random effects
betas <- fixef(lmer_mod_nogee) #fixed
rands <- ranef(lmer_mod_nogee) #random

#Predicting the model onto a new raster. 
# the old way of using just the coefs doesn't work because I don't know 
# how to apply a random effect
#Pred <- (DTW*betas[2]) + (TWI_rspl*betas[3] + (WIP*betas[4]) + betas[4])
Pred = predict(rs, hoh_mod_nogee) #real simple!
plot(Pred)

#write it out and save 
writeRaster(Pred,  'SPATIAL LAYERS/PREDICT_SOIL_CARBON/HOH_CARBON_5_4_22.tif', 'GTiff', overwrite = T)

#predictC <- raster('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/SPATIAL LAYERS/PREDICT_SOIL_CARBON/HOH_SOILC_PENN.tif')
#hoh_geomorph <- raster("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling//WBT_geomorphology/hoh_geomorphons.tif")


#########################################

###### MASHEL Carbon Predict ######

#########################################
#raster layers of variables
#DTW + SLP + log(SCA) + WIP + NDVI + TWI + (1|GEO)
DTW <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_DTW.tif')
SLP <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI_SLP.tif')
SCA <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI_SCA.tif')
WIP <- raster("A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_WIP_clip.tif")
NDVI <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_NDVI.tif')
TWI <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI.tif')
geo_res <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_geo.tif')
#B2 <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh')
#EVI <- raster("")
#NDWI
#NAIPg <- raster('A:/WIP_sample_points/NAIP Imagery/Hoh/Hoh_NAIP_clip.tif', band = 2)
#PRECIP <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh_precip_proj_clip.tif')
GEO <- raster( "A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_DTW_binaryreclass.tif")

GEO_OG_rspl <- raster::resample(geo_res, WIP, method='ngb')
# extent(NDVI_rspl) == extent(WIP)
# NDVI_rspl <- raster::resample(NDVI, WIP, method='ngb')
# 
raster::writeRaster(GEO_OG_rspl, "A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_geo_rspl.tif", "GTiff", overwrite = TRUE)

#stack rasters from model see below
#           sqrt(DTW) + sqrt(SCA) + WIP +  TWI + (1|GEO),
rs <- stack(sqrt(DTW), sqrt(SCA), WIP, TWI, GEO)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c('DTW',  'SCA', 'WIP', 'TWI', 'GEO_bin')

Pred = predict(rs, mas_mod) #real simple!
plot(Pred, col=rev( rainbow( 99, start=0,end=1 ) ),zlim=c(0,1000)  )
plot(Pred)

#write it out and save 
writeRaster(Pred,  'SPATIAL LAYERS/PREDICT_SOIL_CARBON/MAS_CARBON_5_6_22.tif', 'GTiff', overwrite = T)

#########################################

###### COLVILL Carbon Predict ######

#########################################
#raster layers of variables
#DTW  + WIP + NDVI + Precip  + (1|GEO)
DTW <- raster("A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_DTW_rspl.tif")
SLP <- raster("A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_SLP_rspl.tif")
#SCA <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI_SCA.tif')
WIP <- raster("A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_LAYERS/colville_NWI_clip.tif")
NDVI <- raster("A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_NDVI_rspl.tif")
TWI <- raster("A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_TWI_rspl.tif")
#B2 <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh')
#EVI <- raster("")
#NDWI
#NAIPg <- raster('A:/WIP_sample_points/NAIP Imagery/Hoh/Hoh_NAIP_clip.tif', band = 2)
PRECIP <- raster("A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_PRECIP_rspl.tif")
GEO <- raster( "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_GEO_rspl.tif")

extent(WIP) == extent(DTW)
extent(WIP) == extent(SLP)
#extent(WIP) == extent(SCA)
extent(WIP) == extent(GEO_rspl)
extent(WIP) == extent(NDVI)
extent(DTW) == extent(GEO)
extent(DTW) == extent(TWI)
#SLP_rspl <- raster::resample(SLP, WIP, method='ngb')
#TWI_rspl <- raster::resample(TWI, WIP, method='ngb')
# GEO_rspl <- raster::resample(GEO, WIP, method='ngb')
# NDVI_rspl <- raster::resample(NDVI, WIP, method='ngb')
# PRECIP_rspl <- raster::resample(PRECIP, WIP, method='ngb')
# 
#writeRaster(SLP_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_SLP_rspl.tif", "GTiff", overwrite = TRUE)
writeRaster(TWI_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_TWI_rspl.tif", "GTiff", overwrite = TRUE)
# writeRaster(GEO_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_GEO_rspl.tif", "GTiff", overwrite = TRUE)
# writeRaster(NDVI_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_NDVI_rspl.tif", "GTiff", overwrite = TRUE)
# writeRaster(PRECIP_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_PRECIP_rspl.tif", "GTiff", overwrite = TRUE)

#stack rasters from model see below
#           SLP + WIP  + TWI + (1|GEO_bin)
rs <- stack( SLP, WIP, TWI, GEO)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c( 'SLP', 'WIP', "TWI", "GEO_bin")

Pred = predict(rs, col_mod) #real simple!
plot(Pred, col=rev( rainbow( 99, start=0,end=1 ) ),zlim=c(-50,0)  )
plot(Pred)

#write it out and save 
writeRaster(Pred,  'SPATIAL LAYERS/PREDICT_SOIL_CARBON/COL_CARBON_5_6_22.tif', 'GTiff', overwrite = T)



#################### Random Forest ############################
#ignore
library(randomForest)
library(datasets)
library(caret)

rf_data <- all[c("STUDY_AREA", "lon", "lat", "CARBON",
             "Precip", "DTM", "DTW", "GEO_bin", "WIP",
             "SLP", "TWI", "SCA", "NDVI")]

set.seed(222)
ind <- sample(2, nrow(rf_data), replace = TRUE, prob = c(0.7, 0.3))
train <- rf_data[ind == 1, ]
test <- rf_data[ind == 2, ]
rf <- randomForest(CARBON~., data = train, importance=TRUE, type = "regression", rsq = T)
print(rf)

p1 <- predict(rf, newdata = test)
varImpPlot(rf)

rf$rsq[length(rf$rsq)]
rf$predicted

plot(rf$predicted, train$CARBON)

getTree(rf, k = 55)

################ PLSR ####################
#ignore
library(pls)

#train_df <- as.matrix(train)

X_train <- (train[c("Precip", "DTM", "DTW", "GEO_bin", "WIP",
                                "SLP", "TWI", "SCA", "NDVI")])
test2 <- test[c("Precip", "DTM", "DTW", "GEO_bin", "WIP",
                "SLP", "TWI", "SCA", "NDVI")]
Y_train <- (train[c("CARBON")])

my_plsr <- plsr(CARBON ~ Precip + DTM + DTW + GEO_bin + WIP + SLP + TWI + SCA +NDVI,
                 scale = TRUE, validation='CV', data = train)

summary(my_plsr)
plsr_predict <- predict(my_plsr, test2, ncomp = 4)
plot(test$CARBON, plsr_predict)
################### Bayesian from Chad #######################
library(rjags)

## read in data ##
dat <- all #read.csv("ANALYSIS/ALL_SOILC_PTS.csv")
dat

# hoh_geo_dat <- dat[dat$STUDY_AREA  == "HOH", "GEO"]
# hoh_dat <- dat[dat$STUDY_AREA  == "HOH",]
# hoh_dat <- hoh_dat[c("CARBON", "B2","B3","B4","B8",   "DTM","DTW","EVI","NDVI","NDWI","Precip","SCA" ,"SLP","TWI","WIP"  )]

geo_dat <- dat[, "GEO"]

dat <-dat[c("CARBON", "B2","B3","B4","B8","DTM","DTW","EVI","NDVI","NDWI","Precip","SCA" ,"SLP","TWI","WIP")]

y <- (dat$CARBON) #TO log or not to log...... what do we do about heteroskedasity in residuals?
X <- as.matrix(cbind(1,dat[,2:15])) 
n <- length(y)
u.idx <- as.numeric(as.factor(geo_dat))

mod <- lm(y ~ X[,-c(1)]);summary(mod)

r.sq(y,fitted(mod))

## fit Bayesian version of same model 
jags.data <- list("y" = y, "X" = X, "p" = ncol(X), "n" = n)
vars <- c("beta","sigma.sq")

model <- jags.model("ANALYSIS/R/random-effects/lm.jag", data = jags.data)

samps <- coda.samples(model, vars, 50000)

plot(samps)

round(summary(samps)[[2]],3)
summary(mod)

## get fitted values
beta.samps <- samps[[1]][,grep("beta",colnames(samps[[1]]))]
sigma.sq.samps <- samps[[1]][,grep("sigma.sq",colnames(samps[[1]]))]

y.samps <- matrix(NA, nrow = n, ncol = nrow(beta.samps))
for(i in 1:nrow(beta.samps)){
    beta.samp.1 <- matrix(beta.samps[i,], ncol = 1)
    sigma.sq.samp.1 <- sigma.sq.samps[i]
    y.samps[,i] <- X%*%beta.samp.1
}

y.fitted <- apply(y.samps, 1, median)

res <- y - y.fitted

plot(y.fitted,res, pch = 19, col = "grey");abline(h = 0, col = "red")

points(y.fitted[u.idx == 1],res[u.idx == 1], col = "red", pch = 19)
points(y.fitted[u.idx == 2],res[u.idx == 2], col = "blue", pch = 19)
points(y.fitted[u.idx == 3],res[u.idx == 3], col = "green", pch = 19)

r.sq(y,y.fitted)

############################
## random intercept model ##
jags.data <- list("y" = y, "X" = X, "p" = ncol(X), "n" = n, "u.idx" = u.idx, "q" = length(unique(u.idx)))
vars <- c("beta","sigma.sq", "u", "sigma.sq.u")

model <- jags.model("ANALYSIS/R/random-effects/hlm.jag", data = jags.data)
model_rint <- jags.model("ANALYSIS/R/random-effects/hlm.jag", data = jags.data)

samps <- coda.samples(model, vars, 50000)


## get fitted values
beta.samps <- samps[[1]][,grep("beta",colnames(samps[[1]]))]
sigma.sq.samps <- samps[[1]][,grep("sigma.sq",colnames(samps[[1]]))]
u.samps <- samps[[1]][,grep("u\\[",colnames(samps[[1]]))]
y.samps <- matrix(NA, nrow = n, ncol = nrow(beta.samps))

for(i in 1:nrow(beta.samps)){
    beta.samp.1 <- matrix(beta.samps[i,], ncol = 1)
    sigma.sq.samp.1 <- sigma.sq.samps[i]
    u.samp.1 <- u.samps[i,]
    y.samps[,i] <- X%*%beta.samp.1 + u.samp.1[u.idx]
}

y.fitted <- apply(y.samps, 1, median)

res <- y - y.fitted

plot(y.fitted,res, pch = 19, col = "grey");abline(h = 0, col = "red")

points(y.fitted[u.idx == 1],res[u.idx == 1], col = "red", pch = 19)
points(y.fitted[u.idx == 2],res[u.idx == 2], col = "blue", pch = 19)
points(y.fitted[u.idx == 3],res[u.idx == 3], col = "green", pch = 19)

r.sq(y,y.fitted)

plot(beta.samps)
plot(sigma.sq.samps)
plot(u.samps)


###############################
## random coefficients model ##
jags.data <- list("y" = y, "X" = X, "p" = ncol(X), "n" = n, "u.idx" = u.idx, "q" = length(unique(u.idx)))
vars <- c("beta.0","beta.1","beta.2","sigma.sq","sigma.sq.u0","sigma.sq.u1","sigma.sq.u2","u.0","u.1","u.2")

model <- jags.model("ANALYSIS/R/random-effects/hlm-slopes.jag", data = jags.data)

samps <- coda.samples(model, vars, 50000)

## We have a few more parameters to look at here so I'm to break them apart before plotting them
beta.samps <- samps[[1]][,grep("beta",colnames(samps[[1]]))]
sigma.sq.samps <- samps[[1]][,grep("sigma.sq",colnames(samps[[1]]))]

u0.samps <- samps[[1]][,grep("u.0\\[",colnames(samps[[1]]))]
u1.samps <- samps[[1]][,grep("u.1\\[",colnames(samps[[1]]))]
u2.samps <- samps[[1]][,grep("u.2\\[",colnames(samps[[1]]))]

y.samps <- matrix(NA, nrow = n, ncol = nrow(beta.samps))

for(i in 1:nrow(beta.samps)){
    beta.samp.1 <- matrix(beta.samps[i,], ncol = 1)
    sigma.sq.samp.1 <- sigma.sq.samps[i]
    
    u0.samp.1 <- u0.samps[i,]
    u1.samp.1 <- u1.samps[i,]
    u2.samp.1 <- u2.samps[i,]
    
    y.samps[,i] <-
        (beta.samp.1[1] + u0.samp.1[u.idx])*X[,1] +
        (beta.samp.1[2] + u1.samp.1[u.idx])*X[,2] +
        (beta.samp.1[3] + u2.samp.1[u.idx])*X[,3]
}

y.fitted <- apply(y.samps, 1, median)

r.sq(y,y.fitted)


plot(beta.samps)
plot(sigma.sq.samps)
plot(u0.samps)
plot(u1.samps)
plot(u2.samps)

round(summary(samps)[[2]],2)

