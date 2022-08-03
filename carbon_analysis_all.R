library(stats)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(raster)
library(terra)
library(rgdal)
library(corrplot)
library(glmnet)
library(merTools)
library(randomForest)
library(mgcv)
library(gstat)
library(mapview)


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 


#Define dataframe
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS.csv")
#all <- read.csv("ANALYSIS/ALL_SOILC_GEE_6_22_22.csv")
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS_EE_BIN.csv")
all <- read.csv("ANALYSIS/ALL_SOILC_7_31_22.csv")

head(all)
(names(all))
# names(all) <- c("STUDY_AREA", "OID_", "FID_","lon","lat","AGE_LITHOL", "AGE_LITH_C", "B2","B3","B4",
#                 "B8","CARBON","DTM","DTW","EVI","GEO","GEOLOGIC_A", "GEOLOGIC_U", "GEO_CODE",   "LITHOLOGY",
#                 "NAMED_UNIT", "NDVI","NDWI","Precip","SCA","SHAPE_Area", "SHAPE_Leng", "SLP","TWI","WIP", "GEO"  )

#subset for easier viewing and access
all <- all[c("STUDY_AREA", "sample_nam","lat", "lon", "Cstock_g_c", "Cstock_g_REV", "Cstock_Mg_",   
             "Cstock_MgREV","WIP","DEM","DTW", "PRECIP","SLP","TWI",
             "NDVI_s_0_win","MNDWI_s_0_win", "EVI_s_0_win", "b10_s_0_win",
             "NDVI_s_1_spr", "MNDWI_s_1_spr", "EVI_s_1_spr", "b10_s_1_spr",
             "NDVI_s_2_sum",  "MNDWI_s_2_sum", "EVI_s_2_sum", "b10_s_2_sum",
             "NDVI_s_3_aut",  "MNDWI_s_3_aut", "EVI_s_3_aut", "b10_s_3_aut", "GEO_7_22_22" )]
colnames(all) <- c("STUDY_AREA", "NAME","lat", "lon","C_g_cm3","C_g_cm3_REV","C_Mg_ha", 
                   "CARBON","WIP","DEM","DTW","PRECIP","SLP","TWI",
                   "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                   "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                   "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                   "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", "GEO" )
names(all)
#make this a factor
all$GEO <- as.factor(all$GEO)
#all$GEO <- as.factor(all$GEO)

# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
# LASSO Function 
lasso <- function(df){
    set.seed(1)
    x <- data.matrix(df[, c("WIP","DEM","DTW", "GEO","PRECIP","SLP","TWI",
                            "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                            "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                            "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                            "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])
    y <- df$CARBON
    
    cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
    best_lambda <- cv_model$lambda.min
    plot(cv_model)
    print(paste("best lambda:", best_lambda))
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    print(coef(best_model))
}

######Correlation plot
all_mat <- cor(all[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                                   "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                                   "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                                   "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                                   "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(all_mat, method = "number", number.cex= 20/ncol(all))

########### All data linear models ################################################################################



#LASSO for all data
lasso(all)


#Linear model for WIP relationship
WIP_lm <- lm(log(CARBON) ~ WIP, data = all);summary(WIP_lm)

#linear mixed model with the binary class as random effect
#The regular GEO layer is actually way better than the binary class. 
#But for the individual study areas the binary may be better

lmer_mod <- lmer((CARBON) ~  WIP + log10(PRECIP)  + MNDWI_sum + (1|GEO) , data = all, REML = F);summary(lmer_mod)
lmer_mod_log <- lmer(sqrt(CARBON) ~ WIP + log10(PRECIP) +  MNDWI_sum + (1|GEO), data = all, REML = F);summary(lmer_mod_log)


#check some assumptions
hist(resid(lmer_mod))
plot(predict(lmer_mod), resid(lmer_mod))
#check some log assumptions
hist(resid(lmer_mod_log))
plot(predict(lmer_mod_log), resid(lmer_mod_log)) #This is better

#evaluate the nonlog model
anova(lmer_mod) #check on factors, none significant, not sure how to interpret
r.sq(all$CARBON, fitted(lmer_mod)) 

#evaluate the log model
anova(lmer_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(all$CARBON), fitted(lmer_mod_log)) 

#check the predictions of the model vs the actual 
plot(predict(lmer_mod_log), all$CARBON, col = as.factor(all$GEO),  pch = 19)
legend("topleft", legend = paste("Group",  1:length(levels(all$GEO))), col = 1:length(levels(all$GEO)), pch = 19, bty = "n")


#checking other factors with carbon  
# plot(predict(lmer_mod), all$CARBON, col = as.factor(all$STUDY_AREA),  pch = 19)
# #plot(predict(lmer_mod), all$CARBON, col = as.factor(all$GEOLOGIC_A),  pch = 19)
# legend("bottomright", legend = paste("Group", 1:3), col = 1:6, pch = 19, bty = "n")

ggplot() +
    geom_point(aes(y = predict(lmer_mod), x = all$CARBON, colour =  as.factor(all$STUDY_AREA), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(lmer_mod), x = all$CARBON), method = "lm", se = FALSE) +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))

all_names <- names(fixef(lmer_mod_log))
all_vals <- c( unname(fixef(lmer_mod_log)) )
all_plot_dat <- data.frame(all_names, all_vals)

ggplot(all_plot_dat, aes(all_names, all_vals)) +
    geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha)') +
    theme(text = element_text(size = 20))



########### Separate Study Area data linear models ################################################################################


##### Dataframe setup - skip GEO, already in dataframe #############
hoh <- subset(all, STUDY_AREA == "HOH")
col <- subset(all, STUDY_AREA == "COL")
mas <- subset(all, STUDY_AREA == "MAS")


#################################################################

############## Hoh Study Area Model #################



###### Doing LASSO regression to identify variables

lasso(hoh) # for hoh

hoh_mat <- cor(hoh[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                       "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                       "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                       "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                       "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(hoh_mat, method = "number", number.cex= 0.5)

# Taking the coefs from the LASSO results 
#removed SCA since it improved the AIC/BIC
hoh_mod <- lmer((CARBON) ~   WIP + MNDWI_sum + (1|GEO), data = hoh, REML = F);summary(hoh_mod) #nonlog model
hoh_mod_log <- lmer(sqrt(CARBON) ~   WIP +   MNDWI_sum + (1|GEO), data = hoh, REML = F);summary(hoh_mod_log)
####### Hoh evaluate the log model
anova(hoh_mod) #check on factors, none significant, not sure how to interpret
r.sq((hoh$CARBON), fitted(lmer_mod)[31:66])  

anova(hoh_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(hoh$CARBON), fitted(hoh_mod_log)) 

#check the predictions of the model vs the actual 
plot(predict(hoh_mod), hoh$CARBON, col = as.factor(hoh$GEO),  pch = 19)
abline(lm(predict(hoh_mod) ~ hoh$CARBON))
#legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")

#check the predictions of the model vs the actual 
plot(predict(hoh_mod_log), log(hoh$CARBON), col = as.factor(hoh$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:3), col = 1:3, pch = 19, bty = "n")

ggplot() +
    geom_point(aes(y = predict(hoh_mod), x = hoh$CARBON, colour =  as.factor(hoh$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(hoh_mod), x = hoh$CARBON), method = "lm", se = FALSE) +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))

hoh_names <- names(fixef(hoh_mod))
hoh_vals <- c( unname(fixef(hoh_mod)) )
hoh_plot_dat <- data.frame(hoh_names, hoh_vals)

ggplot(hoh_plot_dat, aes(hoh_names, hoh_vals)) +
    geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha)') +
    theme(text = element_text(size = 20))

# (PI <- predictInterval(merMod = hoh_mod, newdata = hoh, level = 0.95,
#                       n.sims = 1000, stat = "median", type = "linear.prediction",
#                       include.resid.var = T))

#################################################################

############## Mashel Study Area Model #################

mat <- cor(mas[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                       "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                       "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                       "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                       "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(mat, method = "number", number.cex= 0.5)

###### Doing LASSO regression to identify variables
lasso(mas)

#### Mashel linear mixed model with binary GEO as random effect
mas_mod <- lmer((CARBON) ~  WIP + MNDWI_sum + SLP + EVI_sum +  (1|GEO), data = mas, REML = F);summary(mas_mod)
mas_mod_log <- lmer(sqrt(CARBON) ~   WIP + MNDWI_sum + SLP + (1|GEO), data = mas, REML = F);summary(mas_mod_log)
#### Mashel check some assumptions
hist(resid(mas_mod))
plot(predict(mas_mod), resid(mas_mod))
#### Mashel  check some log assumptions
hist(resid(mas_mod_log)) #Better
plot(predict(mas_mod_log), resid(mas_mod_log))

#### Mashel  evaluate the nonlog model
anova(mas_mod) #check on factors, none significant, not sure how to interpret
r.sq(mas$CARBON, fitted(mas_mod)) # 0.2795801

#### Mashel  evaluate the log model
anova(mas_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(mas$CARBON), fitted(mas_mod_log)) # 0.2748951

plot(predict(mas_mod_log), log(mas$CARBON), col = as.factor(mas$GEO),  pch = 19)
legend("topleft", legend = paste("Group",  1:length(levels(mas$GEO))), col = 1:length(levels(mas$GEO)), pch = 19, bty = "n")

ggplot() + geom_point(aes(y = predict(mas_mod), x = mas$CARBON, colour =  as.factor(mas$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(mas_mod), x = mas$CARBON), method = "lm", se = FALSE) +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    labs(colour = "Random Effect") +
    theme(legend.position = "none", text = element_text(size = 20))

mas_names <- names(fixef(mas_mod))
mas_vals <- c( unname(fixef(mas_mod)) )
mas_plot_dat <- data.frame(mas_names, mas_vals)

ggplot(mas_plot_dat, aes(mas_names, mas_vals)) +
    geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha') +
    theme(text = element_text(size = 20))

#################################################################

############## Colville Study Area Model #################
mat <- cor(col[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                   "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                   "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                   "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                   "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(mat, method = "number", number.cex= 0.5)

###### Doing LASSO regression to identify variables
lasso(col) #weird

#### Colville linear mixed model with binary GEO as random effect
col_mod <- lmer((CARBON) ~  WIP  +  log10(PRECIP) + (1|GEO), data = col, REML = F);summary(col_mod)
col_mod_log <- lmer(sqrt(CARBON) ~   WIP  + PRECIP   +(1|GEO), data = col, REML = F);summary(col_mod_log)
#### Colville check some assumptions
hist(resid(col_mod))
plot(predict(col_mod), resid(col_mod))
#### Colville  check some log assumptions
hist(resid(col_mod_log)) #Better
plot(predict(col_mod_log), resid(col_mod_log))

#### Colville  evaluate the nonlog model
anova(col_mod) #check on factors, none significant, not sure how to interpret
r.sq(col$CARBON, fitted(col_mod)) #0.4715112

#### Colville  evaluate the log model
anova(col_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(col$CARBON), fitted(col_mod_log)) #0.5428726

plot(predict(col_mod), col$CARBON, col = as.factor(col$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:3), col = 1:3, pch = 19, bty = "n")

ggplot() + geom_point(aes(y = predict(col_mod), x = col$CARBON, colour =  as.factor(col$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(col_mod), x = col$CARBON), method = "lm", se = FALSE) +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))



col_names <- names(fixef(col_mod))
col_vals <- c( unname(fixef(col_mod)) )
col_plot_dat <- data.frame(col_names, col_vals)

ggplot(col_plot_dat, aes(col_names, col_vals)) +
    geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha') +
    theme(text = element_text(size = 20))

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


########################################################
#################### Predictions to Rasters ############################


###### Hoh Predict ######

#raster layers of variables
    # sqrt(SCA) + WIP + TWI  + (1|GEO)
# DTW <- raster('SPATIAL LAYERS/hoh_dtw_twi_classes/MainChannelDTW.tif')
# SLP <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_slp_rsmpl.tif")
# SCA <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_sca_rsmpl.tif")
# WIP <- raster("SPATIAL LAYERS/Hoh_WIP_fill.tif")
# GEO <- raster( "A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh_binary.tif")
# GEO_OG <- raster("SPATIAL LAYERS/HOH_R_RSMPL/hoh_geo_og_rsmpl.tif")
# TWI <- raster('SPATIAL LAYERS/hoh_dtw_twi_classes/TWI_resample_z10.tif')
#NDVI <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh_NDVI.tif')
#B2 <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh')
#EVI <- raster("")

PRECIP <-rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_precip_rsmpl.tif")
#DTW <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/MainChannelDTW.tif')
SLP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_slp_rsmpl.tif")
#SCA <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_sca_rsmpl.tif")
#WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_fill.tif")
WIP2 <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif.tif")
WIP <- 1-WIP2
GEO <- rast( "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_R_GEO_CROP_AGG.tif")
#NDVI_SPR <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_spec_5yr_sea1.tif")[['NDVI']] #seasons 0 = win, 1 = spr, 2 = sum, 3 = fall
#NDVI_SPR_R <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_NDVI_SPR_R.tif")#terra::resample(NDVI_SPR, WIP, method = "bilinear", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_NDVI_SPR_R.tif")
MNDWI_SUM_R <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")#resample(MNDWI_SUM, WIP, method = "bilinear")
#TWI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/TWI_resample_z10.tif')
#NDVI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_NDVI.tif')
#TEMPK <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/')
#EVI <- rast("")

#Check extents
#TWI is weird, might have to address in ArcGIS 
#For now just match extent with nearest neighbor resampling 
ext(WIP) == ext(SLP)
ext(WIP) == ext(MNDWI_SUM_R)

terra::plot(WIP, main = "1-WIP2")
#writeRaster(WIP, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH_WIP_v8_invert.tif")


#write rasters if needed

#terra::writeRaster(MNDWI_SUM_R, "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")

#stack rasters from model see below
            #lmer_mod <- lmer((CARBON) ~  WIP +  log10(PRECIP) + NDVI_spr + MNDWI_sum + (1|GEO)
rs <- c(WIP, PRECIP,  MNDWI_SUM_R, GEO)


#So slope is a factor that fucks up most of our prediction##########################


#Change names to match the dataframe extraction I think...
names(rs) <- c("WIP", "PRECIP",  "MNDWI_sum", "GEO")
#names(rs) <- c("SCA", "WIP", "TWI",  "GEO")

#looking at coefficients for fixed and random effects
betas <- fixef(hoh_mod) #fixed
rands <- ranef(hoh_mod) #random

#Predicting the model onto a new raster. 
# the old way of using just the coefs doesn't work because I don't know 
# how to apply a random effect
#Pred <- (DTW*betas[2]) + (TWI_rspl*betas[3] + (WIP*betas[4]) + betas[4])
Pred <- terra::predict(rs, lmer_mod, allow.new.levels = TRUE, filename = "HOH_CARBON_7_31_22_ASM.tif", overwrite = TRUE)
plot(Pred<0)

# predfun <- function(model, data) {
#     v <- predict(model, data, se.fit=TRUE)
#     cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
# }
# 
# (PI_se <- predictInterval(merMod = hoh_mod, newdata = rs, level = 0.95,
#                                n.sims = 1000, stat = "median", type = "linear.prediction",
#                                include.resid.var = T))
# rast(Pred_se)
# plot(Pred, range = c(0, 1200))



#write it out and save 
#writeRaster(Pred,  'SPATIAL LAYERS/PREDICT_SOIL_CARBON/HOH_CARBON_7_11_22.tif', overwrite = T)

#predictC <- raster('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/SPATIAL LAYERS/PREDICT_SOIL_CARBON/HOH_SOILC_PENN.tif')
#hoh_geomorph <- raster("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling//WBT_geomorphology/hoh_geomorphons.tif")




###### MASHEL Carbon Predict ########################################################################################


#raster layers of variables
#DTW + SLP + log(SCA) + WIP + NDVI + TWI + (1|GEO)
#DTW <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_DTW.tif')
SLP <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_TWI_SLP.tif')
#SCA <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_TWI_SCA.tif')
WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_WIP_clip.tif")
#MNDWI_SUM <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mas_spec_5yr_sea2.tif')[["MNDWI"]]
#MNDWI_SUM_R <- resample(MNDWI_SUM, WIP, method = "bilinear", filename = 'SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM.tif', overwrite = T)
MNDWI_SUM <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM.tif')#[["MNDWI"]]
#TWI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_TWI.tif')
PRECIP <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_precip_resamp7_31.tif')
#PRECIP <- resample(PRECIP, WIP, method = "bilinear", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_precip_resamp7_31.tif", overwrite = T)
GEO <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_R_GEO_CROP_AGG.tif')
plot(PRECIP)

ext(WIP) == ext(PRECIP)
ext(WIP) == ext(MNDWI_SUM)
#B2 <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh')
#EVI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mas_spec_5yr_sea2.tif')[["EVI"]]
#NDWI
#NAIPg <- raster('A:/WIP_sample_points/NAIP Imagery/Hoh/Hoh_NAIP_clip.tif', band = 2)

#GEO <- raster( "A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_DTW_binaryreclass.tif")

#GEO_OG_rspl <- raster::resample(geo_res, WIP, method='ngb')
# extent(NDVI_rspl) == extent(WIP)
# NDVI_rspl <- raster::resample(NDVI, WIP, method='ngb')
# 
#terra::writeRaster(MNDWI_SUM_R, "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM.tif", overwrite = TRUE)

#stack rasters from model see below
#           sqrt(DTW) + sqrt(SCA) + WIP +  TWI + (1|GEO),
rs <- c(WIP, PRECIP,  MNDWI_SUM, GEO)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("WIP", "PRECIP",  "MNDWI_sum", "GEO")

Pred = terra::predict(rs, lmer_mod, allow.new.levels = TRUE, filename = "MAS_CARBON_7_31_22_ASM.tif", overwrite = TRUE)
plot(Pred>0)

#write it out and save 
#writeRaster(Pred, 'SPATIAL LAYERS/PREDICT_SOIL_CARBON/MAS_CARBON_7_11_22.tif',  overwrite = T)

#########################################

###### COLVILL Carbon Predict ######

#########################################
#raster layers of variables
#DTW  + WIP + NDVI + Precip  + (1|GEO)
#DTW <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_DTW_rspl.tif")
SLP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_SLP_rspl.tif")
#SCA <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI_SCA.tif')
WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip.tif")
#MNDWI_SUM <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_spec_5yr_sea2.tif")[["MNDWI"]]
#MNDWI_SUM_R <- resample(MNDWI_SUM, WIP, method = "bilinear")
#terra::writeRaster(MNDWI_SUM_R, "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif", overwrite = TRUE)
MNDWI_SUM <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif")
#TWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_TWI_rspl.tif")
#B2 <- raster('A:/WA_tealcarbon/HOH_CARBON/SPATIAL_LAYERS/hoh')
#EVI <- raster("")
#NDWI
#NAIPg <- raster('A:/WIP_sample_points/NAIP Imagery/Hoh/Hoh_NAIP_clip.tif', band = 2)
PRECIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_PRECIP_rspl.tif")
GEO <- rast( "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_R_GEO_CROP_AGG.tif")

#extent(WIP) == extent(DTW)
#extent(WIP) == extent(SLP)
#extent(WIP) == extent(SCA)
#extent(WIP) == extent(GEO_rspl)
ext(WIP) == ext(PRECIP)
ext(WIP) == ext(GEO)
plot(GEO)
#extent(DTW) == extent(TWI)
#SLP_rspl <- raster::resample(SLP, WIP, method='ngb')
#TWI_rspl <- raster::resample(TWI, WIP, method='ngb')
# GEO_rspl <- raster::resample(GEO, WIP, method='ngb')
# NDVI_rspl <- raster::resample(NDVI, WIP, method='ngb')
# PRECIP_rspl <- raster::resample(PRECIP, WIP, method='ngb')
# 
#writeRaster(SLP_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_SLP_rspl.tif", "GTiff", overwrite = TRUE)
#writeRaster(TWI_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_TWI_rspl.tif", "GTiff", overwrite = TRUE)
# writeRaster(GEO_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_GEO_rspl.tif", "GTiff", overwrite = TRUE)
# writeRaster(NDVI_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_NDVI_rspl.tif", "GTiff", overwrite = TRUE)
# writeRaster(PRECIP_rspl, "A:/WA_tealcarbon/COLVILLE_CARBON/SPATIAL_RESAMP/col_PRECIP_rspl.tif", "GTiff", overwrite = TRUE)

#stack rasters from model see below
#           SLP + WIP  + TWI + (1|GEO_bin)
rs <- c(WIP, PRECIP, MNDWI_SUM, GEO)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("WIP", "PRECIP",   "MNDWI_sum", "GEO")

Pred = terra::predict(rs, lmer_mod, allow.new.levels = TRUE, filename = "COL_CARBON_7_31_22_ASM.tif", overwrite =T)
#plot(Pred, col=rev( rainbow( 99, start=0,end=1 ) ),zlim=c(-50,0)  )
plot(Pred<0)





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


###################### Correlogram ###########
library(gstat)
library(sp)
hoh_correl <- subset(all, STUDY_AREA == "HOH")
coords <- hoh_correl[, c("lon", "lat")]
crs_ <- CRS("+init=epsg:4326")
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = hoh_correl, 
                               proj4string = crs_)
class(spdf)
spplot(spdf, "CARBON")

vario <- variogram(CARBON~1, data = spdf)
TheVariogramModel <- vgm(psill=80000, model="Gau", nugget=20000, range=10)
plot(vario)

FittedModel <- fit.variogram(vario, model = TheVariogramModel)
plot(vario, model = FittedModel)
