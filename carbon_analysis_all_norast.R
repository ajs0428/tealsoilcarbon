library(stats)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
#library(raster)
library(terra)
#library(rgdal)
library(corrplot)
library(glmnet)


plot(terra::rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_GEO_rspl.tif"))


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 


#Define dataframe
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS.csv")
#all <- read.csv("ANALYSIS/ALL_SOILC_GEE_6_22_22.csv")
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS_EE_BIN.csv")
all <- read.csv("ANALYSIS/ALL_SOILC_7_11_22_samp.csv")

head(all)
(names(all))
# names(all) <- c("STUDY_AREA", "OID_", "FID_","lon","lat","AGE_LITHOL", "AGE_LITH_C", "B2","B3","B4",
#                 "B8","CARBON","DTM","DTW","EVI","GEO","GEOLOGIC_A", "GEOLOGIC_U", "GEO_CODE",   "LITHOLOGY",
#                 "NAMED_UNIT", "NDVI","NDWI","Precip","SCA","SHAPE_Area", "SHAPE_Leng", "SLP","TWI","WIP", "GEO"  )

#subset for easier viewing and access
all <- all[c("STUDY_AREA", "sample_nam","lat", "lon","Cstock_g_c","Cstock_g_1","Cstock_Mg_", 
            "Cstock_Mg1","WIP","DEM","DTW", "GEO","PRECIP","SLP","TWI",
            "NDVI_s_0_win","MNDWI_s_0_win", "EVI_s_0_win", "b10_s_0_win",
            "NDVI_s_1_spr", "MNDWI_s_1_spr", "EVI_s_1_spr", "b10_s_1_spr",
            "NDVI_s_2_sum",  "MNDWI_s_2_sum", "EVI_s_2_sum", "b10_s_2_sum",
            "NDVI_s_3_aut",  "MNDWI_s_3_aut", "EVI_s_3_aut", "b10_s_3_aut" )]
colnames(all) <- c("STUDY_AREA", "NAME","lat", "lon","C_g_cm3","C_g_cm3_rock","C_Mg_ha", 
                   "CARBON","WIP","DEM","DTW", "GEO","PRECIP","SLP","TWI",
                   "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                   "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                   "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                   "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut" )
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
  x <- data.matrix(df[, c("lat", "lon","WIP","DEM","DTW", "GEO","PRECIP","SLP","TWI",
                   "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                   "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                   "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                   "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])
  y <- df$CARBON
  
  cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 50)
  best_lambda <- cv_model$lambda.min
  plot(cv_model)
  print(paste("best lambda:", best_lambda))
  
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  print(coef(best_model))
}

#################################################################

########### All data linear models ###############

#################################################################

#LASSO for all data
lasso(all)


#Linear model for WIP relationship
WIP_lm <- lm(log(CARBON) ~ WIP, data = all);summary(WIP_lm)

#linear mixed model with the binary class as random effect
#The regular GEO layer is actually way better than the binary class. 
#But for the individual study areas the binary may be better

lmer_mod <- lmer((CARBON) ~  WIP  + MNDWI_sum + (1|GEO), data = all, REML = F);summary(lmer_mod)
lmer_mod_log <- lmer(sqrt(CARBON) ~ WIP +lat + lon + SLP + MNDWI_sum  + (1|GEO), data = all, REML = F);summary(lmer_mod_log)


#check some assumptions
hist(resid(lmer_mod))
plot(predict(lmer_mod), resid(lmer_mod))
#check some log assumptions
hist(resid(lmer_mod_log))
plot(predict(lmer_mod_log), resid(lmer_mod_log)) #This is better

#evaluate the nonlog model
anova(lmer_mod) #check on factors, none significant, not sure how to interpret
r.sq(all$CARBON, fitted(lmer_mod)) #0.4129633 <- not bad...

#evaluate the log model
anova(lmer_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(all$CARBON), fitted(lmer_mod_log)) #0.4477694 

#check the predictions of the model vs the actual 
plot(predict(lmer_mod_log), all$CARBON, col = as.factor(all$GEO),  pch = 19)
legend("topleft", legend = paste("Group",  1:6), col = 1:6, pch = 19, bty = "n")


#checking other factors with carbon  
# plot(predict(lmer_mod), all$CARBON, col = as.factor(all$STUDY_AREA),  pch = 19)
# #plot(predict(lmer_mod), all$CARBON, col = as.factor(all$GEOLOGIC_A),  pch = 19)
# legend("bottomright", legend = paste("Group", 1:3), col = 1:6, pch = 19, bty = "n")

ggplot() +
  geom_point(aes(y = predict(lmer_mod), x = all$CARBON, colour =  as.factor(all$GEO), size = 3)) +
  xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
  geom_smooth(aes(y = predict(lmer_mod), x = all$CARBON), method = "lm", se = FALSE) +
  #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
  labs(colour = "Random Effect") +
  theme(legend.position = 'none', text = element_text(size = 20))

all_names <- names(fixef(lmer_mod_log))
all_vals <- c( unname(fixef(lmer_mod_log)) )
all_plot_dat <- data.frame(all_names, all_vals)

all_fixplot <- ggplot(all_plot_dat, aes(all_names, all_vals)) +
  geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha)') +
  theme(text = element_text(size = 20))

#################################################################

########### Separate Study Area data linear models ###############

#################################################################

##### Dataframe setup - skip GEO, already in dataframe #############
hoh <- subset(all, STUDY_AREA == "HOH")
col <- subset(all, STUDY_AREA == "COL")
mas <- subset(all, STUDY_AREA == "MAS")

# ##### Add in GEE Extracted Spectral Variables ###########
# GEE_hoh <- read.csv("SPECTRAL/GEE_Hoh_spec.csv")
# GEE_mas <- read.csv("SPECTRAL/GEE_Mas_spec.csv")
# GEE_col <- read.csv("SPECTRAL/GEE_Col_spec.csv")
# 
# ##### Combine GEE and All data for each study area
# hoh <- cbind(hoh, GEE_hoh)
# mas <- cbind(mas, GEE_mas)
# col <- cbind(col, GEE_col)
# 
# hoh <- dplyr::select(hoh, -X)
# mas <- dplyr::select(mas, -X)
# col <- dplyr::select(col, -X)

# hoh_dtwgeomorph <- read.csv('SPATIAL LAYERS/hoh_dtw_twi_classes/hoh_DTW_binary_5_4_22.csv')
# mas_binary <- read.csv('A:/WA_tealcarbon/MASHEL_CARBON/csv/mashel_Csample_pts_sampleDTWbinary.csv')
# col_binary <- read.csv('A:/WA_tealcarbon/COLVILLE_CARBON/csv/colville_DTWbinary_sample.csv')
# 
# #hoh$GEO <- recode_factor(hoh$GEO, "Pleistocene alpine glacial drift" = 3, "Quaternary alluvium" = 2, "Tertiary sedimentary rocks and deposits" = 1)
# hoh$GEO <- as.factor(hoh_dtwgeomorph$Hoh_DTW_bi )
# mas$GEO <- as.factor(mas_binary$mashel_DTW)
# col$GEO <- as.factor(col_binary$colville_D)
# 
# com_GEO <- c(col$GEO,hoh$GEO,mas$GEO)
# all$GEO <- as.factor(com_GEO)
# 
# write.csv(all, "ANALYSIS/ALL_SOILC_PTS_EE_BIN.csv")
#################################################################

############## Hoh Study Area Model #################

#################################################################

###### Doing LASSO regression to identify variables

lasso(hoh) # for hoh


# Taking the coefs from the LASSO results 
      #removed SCA since it improved the AIC/BIC
hoh_mod <- lmer((CARBON) ~   WIP  + MNDWI_sum + (1|GEO), data = hoh, REML = F);summary(hoh_mod)
hoh_mod_log <- lmer(sqrt(CARBON) ~   WIP  + MNDWI_sum + (1|GEO), data = hoh, REML = F);summary(hoh_mod_log)
####### Hoh evaluate the log model
anova(hoh_mod) #check on factors, none significant, not sure how to interpret
r.sq((hoh$CARBON), fitted(hoh_mod))  

anova(hoh_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(hoh$CARBON), fitted(hoh_mod_log)) 

#check the predictions of the model vs the actual 
plot(predict(hoh_mod), hoh$CARBON, col = as.factor(hoh$GEO),  pch = 19)
abline(lm(predict(hoh_mod) ~ hoh$CARBON))
#legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")

#check the predictions of the model vs the actual 
plot(predict(hoh_mod_log), log(hoh$CARBON), col = as.factor(hoh$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:3), col = 1:2, pch = 19, bty = "n")

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

hoh_fixplot <- ggplot(hoh_plot_dat, aes(hoh_names, hoh_vals)) +
  geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha)') +
  theme(text = element_text(size = 20))

#################################################################

############## Mashel Study Area Model #################

#################################################################


###### Doing LASSO regression to identify variables
lasso(mas)

#### Mashel linear mixed model with binary GEO as random effect
mas_mod <- lmer((CARBON) ~ SLP + WIP +  MNDWI_sum + (1|GEO), data = mas, REML = F);summary(mas_mod)
mas_mod_log <- lmer(sqrt(CARBON) ~  SLP + WIP +  MNDWI_sum + (1|GEO), data = mas, REML = F);summary(mas_mod_log)
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
legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")

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

#################################################################
###### Doing LASSO regression to identify variables
lasso(col) #weird

#### Colville linear mixed model with binary GEO as random effect
col_mod <- lmer((CARBON) ~  SLP + WIP  + TWI  + MNDWI_sum  +(1|GEO), data = col, REML = F);summary(col_mod)
col_mod_log <- lmer(sqrt(CARBON) ~  sqrt(DTW) + WIP + (1|GEO), data = col, REML = F);summary(col_mod_log)
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
legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")

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


# ##### Write CSVs to send for data
# all_dat_analysis <- bind_rows(hoh, mas, col)
# 
# write.csv(all_dat_analysis, file = "ANALYSIS/ALL_SOILC_GEE_6_22_22")


