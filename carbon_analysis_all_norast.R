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
#library(randomForest)
#library(mgcv)
#library(gstat)
#library(mapview)
#library(kit)


#Different working directories for Mac/Windows
setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 


#Define dataframe
all <- read.csv("ANALYSIS/ALL_SOILC_7_31_22.csv")
# head(all)
(names(all))

#subset for easier viewing and access
all <- as.tibble(all) %>% dplyr::select(STUDY_AREA, sample_nam,lat, lon, 
                                        #Cstock_g_c, Cstock_g_REV, Cstock_Mg_,   #Don't use these carbon values
                                        Cstock_MgREV,WIP,DEM,DTW, PRECIP,SLP,TWI,
                                        NDVI_s_0_win,MNDWI_s_0_win, EVI_s_0_win, b10_s_0_win,
                                        NDVI_s_1_spr, MNDWI_s_1_spr, EVI_s_1_spr, b10_s_1_spr,
                                        NDVI_s_2_sum,  MNDWI_s_2_sum, EVI_s_2_sum, b10_s_2_sum,
                                        NDVI_s_3_aut,  MNDWI_s_3_aut, EVI_s_3_aut, b10_s_3_aut, GEO_7_22_22) %>%
    stats::setNames(c("STUDY_AREA", "NAME","lat", "lon",
                      #"C_g_cm3","C_g_cm3_REV","C_Mg_ha", 
                      "CARBON","WIP","DEM","DTW","PRECIP","SLP","TWI",
                      "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                      "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                      "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                      "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", "GEO" )) 
names(all)
#make GEO a factor just in case
all$GEO <- as.factor(all$GEO)


# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
# LASSO Function to help examine potential predictors
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


masInv <- head(arrange(all, desc(CARBON)), 3)
corrplot::corrplot(cor(masInv[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                                  "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                                  "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                                  "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                                  "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")]))

########### All data linear models ################################################################################



#LASSO for all data
lasso(all)


#Linear model for WIP relationship
WIP_lm <- lm(log(CARBON) ~ WIP, data = all);summary(WIP_lm)

#linear mixed model with GEO as random effect

lmer_mod <- lmer((CARBON) ~  WIP + log10(PRECIP)  + MNDWI_sum +  (1+WIP|GEO), data = all, REML = F);summary(lmer_mod)
lmer_mod_log <- lmer(sqrt(CARBON) ~ WIP + log10(PRECIP) +  MNDWI_sum + (1|GEO), data = all, REML = F);summary(lmer_mod_log)

ranef(lmer_mod)

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
ggplot() +
    geom_point(aes(y = predict(lmer_mod), x = all$CARBON, colour =  as.factor(all$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(lmer_mod), x = all$CARBON), method = "lm", se = FALSE) +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))

#Making an effects plot
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

############## Hoh Study Area Model ################

###### Doing LASSO regression to identify variables

lasso(hoh) # for hoh

hoh_mat <- cor(hoh[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                       "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                       "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                       "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                       "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(hoh_mat, method = "number", number.cex= 0.5)

# Taking the coefs from the LASSO results and correlation matrix

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

#check the predictions of the model vs the actual 
plot(predict(hoh_mod_log), log(hoh$CARBON), col = as.factor(hoh$GEO),  pch = 19)
legend("bottomright", legend = paste("Group",  1:3), col = 1:3, pch = 19, bty = "n")

ggplot() +
    geom_point(aes(y = predict(hoh_mod), x = hoh$CARBON, colour =  as.factor(hoh$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(hoh_mod), x = hoh$CARBON), method = "lm", se = FALSE) +
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
mas_mod <- lmer((CARBON) ~  WIP + MNDWI_sum + SLP + (1|GEO), data = mas, REML = F);summary(mas_mod)
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
r.sq(sqrt(mas$CARBON), fitted(mas_mod_log)) 

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
