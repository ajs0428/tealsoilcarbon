library(stats)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(terra, raster, rgdal)
library(corrplot)
library(glmnet)
library(merTools)
library(randomForest)
library(caret)
library(cvms)
library(groupdata2)
library(mgcv)
library(gstat)
library(mapview)
library(kit)


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 


#Define dataframe
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS.csv")
#all <- read.csv("ANALYSIS/ALL_SOILC_GEE_6_22_22.csv")
#all <- read.csv("ANALYSIS/ALL_SOILC_PTS_EE_BIN.csv")
all_csv <- read.csv("ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")

# head(all)
 (names(all_csv))
# names(all) <- c("STUDY_AREA", "OID_", "FID_","lon","lat","AGE_LITHOL", "AGE_LITH_C", "B2","B3","B4",
#                 "B8","CARBON","DTM","DTW","EVI","GEO","GEOLOGIC_A", "GEOLOGIC_U", "GEO_CODE",   "LITHOLOGY",
#                 "NAMED_UNIT", "NDVI","NDWI","Precip","SCA","SHAPE_Area", "SHAPE_Leng", "SLP","TWI","WIP", "GEO"  )

#subset for easier viewing and access
all <- as_tibble(all_csv) %>% dplyr::select(STUDY_AREA, sample_nam,lat, lon,
                            Cstock_MgREV, Cstock_MgREV_1m, max_depth, DEM,DTW, PRECIP,SLP,TWI,
                            NDVI_s_0_win,MNDWI_s_0_win, EVI_s_0_win, b10_s_0_win,
                            NDVI_s_1_spr, MNDWI_s_1_spr, EVI_s_1_spr, b10_s_1_spr,
                            NDVI_s_2_sum,  MNDWI_s_2_sum, EVI_s_2_sum, b10_s_2_sum,
                            NDVI_s_3_aut,  MNDWI_s_3_aut, EVI_s_3_aut, b10_s_3_aut, 
                            GEO_8_6_22, WIP_INV,
                            b2_s_1_spr, b3_s_2_sum, b1_s_1_spr,  b5_s_2_sum, b5_s_3_aut, b8_s_2_sum) %>%
    stats::setNames(c("STUDY_AREA", "NAME","lat", "lon", 
               "CARBON", "CARBON_1M", "max_depth", "DEM","DTW","PRECIP","SLP","TWI",
               "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
               "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
               "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
               "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", 
               "GEO",   "WIP",
               "b2_s_1_spr", "b3_s_2_sum", "b1_s_1_spr",  "b5_s_2_sum", "b5_s_3_aut", "b8_s_2_sum"))
#all$MNDWI_sum_scale <- all$MNDWI_sum/sd(all$MNDWI_sum)
#all$b3_s_2_sum_scale <- all$b3_s_2_sum/sd(all$b3_s_2_sum)
names(all)
#make this a factor
all$GEO <- as.factor(all$GEO)

all <- all %>% mutate(wetrivup = ifelse(GEO == "Quaternary", "RIV", ifelse(WIP> 0.5, "WET", "UPL")))
#all$WIP2 <- (all$WIP)**2
#all$b8_s_2_sum <- all_csv$b8_s_2_sum # WHY?
#all$GEO <- as.factor(all$GEO)

# Bar plot for factors
hoh_fac <-subset(all, STUDY_AREA == "HOH")
hoh_fac <- hoh_fac %>% mutate(wetupbetriv = case_when(WIP <=0.1 ~ "UPLAND",
                                                      WIP >0.1 & WIP<0.5 ~ "WETFOR",
                                                      WIP>=0.5 & GEO != "Quaternary"~ "WETLAND",
                                                      WIP>=0.5 & GEO == "Quaternary" ~ "RIV"))
grouped <- hoh_fac %>% group_by(wetupbetriv)
summ_hoh <- summarise(grouped, mean=mean(CARBON), sd=sd(CARBON), count = n())
summ_hoh$se <- summ_hoh$sd/sqrt(summ_hoh$count)
summ_hoh$lwr <- summ_hoh$mean - summ_hoh$se
summ_hoh$upr <- summ_hoh$mean + summ_hoh$se

ggplot(summ_hoh) +
    geom_bar(aes(x = wetupbetriv, y = mean, fill = wetupbetriv), stat = "identity") +#geom_bar(stat = "summary", fun = 'mean') +
    geom_errorbar(aes(wetupbetriv, ymin = lwr, ymax = upr), width = 0.2) +
    xlab("") + ylab('Soil Carbon Stock (Mg/ha)') +
    scale_fill_manual(values = c("#65a0c4" ,"#DAB785", "#95b99d", "#05aa74" )) +
    scale_y_continuous(expand = c(0,0)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(),
          text = element_text(size = 30))

# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
# LASSO Function 
lasso <- function(df){
    set.seed(1)
    x <- data.matrix(subset(df, select = -c(WIP,X,NUM,OID_,STUDY_AREA,
                             sample_nam,sample_name, Cstock_g_c, Cstock_g_REV, Cstock_Mg_, Cstock_MgREV, Cstock_MgREV_1m,          
                             GEO_8_6_22, GEO_7_22_22, OLD_GEO, OLD_GEO_CODE,
                             OLD_GEO_CLASS, OLD_GEO2, AGE_LITH2)))#data.matrix(df[, c(WIP,DEM,DTW, GEO,PRECIP,SLP,TWI,
                            #"NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                            #"NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                            #"NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                            #"NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])
    y <- df$Cstock_MgREV
    
    cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
    best_lambda <- cv_model$lambda.min
    plot(cv_model)
    print(paste("best lambda:", best_lambda))
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    print(coef(best_model))
}

######Correlation plot
all_mat <- cor(all[, c("CARBON", "CARBON_1M", 'max_depth', "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                                   "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                                   "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                                   "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                                   "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(all_mat, method = "number", number.cex= 15/ncol(all))


masInv <- head(arrange(all, desc(CARBON)), 3)
corrplot::corrplot(cor(masInv[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                               "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                               "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                               "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                               "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")]))

#### Subset out the high C data ####

high_csv <- subset(all_csv, all$CARBON_1M > 400)

lasso(high_csv)

high <- subset(all, all$CARBON_1M > 400)
#highWIP <- subset(all, all$WIP > 0.5)

high_lm <- lm(CARBON_1M ~ WIP + lon, data = high);summary(high_lm)

high_mod <- lmer((CARBON_1M) ~  WIP + b3_s_2_sum +(1|GEO), data = high, REML = F);summary(high_mod)
anova(high_mod) 
r.sq(high$CARBON_1M, fitted(high_mod)) 

ggplot() +
    geom_point(aes(y = predict(high_mod), x = (high$CARBON_1M), colour =  as.factor(high$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(high_mod), x = (high$CARBON_1M)), method = "lm", se = F) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    #labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))
########### All data linear models ################################################################################



#LASSO for all data
lass0 <- (lasso(all_csv))
summ <- summary(lass0)
lass0_df <- data.frame(Var = rownames(lass0)[summ$i],
           LassWeight = summ$x)
lass0_df <- lass0_df[order(-lass0_df$LassWeight),]


#Linear model for WIP relationship
WIP_lm <- lm(log(CARBON) ~ max_depth, data = all);summary(WIP_lm)

#linear mixed model with the binary class as random effect
#The regular GEO layer is actually way better than the binary class. 
#But for the individual study areas the binary may be better

lmer_mod <- lmer((CARBON) ~  (WIP) + MNDWI_sum + max_depth + (1|STUDY_AREA:GEO), data = all, REML = F);summary(lmer_mod)
lmer_mod_log <- lmer(log10(CARBON) ~  (WIP) + MNDWI_sum + (1|STUDY_AREA:GEO), data = all, REML = F);summary(lmer_mod_log)


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
r.sq(log10(all$CARBON), fitted(lmer_mod_log)) 

#checking other factors with carbon  
# plot(predict(lmer_mod), all$CARBON, col = as.factor(all$STUDY_AREA),  pch = 19)
# #plot(predict(lmer_mod), all$CARBON, col = as.factor(all$GEOLOGIC_A),  pch = 19)
# legend("bottomright", legend = paste("Group", 1:3), col = 1:6, pch = 19, bty = "n")

ggplot() +
    geom_point(aes(y = predict(lmer_mod), x = (all$CARBON), colour =  as.factor(all$WIP >0.5), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(lmer_mod), x = (all$CARBON)), method = "lm", se = F) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    #labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))

all_names <- names(fixef(lmer_mod_log))
all_vals <- c( unname(fixef(lmer_mod_log)) )
all_plot_dat <- data.frame(all_names, all_vals)


ggplot(all_plot_dat, aes(all_names, all_vals)) +
    geom_col() + xlab("Fixed Coefficients") + ylab('Coefficient Value (Mg/ha)') +
    theme(text = element_text(size = 20))


#### All model 10 fold cross validation ####
lmer_form <- c("CARBON ~  WIP + MNDWI_sum + (1|STUDY_AREA:GEO)")

set.seed(7)
fold_dat <- fold(data = all, k = 10) %>% 
    arrange(.folds)
CV1 <- cross_validate(
    data = fold_dat,
    formulas = lmer_form,
    family = "gaussian",
    REML = F
)
cv_preds <- CV1$Predictions[[1]] #as.data.frame(CV1$Predictions)
cv_preds %>% 
    group_by(Fold) %>% 
    evaluate(
        target_col = "Target",
        prediction_cols = "Prediction",
        type = "gaussian"
    )
plot(cv_preds$Target, cv_preds$Prediction)
r.sq(cv_preds$Target, cv_preds$Prediction) 

pred_cv <- cvms::predict_functions("lmer")

########### Separate Study Area data linear models ################################################################################


##### Dataframe setup - skip GEO, already in dataframe #############
hoh <- subset(all, STUDY_AREA == "HOH")
hoh_csv <- read.csv("ANALYSIS/HOH_SOILC_8_16_22_TH.csv")#subset(all_csv, STUDY_AREA == "HOH")
hoh<- as_tibble(hoh_csv) %>% dplyr::select(STUDY_AREA, sample_nam,lat, lon,
                                     Cstock_MgREV, Cstock_MgREV_1m, DEM,DTW, PRECIP,SLP,TWI,
                                     NDVI_s_0_win,MNDWI_s_0_win, EVI_s_0_win, b10_s_0_win,
                                     NDVI_s_1_spr, MNDWI_s_1_spr, EVI_s_1_spr, b10_s_1_spr,
                                     NDVI_s_2_sum,  MNDWI_s_2_sum, EVI_s_2_sum, b10_s_2_sum,
                                     NDVI_s_3_aut,  MNDWI_s_3_aut, EVI_s_3_aut, b10_s_3_aut, 
                                     GEO_8_6_22, WIP_INV, treeheight.Hoh_TreeHeight) %>%
    stats::setNames(c("STUDY_AREA", "NAME","lat", "lon", 
                      "CARBON", "CARBON_1M", "DEM","DTW","PRECIP","SLP","TWI",
                      "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                      "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                      "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                      "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", 
                      "GEO",   "WIP","TREE_H"))

col <- subset(all, STUDY_AREA == "COL")
mas <- subset(all, STUDY_AREA == "MAS")




############## Hoh Study Area Model #################



###### Doing LASSO regression to identify variables
hoh_csv_wipsub <- subset(hoh_csv, hoh_csv$WIP_INV > 0.5)
lasso(hoh_csv) # for hoh
hoh_wipsub <- subset(hoh, hoh$WIP>0.)

hoh_mat <- cor(hoh_wipsub[, c("CARBON", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                       "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                       "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                       "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                       "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(hoh_mat, method = "number", number.cex= 0.5)


# Taking the coefs from the LASSO results 
#removed SCA since it improved the AIC/BIC
hoh_mod <- lmer((CARBON) ~  WIP + MNDWI_sum + (1|GEO), data = hoh, REML = F);summary(hoh_mod) #nonlog model
hoh_mod_log <- lmer(sqrt(CARBON) ~   WIP +   MNDWI_sum + (1|GEO), data = hoh, REML = F);summary(hoh_mod_log)
####### Hoh evaluate the log model
anova(hoh_mod) #check on factors, none significant, not sure how to interpret
r.sq((hoh$CARBON), fitted(hoh_mod))  

anova(hoh_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(hoh$CARBON), fitted(hoh_mod_log)) 

#check the predictions of the model vs the actual 
plot(predict(hoh_mod), hoh$CARBON, col = as.factor(hoh$GEO),  pch = 19)
abline(lm(predict(hoh_mod) ~ hoh$CARBON))
#legend("bottomright", legend = paste("Group",  1:2), col = 1:2, pch = 19, bty = "n")

ggplot() +
    geom_point(aes(y = predict(hoh_mod), x = hoh$CARBON, colour =  as.factor(hoh$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(hoh_mod), x = hoh$CARBON), method = "lm", se = FALSE) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
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
mas_mod <- lmer((CARBON) ~  WIP + MNDWI_sum + SLP + TWI + (1|GEO), data = mas, REML = F);summary(mas_mod)
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
col_mod <- lmer((CARBON) ~  WIP  + log10(PRECIP) + (1|GEO), data = col, REML = F);summary(col_mod)
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

PRECIP <-rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_precip_rsmpl.tif")
#DTW <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/MainChannelDTW.tif')
#SLP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_slp_rsmpl.tif")
#SCA <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_sca_rsmpl.tif")
#WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_fill.tif")
WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
STUDY <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_STUDY.tif")
#rasterize(STUDY, WIP, field = "value", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_STUDY.tif", overwrite = T)
plot(STUDY)
GEO <- rast( "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_R_GEO_CROP_AGG.tif")
#NDVI_SPR <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_spec_5yr_sea1.tif")[['SR_B5']] #seasons 0 = win, 1 = spr, 2 = sum, 3 = fall
#NDVI_SPR_R <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_NDVI_SPR_R.tif")#terra::resample(NDVI_SPR, WIP, method = "bilinear", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_NDVI_SPR_R.tif")
MNDWI_SUM_R <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")#resample(MNDWI_SUM, WIP, method = "bilinear")
#MNDWI_SUM_R_S <- scale(MNDWI_SUM_R, center = F)
#B3_SUM <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_spec_5yr_sea2.tif")[["SR_B3"]]
B3_SUM_R <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_B3_SUM_R.tif")#terra::resample(B3_SUM, WIP, method = "bilinear", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_B3_SUM_R.tif")
#B3_SUM_R_S <- scale(B3_SUM_R, center = F)
#TWI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/TWI_resample_z10.tif')
#NDVI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_NDVI.tif')
#TEMPK <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/')
#EVI <- rast("")

#Check extents
#TWI is weird, might have to address in ArcGIS 
#For now just match extent with nearest neighbor resampling 
ext(WIP) == ext(STUDY)
ext(WIP) == ext(PRECIP)

terra::plot(B3_SUM_R, main = "1-WIP2")
#writeRaster(WIP, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH_WIP_v8_invert.tif")


#write rasters if needed

#terra::writeRaster(MNDWI_SUM_R, "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")

#stack rasters from model see below
            #lmer_mod <- lmer((CARBON) ~  WIP +  log10(PRECIP) + NDVI_spr + MNDWI_sum + (1|GEO)
rs <- c(WIP, PRECIP,  MNDWI_SUM_R, STUDY, GEO)


#Change names to match the dataframe extraction I think...
names(rs) <- c("WIP", "PRECIP",  "MNDWI_sum", "STUDY_AREA","GEO")
#names(rs) <- c("SCA", "WIP", "TWI",  "GEO")

#looking at coefficients for fixed and random effects
betas <- fixef(lmer_mod) #fixed
rands <- ranef(lmer_mod) #random

#Predicting the model onto a new raster. 
# the old way of using just the coefs doesn't work because I don't know 
# how to apply a random effect
#Pred <- (DTW*betas[2]) + (TWI_rspl*betas[3] + (WIP*betas[4]) + betas[4])
Pred <- terra::predict(rs, lmer_mod, se.fit = TRUE, allow.new.levels = TRUE, filename = "TEST_CONF_HOH_CARBON_8_27_22_RanSLP.tif", overwrite = TRUE)
plot(Pred)

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
#SLP <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_TWI_SLP.tif')
#SCA <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_TWI_SCA.tif')
WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/Mashel_2022_V01/Mashel_2022_V01.tif")
B3_SUM <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mas_spec_5yr_sea2.tif')[["SR_B5"]]
B3_SUM_R <-  rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_B3_SUM_R-Anthony’s MacBook Pro.tif")#resample(B3_SUM, WIP, method = "bilinear", filename = 'SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_B3_SUM_R.tif', overwrite = T)
MNDWI_SUM_R <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM_R.tif')#[["MNDWI"]]
#MNDWI_SUM_R <- resample(MNDWI_SUM, WIP, method = "bilinear", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM_R.tif", overwrite = T)
#TWI <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_TWI.tif')
#PRECIP <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_precip_proj_clip.tif')
PRECIP_R <- rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_PRECIP_R.tif')#resample(PRECIP, WIP, method = "bilinear", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_PRECIP_R.tif", overwrite = T)
#GEO_N <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS8_6_22R_GEO_CROP.tif")
#GEO <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS8_6_22R_GEO_CROP.tif")
GEO_R <-rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_GEO_R.tif")#rast('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_R_GEO_CROP_AGG.tif')
plot(GEO_R)

ext(WIP) == ext(PRECIP_R)
ext(WIP) == ext(MNDWI_SUM_R)
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
rs <- c(WIP, PRECIP_R,  MNDWI_SUM_R,  GEO_R)

#Change names to match the dataframe extraction I think...
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("WIP", "PRECIP",  "MNDWI_sum",  "GEO")

Pred = terra::predict(rs, lmer_mod, allow.new.levels = TRUE, filename = "MAS_CARBON_8_10_22_RanSLP_1M.tif", overwrite = TRUE)
plot(Pred)

#write it out and save 
#writeRaster(Pred, 'SPATIAL LAYERS/PREDICT_SOIL_CARBON/MAS_CARBON_7_11_22.tif',  overwrite = T)



###### COLVILLE Carbon Predict ##############################################

#raster layers of variables
#DTW  + WIP + NDVI + Precip  + (1|GEO)
#DTW <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_DTW_rspl.tif")
#SLP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_SLP_rspl.tif")
#SCA <- raster('A:/WA_tealcarbon/MASHEL_CARBON/Mashel_spatial_layes/mashel_TWI_SCA.tif')
WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")
#B3_SUM <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_spec_5yr_sea2.tif")[["SR_B3"]]
#col_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_nwi_poly.shp")
#B3_SUM_RPJ <- terra::project(B3_SUM, crs(col_poly))
#B3_SUM_C <- crop(B3_SUM_RPJ, col_poly, mask = T)
B3_SUM_R <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_B3_SUM_R.tif")#rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_B3_SUM_R.tif")
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
ext(WIP) == ext(B3_SUM_R)
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

Pred = terra::predict(rs, lmer_mod, allow.new.levels = TRUE, filename = "COL_CARBON_8_10_22_ASM_1M.tif", overwrite =T)
#plot(Pred, col=rev( rainbow( 99, start=0,end=1 ) ),zlim=c(-50,0)  )
plot(Pred, range = c(0, 500))





#################### Random Forest ############################

library(randomForest)
library(datasets)
library(caret)

#subset for easier viewing and access
all_csv <- read.csv("ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")

rf_data <- as.tibble(all_csv) %>% dplyr::select(-NUM, -OID_, -sample_nam, -sample_name, -WIP,
                                            -Cstock_g_c, -Cstock_g_REV, -Cstock_Mg_,
                                            -OLD_GEO, -OLD_GEO_CODE, -OLD_GEO_CLASS,
                                            -OLD_GEO2, -AGE_LITH2, -STUDY_AREA, -GEO_7_22_22, -Cstock_MgREV_1m) #%>%
rf_data2 <- as.tibble(all_csv) %>% dplyr::select(Cstock_MgREV, WIP_INV,    NDVI_s_0_win,  b3_s_2_sum,    lon,   
                                            b2_s_1_spr,  b9_s_0_win, SLP, b3_s_3_aut,  b1_s_1_spr,
                                            MNDWI_s_2_sum, b16_s_0_win, b3_s_1_spr, b4_s_0_win, b12_s_3_aut) 

set.seed(22)
ind <- sample(2, nrow(rf_data), replace = TRUE, prob = c(0.7, 0.3))
train <- rf_data[ind == 1, ]
test <- rf_data[ind == 2, ]
rf <- randomForest(Cstock_MgREV~., data = rf_data2, importance=TRUE, type = "regression", rsq = T, ntree =1000)
print(rf)

p1 <- predict(rf, newdata = test)
varImpPlot(rf)
vf <- as.data.frame(varImp(rf))
rownames(vf)[order(vf$Overall, decreasing=TRUE)][1:15]


rf$rsq[length(rf$rsq)]
rf$predicted

plot(rf$predicted, rf_data$Cstock_MgREV)
abline(1,1)
abline(lm(rf_data$Cstock_MgREV ~ rf$predicted,))

getTree(rf, k = 55)



############# Boosted regression tree #################
library(MASS)
library(gbm)
library(dismo)

all_brt <- all %>% dplyr::select(-NAME, -CARBON_1M, -lon, -lat)
all_brt <- as.data.frame(all_brt)
all_brt$GEO <- as.factor(all_brt$GEO)
all_brt$STUDY_AREA <- as.factor(all_brt$STUDY_AREA)
# train$GEO_8_6_22 <- as.factor(train$GEO_8_6_22)
# test$GEO_8_6_22 <- as.factor(test$GEO_8_6_22)
brt_step <- gbm.step(data = all_brt, gbm.x = c(1,3:30), gbm.y = c(2),
                     family = "gaussian", learning.rate = 0.005, 
                     tree.complexity = 5, bag.fraction = 0.75, n.trees = 50,
                     tolerance.method = "fixed")

summary(brt_step)
plot(brt_step, i = "WIP")
plot(brt_step, i = "SLP")

brt_simp <- gbm.simplify(brt_step)
summary(brt_simp)

brt_step_simp <- gbm.step(all_brt, gbm.x = brt_simp$pred.list[[17]], gbm.y = 2,
                          tree.complexity = 5, learning.rate = 0.005, 
                          family = "gaussian", tolerance.method = "fixed")
par(mar = c(1,1,1,1))
gbm.plot(brt_step_simp, n.plots = 15, plot.layout = c(5,3), write.title = T,
         lwd = 1)
plot.layout = c(1,1)
gbm.perspec(brt_step_simp, 2, 1)

preds_brt <- predict.gbm(brt_step_simp, all_brt, n.trees = brt_step_simp$n.trees,
            type = "response")
r.sq(all_brt$CARBON,preds_brt)

brt_step_simp$weights

#Here is the actual gradient boosting tree to use after determining from step
step_dat <- as.data.frame((all_brt[brt_simp$pred.list[[17]]]))

brt_gbm <- gbm(all_brt$CARBON ~ ., data = step_dat, distribution = "gaussian", 
               bag.fraction = 0.75, cv.folds = 10, n.trees = 500)

gbm.perf(brt_gbm)
summary(brt_gbm$cv.error)
r.sq(brt_gbm$fit, all_brt$CARBON)

# n.trees = seq(from = 100, to = 10000, by = 100)
# predmat = predict(brt, newdata = all_brt, n.trees = n.trees)
# dim(predmat)

# test.err = double(27)
# boost.err = with(all_brt, apply( (predmat - CARBON)^2, 2, mean) )
# plot(n.trees, boost.err, pch = 23, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")
# abline(h = min(test.err), col = "red")
# 
# brt_pred <- predict(brt, all_brt, n.trees = 2000)
# plot(brt_pred, (all_brt$CARBON))
# r.sq((all_brt$CARBON), brt_pred)

ggplot() +
    geom_point(aes(y = brt_pred, x = (all_brt$CARBON), colour =  as.factor(all_brt$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = brt_pred, x = (all_brt$CARBON)), method = "lm", se = F) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    #labs(colour = "Random Effect") +
    theme(legend.position = 'none', text = element_text(size = 20))



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

dat <-dat[c("CARBON", "NDVI_sum","MNDWI_sum", "EVI_sum", "PRECIP","SLP","TWI","WIP")]

y <- (dat$CARBON) #TO log or not to log...... what do we do about heteroskedasity in residuals?
X <- as.matrix(cbind(1,dat[,2:8])) 
n <- length(y)
u.idx <- as.numeric(as.factor(geo_dat$GEO))

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
points(y.fitted[u.idx == 4],res[u.idx == 4], col = "orange", pch = 19)
points(y.fitted[u.idx == 5],res[u.idx == 5], col = "black", pch = 19)
points(y.fitted[u.idx == 6],res[u.idx == 6], col = "purple", pch = 19)
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
