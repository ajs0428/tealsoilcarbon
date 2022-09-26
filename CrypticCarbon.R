library(lme4)
library(lmerTest)
library(terra)
library(dplyr)
library(ggplot2)
library(merTools)
library(glmnet)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 

all <- read.csv("SOIL CARBON/ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")
hoh <- subset(all, all$STUDY_AREA == "HOH")


#subset for easier viewing and access
hoh_dat <- as_tibble(hoh) %>% dplyr::select(STUDY_AREA, sample_nam,lat, lon,
                                            Cstock_MgREV, Cstock_MgREV_1m, max_depth, DEM,DTW, PRECIP,SLP,TWI,
                                            NDVI_s_0_win,MNDWI_s_0_win, EVI_s_0_win, b10_s_0_win,
                                            NDVI_s_1_spr, MNDWI_s_1_spr, EVI_s_1_spr, b10_s_1_spr,
                                            NDVI_s_2_sum,  MNDWI_s_2_sum, EVI_s_2_sum, b10_s_2_sum,
                                            NDVI_s_3_aut,  MNDWI_s_3_aut, EVI_s_3_aut, b10_s_3_aut, 
                                            GEO_8_6_22, WIP_INV,
                                            b2_s_1_spr, b3_s_2_sum, b1_s_1_spr,  b5_s_2_sum, b5_s_3_aut, b8_s_2_sum) %>%
    mutate_if(is.character, as.factor) %>%
    stats::setNames(c("STUDY_AREA", "NAME","lat", "lon", 
                      "CARBON", "CARBON_1M", "max_depth", "DEM","DTW","PRECIP","SLP","TWI",
                      "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                      "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                      "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                      "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", 
                      "GEO",   "WIP",
                      "b2_s_1_spr", "b3_s_2_sum", "b1_s_1_spr",  "b5_s_2_sum", "b5_s_3_aut", "b8_s_2_sum")) %>% 
    as.data.frame()
str(hoh_dat)

# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
# LASSO Function 
lasso <- function(df){
    set.seed(1)
    x <- data.matrix(subset(df, select = -c(CARBON, CARBON_1M, max_depth, GEO)))#data.matrix(df[, c(WIP,DEM,DTW, GEO,PRECIP,SLP,TWI,
    #"NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
    #"NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
    #"NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
    #"NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])
    y <- df$CARBON
    
    cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
    best_lambda <- cv_model$lambda.min
    plot(cv_model)
    print(paste("best lambda:", best_lambda))
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    print(coef(best_model))
    print(summary(best_model))
    # summ <- summary(cv_model)
    # lass0_df <- data.frame(Var = rownames(cv_model)[summ$i],
    #                        LassWeight = summ$x)
    # lass0_df <- lass0_df[order(-lass0_df$LassWeight),]
}


############## Hoh Study Area Model #################

###### Doing LASSO regression to identify variables
#lasshoh <- lasso(hoh_dat) # for hoh

hoh_mat <- cor(hoh_dat[, c("CARBON", "CARBON_1M", "WIP","DEM","DTW", "PRECIP","SLP","TWI",
                              "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                              "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                              "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                              "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])

corrplot::corrplot(hoh_mat, method = "number", number.cex= 0.5)


# Taking the coefs from the LASSO results?

#Big model for backwards step
big_mod <- lmer(CARBON ~  WIP+DEM+DTW+ PRECIP+SLP+TWI+
                NDVI_win+MNDWI_win+ EVI_win+ TempK_win+
                NDVI_spr+ MNDWI_spr+ EVI_spr+ TempK_spr+
                NDVI_sum+  MNDWI_sum+ EVI_sum+ TempK_sum+
                NDVI_aut+  MNDWI_aut+ EVI_aut+ TempK_aut + (1|GEO) +
                b2_s_1_spr + b3_s_2_sum + b1_s_1_spr + b5_s_2_sum + b5_s_3_aut +
                 b8_s_2_sum, data = hoh_dat, REML = F)
(step_res <- step(big_mod))
final <- get_model(step_res)
summary(final)
summary(lmer(formula = CARBON ~ PRECIP + TempK_win + NDVI_spr + MNDWI_spr + 
       TempK_spr + NDVI_sum + NDVI_aut + EVI_aut + b2_s_1_spr + 
       b3_s_2_sum + b5_s_3_aut + b8_s_2_sum + (1|GEO), data = hoh_dat, REML = F))
#

hoh_mod <- lmer((CARBON) ~  WIP + MNDWI_sum +(1|GEO), data = hoh_dat, REML = F);summary(hoh_mod) #nonlog model
hoh_mod_log <- lmer(sqrt(CARBON) ~   WIP +   MNDWI_sum + (1|GEO), data = hoh_dat, REML = F);summary(hoh_mod_log)
####### Hoh evaluate the log model
anova(hoh_mod) #check on factors, none significant, not sure how to interpret
r.sq((hoh_dat$CARBON), fitted(final))  

anova(hoh_mod_log) #check on factors, none significant, not sure how to interpret
r.sq(sqrt(hoh_dat$CARBON), fitted(hoh_mod_log)) 

ggplot() +
    geom_point(aes(y = predict(final), x = hoh_dat$CARBON, colour =  as.factor(hoh_dat$GEO), size = 3)) +
    xlab("Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = predict(hoh_mod), x = hoh_dat$CARBON), method = "lm", se = FALSE) +
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

#### Uncertainty ####
set.seed(7)
PI <- predictInterval(
    hoh_mod,
    hoh_dat,
    which = c("all"),
    level = 0.8,
    n.sims = 1000,
    stat = c("median"),
    type = c("linear.prediction"),
    include.resid.var = TRUE,
    returnSims = FALSE,
    seed = 7,
    .parallel = FALSE,
    .paropts = NULL,
    fix.intercept.variance = FALSE,
    ignore.fixed.terms = NULL
)

ggplot(aes(x=1:30, y=fit, ymin=lwr, ymax=upr), data=PI[1:30,]) +
    geom_point() +
    geom_linerange() +
    labs(x="Index", y="Prediction w/ 95% PI") + theme_bw()

predictInterval(hoh_mod, hoh_dat[1,], include.resid.var = 0)


test <-confint(hoh_mod, oldNames= F)

lmerTest::get_coefmat()

#### Map making ####
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
GEO <- rast( "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_R_GEO_CROP_AGG.tif")
MNDWI_sum <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")

#masked layers
WIPm <- mask(WIP, (MNDWI_sum>-0.30),  maskvalues = 1, updatevalue = NA)
GEOm <- mask(GEO, (MNDWI_sum>-0.30),  maskvalues = 1, updatevalue = NA)
MNDWI_sum_m <- mask(MNDWI_sum, (MNDWI_sum>-0.30),  maskvalues = 1, updatevalue = NA)


rs <- c(WIPm, MNDWI_sum_m, GEOm)
names(rs) <- c("WIP", "MNDWI_sum", "GEO")
Pred <- rast("SOIL CARBON/CrypticCarbon-AnthonyMBP.tif")#terra::predict(rs, hoh_mod, se.fit = TRUE, allow.new.levels = T)
#writeRaster(Pred, filename = "SOIL CARBON/CrypticCarbon.tif", overwrite = T)
plot(WIPm)

# PI_big <- predictInterval(
#     hoh_mod,
#     rs,
#     which = c("all"),
#     level = 0.8,
#     n.sims = 1000,
#     stat = c("median"),
#     type = c("linear.prediction"),
#     include.resid.var = TRUE,
#     returnSims = FALSE,
#     seed = 7,
#     .parallel = TRUE,
#     .paropts = NULL,
#     fix.intercept.variance = FALSE,
#     ignore.fixed.terms = NULL
# )

#### Masking for Stats ####

