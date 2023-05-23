library(lme4)
library(lmerTest)
library(terra)
library(sp)
library(tidyverse)
library(car)
library(ggplot2)
library(merTools)
library(glmnet)
library(stats)
library(ggcorrplot)
library(RColorBrewer)
library(cowplot)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions

#setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/')
#wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/' 

hoh_csv <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbon_Jlat_lithextract.csv")

#subset for easier viewing and access
hoh_dat <- as_tibble(hoh_csv) %>% dplyr::select(sample_name, CHN_Cstock_Mg_ha, CHN_1m_Cstock_Mg_ha, 
                                                CHN_30m_Cstock_Mg_ha, CHN_90m_Cstock_Mg_ha, CHN_120m_Cstock_Mg_ha,
                                                LITHOLOGY, WIPv8, jlat, jlon ) %>%
    mutate_if(is.character, as.factor) %>%
    stats::setNames(c("NAME", "CARBON_FULL", "CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM",
                      "LITHOL", "WIP", "jlat", "jlon")) %>% 
    as.data.frame()
str(hoh_dat)

# Quick R^2 function
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}

#### model section ####
shapiro.test(sqrt(hoh_dat$CARBON_1M) ) #normality check 

mod <-  lmer(sqrt(CARBON_1M) ~ WIP +(1|LITHOL), data = hoh_dat)

plot(cooks.distance(mod)) # influential points, but I NEED this data

summary(mod)
anova(mod)
tab_model(mod)
plot(mod)
qqPlot(resid(mod))


r.sq(sqrt(hoh_dat$CARBON_1M), fitted(mod))


#### Prediction to map ####
GEOm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
WIPm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")

rs <- c(WIPm , GEOm)
names(rs) <- c("WIP", "LITHOL")
Pred <- terra::predict(rs, mod, allow.new.levels = T)
Pred_nl <- (Pred**2)
plot(Pred_nl)
writeRaster(Pred_nl, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16.tif", overwrite = T  )
predicted <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16.tif")
plot(predicted)

#### Model fit ####


set.seed(7)
PI <- predictInterval(
    mod,
    hoh_dat,
    which = c("full"),
    level = 0.95,
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

PI <- PI |> mutate(fit_t = fit**2,
                   upr_t = upr**2,
                   lwr_t = lwr**2,
                   act = hoh_dat$CARBON_1M)
PI

rootMSE <- mean(abs(PI$fit - PI$act))

ggplot(aes(x=sqrt(hoh_dat$CARBON_1M), y=fit, ymin=lwr, ymax=upr), dat = PI) +
    geom_point() +
    geom_linerange() +
    geom_smooth(aes(y = fitted(mod), x = sqrt(hoh_dat$CARBON_1M)), method = "lm", se = F) + 
    labs(x="SOC MgC ha-1", y="Prediction w/ 95% PI") + theme_bw()


kFoldCV <- function(K = 6, reps = 10, randSeed = 11){
    # Make sure nrow / K is an integer
    stopifnot("Rows not divisible by K" = (nrow(hoh_dat) %% K) == 0)
    set.seed(randSeed)
    kFoldMSE <- 0
    
    for(i in 1:reps){
        rowsLeft <- seq(1, nrow(hoh_dat)) # a sequence of row numbers 1 to 36
        kFoldMSE_temp <- 0 # A zero placeholder for MSE
        for(j in 1:K){
            # Get indices for the fold by sampling the vector of 1-36 with the size of the integer from 'floor'
            rowsTest  <- sample(x        = rowsLeft, #the 1-36 sequence
                                size     = floor(nrow(hoh_dat) / K), #takes single numeric argument (x) and returns vector containing the largest integers not greater than (x) 
                                replace  = FALSE) # don't replace the sample taken from the sequence
            # Remove those already chosen for next loop
            rowsLeft <- rowsLeft[! rowsLeft %in% rowsTest] # which row labels are left 
            
            # Split data
            dataTest  <- hoh_dat[rowsTest, ] # data that is the rows of the fold (smaller)
            dataTrain <- hoh_dat[-rowsTest,] # data that is outside the fold
            
            # Run model on K-1 folds (training data)
            mod_kFold <- lmer(formula = sqrt(CARBON_1M) ~ WIP +(1|LITHOL),
                              data    = dataTrain,
                              REML    = F)
            # Get predicted values on test dataset, as well as true values
            pred_kFold <- predict(object  = mod_kFold,
                                  newdata = dataTest)
            actual_kFold    <- sqrt(dataTest$CARBON_1M)
            # Calculate MSE for this fold
            kFoldMSE_temp <- kFoldMSE_temp + sum((pred_kFold - actual_kFold)^2)
        }
        # Add MSE for the given rep
        kFoldMSE <- kFoldMSE + kFoldMSE_temp
    }
    # Return average MSE across the reps
    estMSE <- kFoldMSE / reps
    names(estMSE) <- "Estimated MSE"
    return(estMSE)
}
# Test with K = N = 36 -  this is LOOCV.
### Note: Tends be lower than with K < N (lower bias, higher variance)
# Test with K = 6
kFoldCV(K         = 6,
        reps      = 10,
        randSeed  = 7)
# Test with K = N = 36 -  this is LOOCV.
### Note: Tends be lower than with K < N (lower bias, higher variance)
kFoldCV(K         = 36,
        reps      = 10,
        randSeed  = 7)


############# From the pierre roudier github ##############
library(parallel)
library(doParallel)
library(terra)
library(foreach)

k = 200
LITHOL <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
rs <- c(WIP, LITHOL)
names(rs) <- c("WIP", "LITHOL")
#predlist <- c(WIP)
#val_preds <- matrix(nrow = nrow(hoh_dat), ncol = k)
##### Non-parallel
# r <- rast(nrows=5, ncols=9, vals=1:45)
# x <- c(r, r*2)
# add(x) <- r*3
# x

clust <- makePSOCKcluster(4)
registerDoParallel(clust)

predList <- foreach(i = 1:k, .packages = c("terra", "tidyverse", "lme4")) %dopar% {
    hoh_dat <- as_tibble(hoh_csv) %>% dplyr::select(sample_name, CHN_Cstock_Mg_ha, CHN_1m_Cstock_Mg_ha, 
                                                  CHN_30m_Cstock_Mg_ha, CHN_90m_Cstock_Mg_ha, CHN_120m_Cstock_Mg_ha,
                                                  LITHOLOGY, WIPv8, jlat, jlon ) %>%
    mutate_if(is.character, as.factor) %>%
    stats::setNames(c("NAME", "CARBON_FULL", "CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM",
                      "LITHOL", "WIP", "jlat", "jlon")) %>% 
    as.data.frame()
    LITHOL <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
    WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
    rs <- c(WIP, LITHOL)
    names(rs) <- c("WIP", "LITHOL")
    # Generate a bootstrap resample of ph_cal rows
    bootIdx <- sample(x        = seq(1, nrow(hoh_dat)), #indexs from 1 to end of dataframe
                      size     = nrow(hoh_dat), #a non-negative integer giving the number of items to choose 
                      replace  = TRUE) #replacement
    bootData <- hoh_dat[bootIdx, ]  %>% #all 36 rows and all columns
        mutate(log_CARBON = sqrt(CARBON_1M)) %>% #adding log carbon values
        arrange(NAME) #arranging by sample name 
    # Fit a Hoh SOC model 
    modBoot <- lmer(formula = sqrt(CARBON_1M) ~ WIP + (1|LITHOL),
                    data    = bootData,
                    REML    = F)
    # Create cleaned bootstrapped dataset for this replication
    # bootData <- bootData %>%
    #     mutate( fittedFull = fitted(modBoot),
    #             pred     = predict( object  = modBoot,
    #                                 newdata = hoh_dat[bootIdx, ]),
    #             bootRep  = i) %>%
    #     dplyr::select(NAME, log_CARBON,fittedFull, pred, bootRep)
    
    # Predict onto raster
    pred <- predict(rs, modBoot, allow.new.levels = T, 
                    filename = paste0("bootstrapped/", i, "boot",'.tif'), overwrite = T)
    pred
    #add(predlist) <- pred
    #predstack <- rast(predlist)
    #val_preds[, i] <- predict(modBoot, newdata = rs)
    #predlist <- stack(predlist, predict(rs, modBoot, allow.new.levels = T))
}

predlist
stopCluster(clust)

c_mean <- terra::app(predlist, mean)
plot(10^c_mean)

c_interval <- terra::app(predlist, function(x){
    quantile(x, probs = c(0.05, 0.95), na.rm = T)
}, cores = 8, filename = "intervals.tif", overwrite = T)
#c_interval <- terra::quantile(predlist, probs = c(0.025, 0.975), na.rm = T, filename = "intervals.tif")
c_interval <- rast("intervals.tif")
plot(10**c_interval)