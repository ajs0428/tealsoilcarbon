library(lme4)
library(lmerTest)
library(terra)
library(sp)
library(dplyr)
library(ggplot2)
library(merTools)
library(glmnet)
library(stats)
library(MASS)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')

hoh_csv <- read.csv("SOIL CARBON/ANALYSIS/hoh_CHN_1m30cm_Stocks_WIPupd.csv")

################################################################################
################################### PRZ ADDED ##################################
################################################################################
# LOADING TIDYVERSE PACKAGE
library(tidyverse)
#subset for easier viewing and access
hoh_dat <- as_tibble(hoh_csv) %>% dplyr::select(sample_name,
                                                CHN_Cstock_Mg_ha, CHN_1m_Cstock_Mg_ha, CHN_30m_Cstock_Mg_ha,
                                                WIP_NEW, DEM,DTW, PRECIP,SLP,TWI, DTW_RIV,
                                                NDVI_s_0_win,MNDWI_s_0_win, EVI_s_0_win, b10_s_0_win,
                                                NDVI_s_1_spr, MNDWI_s_1_spr, EVI_s_1_spr, b10_s_1_spr,
                                                NDVI_s_2_sum,  MNDWI_s_2_sum, EVI_s_2_sum, b10_s_2_sum,
                                                NDVI_s_3_aut,  MNDWI_s_3_aut, EVI_s_3_aut, b10_s_3_aut, 
                                                GEO_250, b2_s_1_spr, b3_s_2_sum, b1_s_1_spr,  
                                                b5_s_2_sum, b5_s_3_aut, b8_s_2_sum, DTW_RIV) %>%
    #mutate(GEO_8_6_22 =  replace(GEO_8_6_22, GEO_8_6_22 == "Quaternary", "Holocene-Quaternary-Present")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_at(c("DTW_RIV"), as.factor) %>%
    stats::setNames(c("NAME", "CARBON_FULL", "CARBON_1M", "CARBON_30CM",
                      "WIP", "DEM","DTW","PRECIP","SLP","TWI", "DTW_RIV",
                      "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                      "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                      "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                      "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", "GEO",
                      "b2_s_1_spr", "b3_s_2_sum", "b1_s_1_spr",  "b5_s_2_sum", 
                      "b5_s_3_aut", "b8_s_2_sum", "DTW_RIV")) %>% 
    as.data.frame()
str(hoh_dat)

# GEOLOGY-specific datasets and models (just doing GLM)
hoh_Qo <- hoh_dat %>% filter(GEO == "Quat_old_clastic")
hoh_Qn <- hoh_dat %>% filter(GEO == "Quat_new")
hoh_Qa <- hoh_dat %>% filter(GEO == "Quat_old_alluv")
hoh_ME <- hoh_dat %>% filter(GEO == "MioEo")

# HISTOGRAMS OF NUMERIC VARIABLES BY GEOLOGY
plotCARBON <- ggplot( data    = hoh_dat,
                      mapping = aes(x = CARBON_1M, fill = GEO)) +
  geom_histogram(position = "dodge", bins = 36, binwidth = 50) +
  labs( title         = "Histogram of CARBON by GEOLOGY") +
  theme(plot.title    = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(0, 800)

plotWETLAND <- ggplot(data    = hoh_dat,
                      mapping = aes(x = WIP, fill = GEO)) +
  geom_histogram(position = "dodge", bins = 36) +
  labs( title         = "Histogram of WETLAND by GEOLOGY") +
  theme(plot.title    = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(0, 1)

plotMNDWI <- ggplot(data    = hoh_dat,
                    mapping = aes(x = MNDWI_sum, fill = GEO)) +
  geom_histogram(position = "dodge", bins = 36, binwidth = 50) +
  labs( title         = "Histogram of MNDWI by GEOLOGY") +
  theme(plot.title    = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(-0.75, -0.25)

plotEVI <- ggplot(data    = hoh_dat,
                    mapping = aes(x = EVI_sum, fill = GEO)) +
    geom_histogram(position = "dodge", bins = 36, binwidth = 50) +
    labs( title         = "Histogram of MNDWI by GEOLOGY") +
    theme(plot.title    = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)) +
    xlim(0, 0.75)

plotCARBON
plotWETLAND
plotMNDWI
plotEVI

################################################################################
####################### K-fold cross-validation function #######################
################################################################################
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
      mod_kFold <- lmer(formula = log10(CARBON_1M) ~ WIP +(1|GEO),
                        data    = dataTrain,
                        REML    = F)
      # Get predicted values on test dataset, as well as true values
      pred_kFold <- predict(object  = mod_kFold,
                            newdata = dataTest)
      actual_kFold    <- log10(dataTest$CARBON_1M)
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

# Test with K = 6
kFoldCV(K         = 6,
        reps      = 10,
        randSeed  = 11)
# Test with K = N = 36 -  this is LOOCV.
### Note: Tends be lower than with K < N (lower bias, higher variance)
kFoldCV(K         = 36,
        reps      = 10,
        randSeed  = 11)

################################################################################
################# Bootstrapped predicted intervals function ####################
################################################################################
bootPredIntervals <- function(reps      = 1000,   randSeed = 11,
                              pctLower  = 0.025,  pctUpper = 0.975){
  # Regular LMER model 
  hoh_mod <- lmer(formula = log10(CARBON_1M) ~ WIP + (1|GEO),
                  data    = hoh_dat,
                  REML    = F);summary(hoh_mod)
  
  # Set random seed
  set.seed(randSeed)
  # Perform bootstrap replications 
  for(i in 1:reps){
    # Get bootstrap resampling indices
    bootIdx   <- sample(x        = seq(1, nrow(hoh_dat)), #indexs from 1 to end of dataframe
                        size     = nrow(hoh_dat), #a non-negative integer giving the number of items to choose 
                        replace  = TRUE) #replacement
    # Get data for these rows
    bootData <- hoh_dat[bootIdx, ]  %>% #all 36 rows and all columns
      mutate(log_CARBON = log10(CARBON_1M)) %>% #adding log carbon values
      arrange(NAME) #arranging by sample name 
    # Run model on bootstrap sample
    modBoot <- lmer(formula = log10(CARBON_1M) ~ WIP + (1|GEO),
                    data    = bootData,
                    REML    = F)
    # Create cleaned bootstrapped dataset for this replication
    bootData <- bootData %>%
      mutate( fittedFull = fitted(hoh_mod),
              pred     = predict( object  = modBoot,
                                  newdata = hoh_dat[bootIdx, ]),
              bootRep  = i) %>%
      dplyr::select(NAME, log_CARBON,fittedFull, pred, bootRep)
    # Append dataset
    if(i == 1){
      bootDataLong <- bootData
    } else{
      bootDataLong <- rbind(bootDataLong, bootData) %>%
        arrange(bootRep)
    }
  }
  # Take empirical percentiles of predicted values for each observation
  bootDataFinal <- bootDataLong %>%
                    group_by(NAME) %>%
                    summarize(log_CARBON  = mean(log_CARBON),
                              fittedFull  = mean(fittedFull),
                              bootLower   = quantile(pred, pctLower),
                              bootUpper   = quantile(pred, pctUpper)) %>%
                    arrange(NAME)
  bootDataFinal <- bootDataFinal %>%
                    mutate(predInterval = paste0(round(fittedFull, 2), " [",
                                                  round(bootLower, 2) , " - ",
                                                  round(bootUpper, 2),"]"),
                           bootlow = round(bootLower, 2),
                           bootupr = round(bootUpper, 2)) %>%
                    dplyr::select(log_CARBON, predInterval, bootlow, bootupr)
  return(bootDataFinal)
}

# Test intervals - sometimes we run into issues when our resampled
### dataset doesn't have all 3 levels of GEOLOGY
predIntervals <- bootPredIntervals( reps      = 1000,
                                    randSeed  = 11,
                                    pctLower  = 0.025,
                                    pctUpper  = 0.975)
View(predIntervals)



############# From the pierre roudier github ##############
library(parallel)
library(doParallel)
library(terra)

k = 5
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
GEO <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_250k_reclassMask.tif")
rs <- c(WIP, GEO)
names(rs) <- c("WIP", "GEO")
predlist <- c(WIP)
#val_preds <- matrix(nrow = nrow(hoh_dat), ncol = k)
##### Non-parallel
r <- rast(nrows=5, ncols=9, vals=1:45)
x <- c(r, r*2)
add(x) <- r*3
x

for(i in 1:k) {
    # Generate a bootstrap resample of ph_cal rows
    bootIdx <- sample(x        = seq(1, nrow(hoh_dat)), #indexs from 1 to end of dataframe
                      size     = nrow(hoh_dat), #a non-negative integer giving the number of items to choose 
                      replace  = TRUE) #replacement
    bootData <- hoh_dat[bootIdx, ]  %>% #all 36 rows and all columns
        mutate(log_CARBON = log10(CARBON_1M)) %>% #adding log carbon values
        arrange(NAME) #arranging by sample name 
    # Fit a Hoh SOC model 
    modBoot <- lmer(formula = log10(CARBON_1M) ~ WIP + (1|GEO),
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
            filename = paste0(i, "boot",'.tif'), overwrite = T)
    #list()
    add(predlist) <- pred
    #predstack <- rast(predlist)
    #val_preds[, i] <- predict(modBoot, newdata = rs)
    #predlist <- stack(predlist, predict(rs, modBoot, allow.new.levels = T))
}

predlist
c_mean <- terra::app(predlist, mean)
plot(10^c_mean)

c_interval <- terra::app(predlist, function(x){
    quantile(x, probs = c(0.05, 0.95), na.rm = T)
}, cores = 8, filename = "intervals.tif", overwrite = T)
#c_interval <- terra::quantile(predlist, probs = c(0.025, 0.975), na.rm = T, filename = "intervals.tif")
plot(c_interval)
