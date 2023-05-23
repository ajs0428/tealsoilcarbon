#### Cryptic Carbon Spatial ####
library(lme4)
library(lmerTest)
library(terra)
library(sf)
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
library(spatialEco)
library(gstat)
library(spdep)
library(moranfast)


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 

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

mod <-  lmer(sqrt(CARBON_1M) ~ WIP + (1|LITHOL), data = hoh_dat)

#### Spatial Investgation Correlograms and Variograms ####

#### variogram from gstat package ####
hoh_dat$resid <- resid(mod)
coordinates(hoh_dat) <- c("jlon", "jlat")
#hoh_sp <- st_as_sf(hoh_dat, coords = c("jlon", "jlat"), crs = 4326)
vario <- variogram(resid~jlon+jlat, data = hoh_dat)
plot(vario$dist, vario$gamma, main = "Variogram of model residuals ")


#### variogram cloud from Landscape Ecology ####
# compute a variogram cloud 
lag <- dist(coordinates(hoh_dat)) # 'dist' computes the Euclidean distances as the default
varTP <- dist(hoh_dat$CARBON_1M)^2*.5
plot(lag,varTP,xlab='Spatial lag (m)',ylab='Semivariance of SOC (Mg/ha)')
# add a smoothing line to examine the average pattern of semivariance as a function of distance
lines(lowess(lag,varTP),col='red',lwd=2)
# a rule of thumb is to not interpret beyond 1/2 of the spatial extent of the dataset
abline(v=max(lag)/2,lwd=2,lty=2,col='dodgerblue')
# add a legend to the topleft corner of the plot
legend("topleft",c('lowess line','half max extent'),lty=c(1,2),col=c('red','dodgerblue'),bty='n',lwd=2) 

dev.off()

##### Global Moran's I #####
moranfast(hoh_dat$CARBON_1M, hoh_dat$jlon, hoh_dat$jlat)

