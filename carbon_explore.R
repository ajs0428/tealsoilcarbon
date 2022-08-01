library(stats, dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(raster)
library(rgdal)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 
hoh <- read.csv('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/HOH_SOIL_CARBON_2021/Hoh_soilcarbon_variablestable_v03.csv')

mash <- read.csv('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/MASHEL SOIL CARBON/Mash_soilcarbon_varaiblestable_v03.csv')

str(hoh)
hoh_C = lm(Cstock_Mg_ ~ WIP + HGM + DTW + slope + TWI, data = hoh)
summary(hoh_C)
anova(hoh_C)

hoh_C$coefficients

############# Meghan's Code ######################
#In case my R code is helpful
##Load packages

library(randomForest)
library(corrplot)
##Load data
all<-read.csv("ANALYSIS/R/Hoh_Mash_combined_data.csv")[1:11]
head(all)
as.factor(all$MHCLASS_FACT)

##Explore all data
lm.all<-lm(all$Cs_M___ ~ all$WIPv8 *all$DTW)
summary(lm.all)
A<-cor(all[,2:6])
head(round(A,2))
corrplot(A, method="circle")
pairs(all[,1:6], pch = 19)

##Break into classes
all.upl<- all[all$MHCLASS=='UPL',]
all.riv<- all[all$MHCLASS=='RIV',]
all.wet<- all[all$MHCLASS=='WET',]


##Explore upland
lm.all.upl<-lm(all.upl$Cs_M___ ~ all.upl$WIPv8)
summary(lm.all.upl)
u<-cor(all.upl[,1:6])
head(round(u,2))
corrplot(u, method="circle")
##Explore wetland
lm.all.wet<-lm(all.wet$Cs_M___ ~ all.wet$WIPv8 * all.wet$DTW)
summary(lm.all.wet)
w<-cor(all.wet[,1:6])
head(round(w,2))
corrplot(w, method="circle")
#Explore riverine
lm.all.riv<-lm(all.riv$Cs_M___ ~ all.riv$slope)
summary(lm.all.riv)
r<-cor(all.riv[,1:6])
head(round(r,2))
corrplot(r, method="circle")

#Mashel
all.wet.m<- all.wet[all.wet$StudyArea=="MASH",]
lm.all.wet.m<-lm(all.wet.m$Cs_M___ ~ all.wet.m$WIPv8)
plot(all.wet.m$WIPv8, all.wet.m$Cs_M___, pch=18)
summary(lm.all.wet.m)
w<-cor(all.wet.m[,1:6])
head(round(w,2))
corrplot(w, method="circle")
pairs(all.wet.m[,1:6], pch = 19)
#Hoh
all.wet.h<- all.wet[all.wet$StudyArea=="HOH",]
lm.all.wet.h<-lm(all.wet.h$Cs_M___ ~ all.wet.h$WIPv8)
plot(all.wet.h$WIPv8, all.wet.h$Cs_M___, pch=18)
summary(lm.all.wet.h)
w<-cor(all.wet.h[,1:6])
head(round(w,2))
pairs(all.wet.h[,1:6], pch = 19)

# Basic box plot- all
p <- ggplot(all, aes(x=Cs_M___, y=MHCLASS)) +
    geom_boxplot()
p + coord_flip()

# Basic box plot- hoh
all.hoh<- all[all$StudyArea=="HOH",]
head(all.hoh)

p <- ggplot(all.hoh, aes(x=Cs_M___, y=MHCLASS, fill = MHCLASS)) +
    geom_boxplot() + theme(element_blank()) +
    stat_summary(fun = mean, colour = "black", geom = "point", shape=18, size = 5) +
    scale_fill_manual(values=c("#57B8FF", "#FBB13C", "#AFD5AA"), guide = 'none')
p + coord_flip()


# Basic box plot - mash
all.MASH<- all[all$StudyArea=="MASH",]

p <- ggplot(all.MASH, aes(x=Cs_M___, y=MHCLASS)) +
    geom_boxplot()
p + coord_flip()


################# Spatial mapping #########################
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/TWI_NAD83Z10.tif'))
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/MainChannelDTW.tif'))
GDALinfo(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/riv_wet_up_clean.tif'))

TWI <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/TWI_NAD83Z10.tif'))
DTW <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/MainChannelDTW.tif'))
CLASS <- raster(paste0(wd, 'SPATIAL LAYERS/hoh_dtw_twi_classes/riv_wet_up_clean.tif'))
WIP <- raster(paste0(wd, "SPATIAL LAYERS/Hoh_WIP_fill.tif"))

crs(TWI)
crs(DTW)
crs(CLASS)
crs(WIP) 

extent(WIP)
extent(TWI)
extent(CLASS)
extent(CLASS) == extent(DTW)
extent(CLASS) == extent(TWI)
extent(TWI) <- alignExtent(DTW, TWI, snap = 'near')
TWI_rspl <- raster::resample(TWI, DTW, method='ngb')

plot(CLASS)

#Define dataframes 
hoh_check <- read.csv("ANALYSIS/R/hoh_c_wip_check.csv")
head(hoh_check)

#linear mixed model with class as random effect
lmer_mod <- lmer(carbon ~ DTW_CHECK + TWI_CHECK + WIP_CHECK2 + (1|RIV_WET_UP_CLEAN), data = hoh_check);summary(lmer_mod)
anova(lmer_mod)
plot(predict(lmer_mod), hoh_check$carbon, col = hoh_check$RIV_WET_UP_CLEAN)
plot(hoh_check$WIP_CHECK2, hoh_check$carbon, col = hoh_check$RIV_WET_UP_CLEAN)
plot(hoh_check$SLP_CHECK, hoh_check$carbon, col = hoh_check$RIV_WET_UP_CLEAN)
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")

r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
r.sq(all.hoh$carbon, fitted(lmer_mod))

#stack rasters
rs <- stack(TWI_rspl, DTW, CLASS, WIP)
#names(rs) <- c("TWI", "DTW", "MHCLASS_FACT", "WIPv8")
names(rs) <- c("TWI_CHECK", "DTW_CHECK", "RIV_WET_UP_CLEAN", "WIP_CHECK2")

betas <- fixef(lmer_mod)
rands <- ranef(lmer_mod)

#Pred <- (DTW*betas[2]) + (TWI_rspl*betas[3] + (WIP*betas[4]) + betas[4])
Pred = predict(rs, lmer_mod)
plot(Pred)
writeRaster(Pred, 'HOH_SOIL_CARBON_2021/HOH_SOIL_C_PREDICT.tif', 'GTiff')

#Do not uncomment 
#TWI_rpj <- projectRaster(TWI, crs = crs(DTW))
#crs(TWI_rpj)
#writeRaster(TWI_rpj, 'TWI_NAD83Z10.tif', 'GTiff')

