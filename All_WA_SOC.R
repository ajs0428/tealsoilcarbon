library(lme4)
library(MASS)
library(lmerTest)
library(RLRsim)
library(terra)
library(sf)
library(tidyverse)
library(car)
library(ggplot2)
library(sjPlot)
library(sjstats)
library(ggeffects)
library(merTools)
library(glmnet)
library(stats)
library(ggcorrplot)
library(RColorBrewer)
library(cowplot)


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')


wa <- read.csv("SOIL CARBON/ANALYSIS/WA_SOC_geo.csv")

#wa <- read.csv("SOIL CARBON/ANALYSIS/WA_SOC.csv")
# wa_pts <- vect(wa, geom = c("lon", "lat"), crs = "EPSG:4326")
# wa_pts <- terra::project(wa_pts, "EPSG:26910")
# wa_geo <- vect("WA_Geo/WA_Geology_100K.gpkg")
# wa_geo_proj <- terra::project(wa_geo, "EPSG:26910")
# geo_extract <- terra::extract(wa_geo_proj, wa_pts)
# wa$geo_extract.Label <- geo_extract$GEO_AGE_SUM
# wa <- na.omit(wa)
# write.csv(wa, "SOIL CARBON/ANALYSIS/WA_SOC_geo.csv")
# col <- wa[wa$site == "COL",]
# col_pts <- vect(col, geom = c("lon", "lat"), crs = "EPSG:4326")
# col_pts <- terra::project(col_pts, "EPSG:26910")
# col_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")
# col_extract <- terra::extract(col_WIP, col_pts)
# wa[wa$site == "COL", "WIP"] <- col_extract$Band_1




#subset for easier viewing and access
wa_dat <- wa |> dplyr::select( "site", "sample_name", "lat","lon","CHN_30cm_Cstock_Mg_cm2",
                                            "CHN_90cm_Cstock_Mg_ha",  "CHN_1m_Cstock_Mg_ha",   "CHN_120cm_Cstock_Mg_ha",
                                            "WIP", "geo_extract.Label" ) |>
    mutate_if(is.character, as.factor) |>
    stats::setNames(c("site", "name", "lat","lon","SOC30",
                      "SOC90",  "SOC1",   "SOC120",
                      "WIP", "GEO_Label")) |> 
    as.data.frame()
str(wa_dat)

hist(wa_dat$SOC1)

# Quick R^2 function
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}

#### LMER #####
wa_mod1 <- lmer(log(SOC1) ~ WIP + (1|site) + (1|GEO_Label), data = wa_dat)
wa_mod2 <- lmer((SOC1) ~ WIP + (1|GEO_Label), data = wa_dat) 
wa_mod3 <-  lmer((SOC1) ~ WIP + (1|site), data = wa_dat)
wa_mod4 <-  lm((SOC1) ~ WIP, data = wa_dat)

exactRLRT(wa_mod2, wa_mod1, wa_mod3)
exactRLRT(wa_mod3, wa_mod1, wa_mod2)

wa_mod2F <- lmer((SOC1) ~ WIP + (1|GEO_Label), data = wa_dat, REML = F) 
wa_mod3F <-  lmer((SOC1) ~ WIP + (1|site), data = wa_dat, REML = F)

exactLRT(wa_mod2F, wa_mod4)
exactLRT(wa_mod3F, wa_mod4)

#### LMER Summary####

summary(wa_mod1)
plot(wa_mod1)
r.sq(log(wa_dat$SOC1), fitted(wa_mod1))
plot(log(wa_dat$SOC1), fitted(wa_mod1), ylim = c(3, 7), xlim = c(3,7),col = unique(wa_dat$site))
abline(0,1)
# Add a legend
legend("bottomright", pch=16,
       legend = levels(wa_dat$site),
       col = unique(wa_dat$site))

RMSE.merMod(wa_mod1, scale = T)*sd(wa_dat$SOC1)

hist(wa_dat$SOC1)

##### GLMER #####

wa_gmod1 <- glmer( (SOC1) ~   (WIP) + (1|site) + (1|GEO_Label), family = Gamma(link = "log"), data = wa_dat);summary(wa_gmod1)
wa_gmod1.1 <- glmer( (SOC1) ~   WIP*site + (1|GEO_Label), family = Gamma(link = "log"), data = wa_dat);summary(wa_gmod1.1)
wa_gmod1.2 <- glmer( (SOC1) ~   WIP*GEO_Label + (1|site), family = Gamma(link = "log"), data = wa_dat);summary(wa_gmod1.2)
wa_gmod1.3 <- glmer( (SOC1) ~   WIP + (1|GEO_Label:site), family = Gamma(link = "log"), data = wa_dat);summary(wa_gmod1.3)

wa_gmod2 <- glmer((SOC1) ~ WIP + (1|GEO_Label), family = Gamma(link = "log"),data = wa_dat);summary(wa_gmod2)
wa_gmod3 <-  glmer((SOC1) ~ WIP + (1|site), family = Gamma(link = "log"),data = wa_dat);summary(wa_gmod3)
wa_gmod4 <-  glm((SOC1) ~ WIP, family = Gamma(link = "log"), data = wa_dat);summary(wa_gmod3)


#exactRLRT(wa_gmod2, wa_gmod1, wa_gmod4)
#exactLRT(wa_gmod3, wa_gmod4)

#### Gamma Summary ####
summary(wa_gmod1.3)
deviance(wa_gmod1.3) #deviance
pchisq(deviance(wa_gmod1.3),length(wa_dat$site)-1,lower.tail = F) #p-value deviance
gamma.dispersion(wa_gmod1.3)

plot(wa_gmod1.3)
plot(cooks.distance(wa_gmod1.3))
r.sq((wa_dat$SOC1), fitted(wa_gmod1.3))
plot(wa_dat$SOC1, fitted(wa_gmod1.3), col = as.factor(wa_dat$WIP>0.5) )
abline(0,1)
abline(lm((fitted(wa_gmod1.3)~ wa_dat$SOC1)), col = "red", lty = 3)
#lines(seq(0,700,7.4), predict(wa_gmod1, newdata = wa_dat, type = "response"), col = "black")
coef(wa_gmod1)
# Add a legend
legend("topleft", pch=16,
       legend = levels(wa_dat$GEO_Label),
       col = unique(wa_dat$GEO_Label))

rmse(wa_gmod1.3)#*sd(wa_dat$SOC1)
#confint(wa_gmod1.2, method = "boot")

ggplot(wa_dat, aes(y = SOC1, x = WIP, col = GEO_Label)) +
    geom_point(aes(shape = as.factor(WIP>=0.5) ),size = 6, stroke = 1, show.legend = F)+
    geom_smooth(aes(y = predict(wa_gmod1.3, type = "response")), se = F)


gm <- ggeffects::ggpredict(wa_gmod1, type = "re", terms = c("WIP","GEO_Label"))
plot(gm)
?ggeffect
