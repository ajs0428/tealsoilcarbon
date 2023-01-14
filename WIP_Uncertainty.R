#Checking out WIP uncertainty 
    # Look at changing the WIP 0.5 threshold and see how error in validation changes
library(terra)
library(tidyverse)
library(caret)
library(pROC)
setwd("OneDrive - UW/University of Washington/Data and Modeling/")
WIP <-  rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
# bbound <- terra::draw(x = "polygon")
# bpoint <- terra::draw(x = "point")
# crs(bpoint) <- crs(soil_C)
# buffpt <- terra::buffer(bpoint, width = 1000, filename = "SOIL CARBON/CrypticCarbonMaps/circle1000m.shp")
# writeVector(buffpt,filename = "SOIL CARBON/CrypticCarbonMaps/circle1000m.shp" )
# plot(buffpt, add = T)
# buffpt_extract <- crop(WIPm, buffpt, mask = T, filename = "SOIL CARBON/CrypticCarbonMaps/WIP_CROP.tif")
# plot(buffpt_extract)

WIP_valpts <- vect("WIP/Hoh_data/Training_and_Validation_Data/Hoh_Validation_Data_07182022.shp")
WIP_valpts_df <- as_tibble(values(WIP_valpts))
names(WIP_valpts_df)

WIP_valpts_df<- WIP_valpts_df %>%
    mutate(wip_8_04_.5class = cut(wip_8_04, breaks = c(-1,0.49999,1), labels = c("UPL", "WET"))) %>%
    mutate(wip_8_04_.35class = cut(wip_8_04, breaks = c(-1,0.35,1), labels = c("UPL", "WET"))) %>%
    mutate(wip_8_04_.65class = cut(wip_8_04, breaks = c(-1,0.65,1), labels = c("UPL", "WET"))) %>%
    mutate(wip_8_03_.5class = cut(wip_8_03, breaks = c(-1,0.49999,1), labels = c("UPL", "WET"))) %>%
    mutate(wip_lidar_.5class = cut(wip_lidar_, breaks = c(-1,0.49999,1), labels = c("UPL", "WET"))) %>%
    mutate_if(is.character, as.factor) #%>%
    #mutate_at(c("wip_8_04_.5class", "MH_Class"), as.factor)
confusionMatrix(WIP_valpts_df$MH_Class, WIP_valpts_df$wip_8_04_.65class)

r_curve <- roc(response = as.ordered(WIP_valpts_df$MH_Class),
               predictor = as.ordered(WIP_valpts_df$wip_8_04_.5class), percent = T)

