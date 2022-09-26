library(terra)
library(mapview)
library(raster)
library(sf)
library(dplyr)

setwd("OneDrive - UW/University of Washington/Data and Modeling/")

WIP <- rast("HLEF/SEAK_WIP_9-10-2021_output.tif")
plot(WIP)
landformsK <- rast("HLEF/HLEF_GEE_Kmeans_landforms__rsplR.tif")
landforms <- rast("HLEF/HLEF_GEElandforms__rsplR.tif")
plot(landforms)
# landforms_class <- classify(landforms, 
#                             matrix(c(-10000, -1, NA), ncol =3, byrow =T), 
#                             include.lowest = T,
#                             right = F)
# landformsK_class <- classify(landformsK, matrix(c(-10000, -1, NA), ncol =3, byrow =T), 
#                              include.lowest = T,
#                              right = F)
# plot(landforms_class)
# plot(landformsK_class == 5)
# landforms_class <- resample(landforms_class, WIP)
# cls <- data.frame(id=0:5, cover=c("ridge", "slope", "vallyfloor", "floodplain", "lowland", "footslope"))
# clsK <- data.frame(id=0:5, cover=c("lowland", "highmountain", "vallyfoot", "narrowridge", "flatridge", "slope"))
# landformsK_class <- categories(landformsK_class, layer = 1, clsK, 2)
# writeRaster(landformsK_class, filename = "HLEF/HLEF_GEE_Kmeans_landforms__rsplR.tif")
#ext(landforms_class) <- ext(landformsK_class)
# hs <- rast("HLEF/HLEF_DEM_5m_shd.tif")
# hs_rpj <- project(hs, WIP, align = T)
# plot(hs_rpj, col = grey(1:100/100),
#      legend = FALSE)

locs_csv <- read.csv("HLEF/HLEF2022LAB-LOCATION.csv")
locs <- st_as_sf(locs_csv, coords = c('lon', 'lat'), crs = "EPSG:4326")
plot(locs['landform'])
#mapView(locs['landform']) +mapView(WIP)

locs_WIP <- extract(WIP, locs, method = "bilinear")
locs_csv$WIP <- locs_WIP[, 2]
locs_csv <- locs_csv %>% 
    mutate(wet_upl_wip = case_when(WIP < 0.5 ~ "wet",
                               WIP >= 0.5 ~ "upl"))
locs_land <- extract(landforms, locs, method = "simple")
locs_csv$landforms <- locs_land[, 2]
locs_landK <- extract(landformsK, locs, method = "simple")
locs_csv$landformsK <- locs_landK[,2]

# hist(locs$WIP, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# min(locs$WIP)
# max(locs$WIP)
# length(locs$WIP)


write.csv(locs_csv, file = "HLEF/HLEF_8_29_2022_locations.csv")
locs_new <- read.csv("HLEF/HLEF_8_29_2022_locations.csv")

library(ggplot2)
ggplot(data = locs_new, aes(x = landforms, y = WIP)) + geom_bar(stat = "summary", fun = "mean")
