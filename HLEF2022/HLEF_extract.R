library(terra)
library(mapview)
library(raster)
library(sf)

setwd("OneDrive - UW/University of Washington/Data and Modeling/")

WIP <- raster("HLEF/SEAK_WIP_9-10-2021_output.tif")
plot(WIP)
landforms <- raster("HLEF/GEE_KMeans_LandformsClass.tif")
plot(landforms>0)
# hs <- rast("HLEF/HLEF_DEM_5m_shd.tif")
# hs_rpj <- project(hs, WIP, align = T)
# plot(hs_rpj, col = grey(1:100/100),
#      legend = FALSE)

locs_csv <- read.csv("HLEF/LEF_8_4_2022_locations.csv")
locs <- st_as_sf(locs_csv, coords = c('lon', 'lat'), crs = "EPSG:4326")
plot(locs['landform'])
mapView(locs['landform']) +mapView(WIP)

locs_vals <- read.csv("HLEF/LEF_8_4_2022_locations.csv")
locs_csv$WIP <- locs_vals$WIP

# locs_WIP <- extract(WIP, locs, method = "bilinear")
# locs$WIP <- locs_WIP[, 2]

hist(locs$WIP, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
min(locs$WIP)
max(locs$WIP)
length(locs$WIP)

write.csv(locs_csv, file = "HLEF/LEF_8_4_2022_locations.csv")
