library(terra)

#setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/")
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_fill.tif")
hoh_C <- rast("SOIL CARBON/HOH_CARBON_7_20_22_mask.tif")
AGB <- rast("AGB/WA_Biomass/AGB_Forest_NW_USA_2015.tif")
#AGB_rpj <- #terra::project(AGB, "EPSG:26910", filename = "/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/AGB/WA_Biomass/AGB_Forest_NW_USA_2016_WAsouthrpj.tif")
hoh_poly<- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.shp")
hoh_poly_rpj <- terra::project(hoh_poly, "EPSG:5070")

AGB_crop <- crop(AGB, hoh_poly_rpj, mask = T, filename = "AGB/WA_Biomass/HOH_crop_AGB_Forest_NW_USA_2015.tif", overwrite = T)
plot(AGB_crop)

