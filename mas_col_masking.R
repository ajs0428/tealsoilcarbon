library(terra)
library(rgdal)


setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")


mas_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM.tif")
col_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif")