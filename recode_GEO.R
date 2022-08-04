library(terra)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wa_geology <- vect("SPATIAL LAYERS/WA_GEO/WA_geologic_unit_poly_100k.shp")
hoh_geology <- vect('SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_GEO_CLIP_7_22.shp')
hoh_poly <- vect('SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.shp')
plot(hoh_poly)
col_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_nwi_poly.shp")
plot(col_poly)
#hoh_polygon <- as.polygons(hoh_poly)
#plot(hoh_polygon)
#crs(hoh_poly) == crs(geology)

wa_geo_proj<- terra::project(wa_geology, "EPSG:26910")

hoh_geo_crop <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP.gpkg")#crop(wa_geo_proj, hoh_poly)
#writeVector(hoh_geo_crop, "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP.gpkg")
plot(hoh_geo_crop, "GEOLOGIC_A", add = F)


# #Bring in points to visualize where they land
# pts <- read.csv("ANALYSIS/ALL_SOILC_7_11_22.csv") 
# pts_vect <- vect(pts, geom = c("lon", "lat"))
# set.crs(pts_vect, "EPSG:4326")
# pts_vect <- terra::project(pts_vect, "EPSG:26910")
# #plot(pts_vect, add = T)
# e <- c(405000,410000,529500, 530000)
# zoom(hoh_geo_crop, "GEOLOGIC_A", e = e)
# points(pts_vect)

#####rename all Miocene in Hoh to Miocene-Eocene and Present to Holocen-Quaternary####
hoh_geo_crop$GEOLOGIC_A[hoh_geo_crop$GEOLOGIC_A == "Miocene"] <- "Miocene-Eocene"
hoh_geo_crop$GEOLOGIC_A[hoh_geo_crop$GEOLOGIC_A == "Quaternary"] <- "Holocene-Quaternary-Present"
hoh_geo_crop$GEOLOGIC_A[hoh_geo_crop$GEOLOGIC_A == "Present"] <- "Holocene-Quaternary-Present"
#hoh_geo_crop$GEOLOGIC_A <- as.factor(hoh_geo_crop$GEOLOGIC_A)

##### write to new layer and rasterize #####
writeVector(hoh_geo_crop, "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP_AGG.shp", overwrite = T)
hoh_geo_crop_agg <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP_AGG.shp")
hoh_geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/GEO_resample_z10_R.tif")
hoh_geo_crop_agg_rast <- terra::rasterize(hoh_geo_crop_agg, hoh_geo_old, "GEOLOGIC_A", 
                                          filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_R_GEO_CROP_AGG.tif",
                                          overwrite = T)
plot(hoh_geo_crop_agg_rast)

#Other Study areas

#get shapefile
#col_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_poly.shp")
mas_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_poly/mashel_poly.shp")
mas<- read.csv("ANALYSIS/MAS_SOILC_7_11_22_samp.csv")
mas_points <- vect(mas, geom = c("lon", "lat"))
set.crs(mas_points, "EPSG:4326")
#reproject
col_poly_proj <- terra::project(col_poly, "EPSG:26910")
mas_poly_proj <- terra::project(mas_poly, "EPSG:26910")
mas_points_proj <- terra::project(mas_points, "EPSG:26910")

#crop wa geo then read in written file
col_geo_crop <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/COL_R_GEO_CROP.gpkg")#crop(wa_geo_proj, col_poly_proj)
mas_geo_crop <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/MAS_R_GEO_CROP.gpkg")#crop(wa_geo_proj, mas_poly_proj)
#writeVector(mas_geo_crop,  "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/MAS_R_GEO_CROP.gpkg")
plot(col_geo_crop, "GEOLOGIC_A")
plot(mas_geo_crop, "GEOLOGIC_U")
plot(mas_points_proj, add = T)
#all of the geologic ages in all study areas
h <- hoh_geo_crop$GEOLOGIC_A #"Miocene-Eocene"      "Holocene-Quaternary" "Pleistocene"  
c <- col_geo_crop$GEOLOGIC_A #"Eocene"  "pre-Tertiary" "Quaternary"   "Pleistocene"  "Present"   
m <- mas_geo_crop$GEOLOGIC_A #"Eocene" "Miocene, middle to upper" "Miocene-Oligocene" "Oligocene-Eocene" "Quaternary" "Pleistocene" "Present" 

hcm <- c(h, c, m)
unique(h) #unique values


col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "Eocene"] <- "Miocene-Eocene"
col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "Quaternary"] <- "Holocene-Quaternary-Present"
col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "Present"] <- "Holocene-Quaternary-Present"
#col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "pre-Tertiary"] <- "Holocene-Quaternary"

mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Miocene, middle to upper"] <- "Miocene-Eocene"
mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Miocene-Oligocene"] <- "Miocene-Eocene"
mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Oligocene-Eocene"] <- "Miocene-Eocene"
mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Eocene"] <- "Miocene-Eocene"
mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Quaternary"] <- "Holocene-Quaternary-Present"
mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Present"] <- "Holocene-Quaternary-Present"

##### write to new layer and rasterize #####
col_geo_recrop <- crop(col_geo_crop, col_poly)#, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_R_GEO_NWICROP_AGG.tif")
plot(col_geo_recrop, "GEOLOGIC_A")
writeVector(col_geo_recrop, "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/COL_R_GEO_CROP_AGG.shp", overwrite = T)
geo_crop_agg <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/COL_R_GEO_CROP_AGG.shp")
geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_GEO_rspl.tif")
geo_crop_agg_rast <- terra::rasterize(geo_crop_agg, geo_old, "GEOLOGIC_A",
                                      filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_R_GEO_CROP_AGG.tif", overwrite = T)
plot(geo_crop_agg_rast)

col_geo_crop_agg_rast <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_R_GEO_CROP_AGG.tif")
plot(col_geo_crop_agg_rast)

##### write to new layer and rasterize #####
writeVector(mas_geo_crop, "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/MAS_R_GEO_CROP_AGG.shp", overwrite =T)
geo_crop_agg <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/MAS_R_GEO_CROP_AGG.shp")
geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_geo_rspl.tif")
geo_crop_agg_rast <- terra::rasterize(geo_crop_agg, geo_old, "GEOLOGIC_A", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_R_GEO_CROP_AGG.tif")
plot(geo_crop_agg_rast)

mas_geo_crop_agg_rast <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_R_GEO_CROP_AGG.tif")

#unique values of the study points
all_geo <- read.csv("ANALYSIS/ALL_SOILC_7_11_22_samp.csv")
df <- subset(all_geo, STUDY_AREA == "COL")
pts <- vect(df, geom = c("lon", "lat"))
set.crs(pts, "EPSG:4326")
pts <- terra::project(pts, "EPSG:26910")
plot(pts)

pts_geoext <- terra::extract(col_geo_crop_agg_rast, pts)
pts$GEO_7_22_22 <- pts_geoext$GEOLOGIC_A

pts_new <- values(pts)

write.csv(pts_new, "ANALYSIS/COL_SOILC_NEWGEO_7_22_22.csv")




