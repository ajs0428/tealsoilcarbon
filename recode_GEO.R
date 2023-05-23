library(terra)
library(dplyr)
library(rgdal)
library(sf)
library(tidyterra)

setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
hoh_poly <- vect('SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.shp')
# 
wa_250 <- sf::st_read("SOIL CARBON/SPATIAL LAYERS/WA_GEO/ger_portal_surface_geology_250k/WGS_Surface_Geology_250k.gdb",
                      layer = "MapUnitPolys")
# wa_250t <- vect(wa_250)
# wa_250tproj <- terra::project(wa_250t, "EPSG:26910")
# hoh_250 <- crop(wa_250tproj, hoh_poly)
# plot(hoh_250, "MapUnit")
# hoh_250_reclass <- hoh_250 |> tidyterra::mutate(MapUnit = case_when(MapUnit == "Qapw(2)" ~ "Quat_old_clastic",
#                                                          MapUnit == "Qguc" ~ "Quat_old_clastic",
#                                                          MapUnit == "Qapo_NW" ~ "Quat_old_clastic",
#                                                          MapUnit == "Qad_NW" ~ "Quat_old_clastic",
#                                                          MapUnit == "Qapw(1)" ~ "Quat_old_clastic",
#                                                          MapUnit == "Qap_NW" ~ "Quat_old_clastic",
#                                                          
#                                                          MapUnit == "Qao_NW" ~ "Quat_old_alluv",
#                                                          MapUnit == "Qa_NW" ~ "Quat_new",
#                                                          MapUnit == "Qls_NW" ~ "Quat_new",
#                                                          MapUnit == "wtr_NW" ~ "Quat_new", 
#                                                          
#                                                          MapUnit == "MEm" ~ "MioEo",
#                                                          MapUnit == "MEmst" ~ "MioEo",
#                                                          TRUE ~ "MioEo")) 
# plot(hoh_250_reclass, "MapUnit")
# 
# GEOm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_250k_reclassMask.tif")
# hoh_250_reclass_tif <- rasterize(hoh_250_reclass, GEOm, field = "MapUnit", 
#                                  filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_250k_reclassified.tif",
#                                  overwrite = T)
#writeRaster(hoh_250_reclass, "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_250k_reclassified.tif")


wa_100 <- sf::st_read("SOIL CARBON/SPATIAL LAYERS/WA_GEO/WA_geologic_unit_poly_100k.shp")
wa_100t <- vect(wa_100)
wa_100tproj <- terra::project(wa_100t, "EPSG:26910")
hoh_100 <- crop(wa_100tproj, hoh_poly)
plot(hoh_100, "LITHOLOGY")
hoh_litho <- terra::subset(hoh_100, hoh_100$LITHOLOGY =="continental glacial and non-glacial deposits, Fraser-age")


hoh_100_reclass <- hoh_100 |> tidyterra::mutate(LITHOLOGY = case_when(LITHOLOGY == "alpine glacial drift, pre-Fraser"   ~ "glac_drift",
                                                                      LITHOLOGY == "alpine glacial drift, pre-Wisconsinan, older" ~ "glac_drift",
                                                                      LITHOLOGY == "alpine glacial drift, Fraser-age" ~ "glac_drift",
                                                                      LITHOLOGY == "alpine glacial drift, pre-Wisconsinan, younger"  ~ "glac_drift",
                                                                     
                                                                       LITHOLOGY == "alpine glacial till, pre-Wisconsinan"  ~ "till_outwash",
                                                                      LITHOLOGY == "alpine glacial outwash, pre-Fraser"  ~ "till_outwash",
                                                                      LITHOLOGY == "alpine glacial till, Fraser-age" ~ "till_outwash",
                                                                      LITHOLOGY == "glacial outwash, alpine, Fraser-age"  ~ "till_outwash",
                                                                    LITHOLOGY == "alpine glacial till, pre-Fraser" ~ "till_outwash",
                                                                    LITHOLOGY == "alpine glacial outwash, pre-Wisconsinan" ~ "till_outwash",
                                                                    
                                                                    
                                                                    LITHOLOGY == "alluvium"  ~ "alluvium_marine_water",
                                                                    LITHOLOGY == "water" ~ "alluvium_marine_water",
                                                                    
                                                                    LITHOLOGY == "tectonic breccia"  ~ "MioEo",
                                                                    LITHOLOGY ==  "marine clastic rocks, dominantly thick-bedded lithic sandstone" ~ "MioEo",
                                                                    LITHOLOGY == "continental glacial and non-glacial deposits, Fraser-age"  ~ "MioEo",
                                                                    LITHOLOGY == "marine sedimentary rocks"  ~ "MioEo",
                                                                    LITHOLOGY == "mass-wasting deposits, mostly landslides" ~ "MioEo",
                                                                    TRUE ~ "MioEo")) 
plot(hoh_100_reclass, "LITHOLOGY")
writeVector(hoh_100_reclass, "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_100_reclass_lithology.shp")

GEOm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")
WIPm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
hoh_100_reclass_tif <- rasterize(hoh_100_reclass, GEOm, field = "LITHOLOGY", 
                                 filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif",
                                 overwrite = T)


#Bring in points to visualize where they land
pts <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbon_Jlat_lith.csv")
pts_vect <- vect(pts, geom = c("jlon", "jlat"))
#set.crs(pts_vect, "EPSG:4326")
pts_vect <- terra::project(pts_vect, "EPSG:26910")
plot(pts_vect, add = T)
pts_vect_extr <- terra::extract(hoh_100_reclass, pts_vect)
pts_vect_WIPextr <- terra::extract(WIPm, pts_vect, method = "bilinear")
#pts_vect_extr<- cbind(pts_vect, pts_vect_extr)
pts_vect$LITHOLOGY <- pts_vect_extr$LITHOLOGY
pts_vect$jlat <- pts$jlat
pts_vect$jlon <- pts$jlon
pts_vect$WIPv8 <- pts_vect_WIPextr$WET
#writeVector(pts_vect, filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/CrypticCarbon_Jlat_lith.shp", overwrite = T)
write.csv(pts_vect, file = "SOIL CARBON/ANALYSIS/CrypticCarbon_Jlat_lithextract.csv")



# #####rename all Miocene in Hoh to Miocene-Eocene and Present to Holocen-Quaternary####
# hoh_geo_crop$GEOLOGIC_A[hoh_geo_crop$GEOLOGIC_A == "Miocene"] <- "Miocene-Eocene"
# hoh_geo_crop$GEOLOGIC_A[hoh_geo_crop$GEOLOGIC_A == "Quaternary"] <- "Holocene-Quaternary-Present"
# hoh_geo_crop$GEOLOGIC_A[hoh_geo_crop$GEOLOGIC_A == "Present"] <- "Holocene-Quaternary-Present"
# #hoh_geo_crop$GEOLOGIC_A <- as.factor(hoh_geo_crop$GEOLOGIC_A)
# 
# ##### write to new layer and rasterize #####
# writeVector(hoh_geo_crop, "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP_AGG.shp", overwrite = T)
# hoh_geo_crop_agg <- vect("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP_AGG.shp")
# hoh_geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/GEO_resample_z10_R.tif")
# hoh_geo_crop_agg_rast <- terra::rasterize(hoh_geo_crop_agg, hoh_geo_old, "GEOLOGIC_A", 
#                                           filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_R_GEO_CROP_AGG.tif",
#                                           overwrite = T)
# plot(hoh_geo_crop_agg_rast)

#Other Study areas

#get shapefile
col_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_poly.shp")
mas_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_poly/mashel_poly.shp")
mas<- read.csv("ANALYSIS/MAS_SOILC_7_11_22_samp.csv")
mas_points <- vect(mas, geom = c("lon", "lat"))
set.crs(mas_points, "EPSG:4326")
#reproject
col_poly_proj <- terra::project(col_poly, "EPSG:26910")
mas_poly_proj <- terra::project(mas_poly, "EPSG:26910")
mas_points_proj <- terra::project(mas_points, "EPSG:26910")

#crop wa geo then read in written file
hoh_geo_crop <- vect("SOIL CARBON/SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/HOH_R_GEO_CROP.gpkg")
col_geo_crop <- vect("SOIL CARBON/SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/COL_R_GEO_CROP.gpkg")
col_geo_crop <- crop(wa_geo_proj, col_poly_proj)
mas_geo_crop <- vect("SOIL CARBON/SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/MAS_R_GEO_CROP.gpkg")#crop(wa_geo_proj, mas_poly_proj)
#writeVector(mas_geo_crop,  "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/MAS_R_GEO_CROP.gpkg")
plot(hoh_geo_crop, "GEOLOGIC_A")
plot(col_geo_crop, "LITHOLOGY")
plot(mas_geo_crop, "LITHOLOGY")
plot(mas_points_proj, add = T)
#all of the geologic ages in all study areas
h <- hoh_geo_crop$GEOLOGIC_A #"Miocene-Eocene"      "Holocene-Quaternary" "Pleistocene"  
c <- col_geo_crop$GEOLOGIC_A #"Eocene"  "pre-Tertiary" "Quaternary"   "Pleistocene"  "Present"   
m <- mas_geo_crop$GEOLOGIC_A #"Eocene" "Miocene, middle to upper" "Miocene-Oligocene" "Oligocene-Eocene" "Quaternary" "Pleistocene" "Present" 

hcm <- c(h, c, m)
unique(hcm) #unique values from geology polygons
#unique(values(col_geo_crop)$GEOLOGIC_A) #unique values from colville geology polygon
unique(pts_extract$GEOLOGIC_A) #unique values from all points extracted
unique(hcm)[!(unique(hcm) %in% unique(pts_extract$GEOLOGIC_A))] #values from polygons not in points
new_wa_geo_proj <- as_tibble(wa_geo_proj$GEOLOGIC_A) %>% 
    mutate_all(~case_when(. == "Miocene" ~ "Miocene-Eocene", 
                          . == "Present" ~ "Quaternary", 
                          . == "Miocene, middle to upper" ~ "Miocene-Eocene", 
                          . == "Miocene-Oligocene" ~ "Oligocene-Eocene", 
                           TRUE ~ .))
wa_geo_proj$GEOLOGIC_A <- new_wa_geo_proj$value

#Rewrite the crop study areas with the replaced wa_geo_proj
hoh_geo_newcrop <- crop(wa_geo_proj, hoh_poly)#, filename = "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/hoh_geo_newcrop_8_6_22.gpkg")
mas_geo_newcrop <- crop(wa_geo_proj, mas_poly_proj)#, filename = "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/mas_geo_newcrop_8_6_22.gpkg")
col_geo_newcrop <- crop(wa_geo_proj, col_poly_proj)#, filename = "SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/col_geo_newcrop_8_6_22.gpkg")

#check the unique values
h <- hoh_geo_newcrop$GEOLOGIC_A 
c <- col_geo_newcrop$GEOLOGIC_A 
m <- mas_geo_newcrop$GEOLOGIC_A 
hcm <- c(h, c, m)
unique(hcm)
unique(pts_extract$GEOLOGIC_A)
unique(hcm)[!(unique(hcm) %in% unique(pts_extract$GEOLOGIC_A))] #values from polygons not in points

#write the new crop study areas and rasterize
# hoh_geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/GEO_resample_z10_R.tif")
# mas_geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/mashel_geo_rspl.tif")
# col_geo_old <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/col_GEO_rspl.tif")

#This is a function to write the new crop vector and rasterize
write_all <- function(new_crop, study_area_name, date_str, oldgeo){
    writeVector(new_crop, paste0("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/", study_area_name, date_str, "R_GEO_CROP.gpkg"), overwrite = T)
    new_geo_crop <- vect(paste0("SPATIAL LAYERS/WA_GEO/GEO_STUDY_AREAS/", study_area_name, date_str, "R_GEO_CROP.gpkg"))
    geo_crop_agg_rast <- terra::rasterize(new_geo_crop, oldgeo, "GEOLOGIC_A",
                                          filename = paste0("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/", study_area_name,"/",study_area_name, date_str,"R_GEO_CROP.tif"), overwrite = T)
    
}
#
write_all(col_geo_newcrop, "COL", "8_6_22", col_geo_old)


check <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL8_6_22R_GEO_CROP.tif")
plot(check, legend = T)

#add the new geo values to the points for GEOLOGIC_A
pts$GEO_8_6_22 <- pts_extract$GEOLOGIC_A
write.csv(pts, file = "ANALYSIS/ALL_SOILC_8_6_22.csv")

# col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "Eocene"] <- "Miocene-Eocene"
# col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "Quaternary"] <- "Holocene-Quaternary-Present"
# col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "Present"] <- "Holocene-Quaternary-Present"
# #col_geo_crop$GEOLOGIC_A[col_geo_crop$GEOLOGIC_A == "pre-Tertiary"] <- "Holocene-Quaternary"
# 
# mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Miocene, middle to upper"] <- "Miocene-Eocene"
# mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Miocene-Oligocene"] <- "Miocene-Eocene"
# mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Oligocene-Eocene"] <- "Miocene-Eocene"
# mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Eocene"] <- "Miocene-Eocene"
# mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Quaternary"] <- "Holocene-Quaternary-Present"
# mas_geo_crop$GEOLOGIC_A[mas_geo_crop$GEOLOGIC_A == "Present"] <- "Holocene-Quaternary-Present"

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




