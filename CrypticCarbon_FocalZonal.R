library(terra)
library(sf)
library(rgdal)
library(tidyterra)
library(tidyverse)
library(parallel)
library(spatialEco)
library(landscapemetrics)
library(leaflet)
library(tmap)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")

########################################################################################################################################################################################################################
#### Let's make a majority filter mask here to get wetlands as objects ####
########################################################################################################################################################################################################################
hoh_WIP_mask <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
hoh_C_mask0 <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16mask0.tif")
GEE_HOHTCCproj <- rast("AGB/HOH/GEE_hohTCC.tif_proj.tif")


# m <- c(0, 0.5, 1,
#        0.5, 1, 2)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# WIP either Wetland or not
#hoh_WIP_bin <- classify(hoh_WIP_mask, rclmat, include.lowest = T, right = F, filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary.tif", overwrite = T)
hoh_WIP_bin <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary.tif")
plot(hoh_WIP_bin)

#use a moving window to condense continuous WIP binary to patches of individual wetlands 
# 5 seems to work best between 3 & 7, also need to omit NAs to not add area
#hoh_WIP_maj <- focal(hoh_WIP_bin, w = 5, fun = "modal", na.policy = "omit", na.rm = T, filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority.tif", overwrite = T)
#foc<- focal(hoh_WIP_bin, w = 5, fun = "modal", na.policy = "omit", na.rm = T, filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_foc_NA.tif", overwrite = T) 
hoh_WIP_maj3 <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority3.tif")#focal(hoh_WIP_bin, w = 3, fun = "modal", filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority3.tif", overwrite = T)
plot(hoh_WIP_maj3)
#hoh_WIP_mean <- focal(hoh_WIP_bin, w = 7, fun = "mean", filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_mean.tif", overwrite = T)
#hoh_WIP_max <- focal(hoh_WIP_bin, w = 7, fun = "max", filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_max.tif", overwrite = T)
#hoh_WIP_min <- focal(hoh_WIP_bin, w = 7, fun = "min", filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_min.tif", overwrite = T)

#bring in as a SpatRaster object
hoh_WIP_maj <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority.tif")
plot(hoh_WIP_maj, main = "5x5 window")# I think this is the one to use
plot(hoh_WIP_maj3, main = "3x3 window")


# also make a cellSize layer for area in polygons
#hoh_WIP_maj_vect_area <- cellSize(hoh_WIP_maj, mask = T)

#plot(hoh_WIP_maj3, main = "3x3 window")
#plot(hoh_WIP_mean)
#plot(hoh_WIP_max)
#plot(hoh_WIP_min)


#### Make majority filtered binary WIP raster into polygons ####
get_polys <- function(raster){
    vect_polys <- as.polygons(raster)
    sfpolys <- st_as_sf(vect_polys)
    sfcast <- sfpolys |> st_cast("POLYGON") 
    sfcast <- sfcast |>
        mutate(area = as.numeric(st_area(sfcast)),
               area_ha = area*0.0001)
    final_polys <- vect(sfcast)
}
hoh_WIP_maj_sfcastvect <- get_polys(hoh_WIP_maj)
hoh_WIP_bin_sfcastvect <- get_polys(hoh_WIP_bin)
plot(hoh_WIP_bin_sfcastvect)

# hoh_WIP_maj_vect <- as.polygons(hoh_WIP_maj)
# hoh_WIP_maj_sf <- st_as_sf(hoh_WIP_maj_vect)
# hoh_WIP_maj_sfcast <- hoh_WIP_maj_sf |> 
#     st_cast("POLYGON")
# hoh_WIP_maj_sfcast <- hoh_WIP_maj_sfcast |>
#     mutate(area = as.numeric(st_area(hoh_WIP_maj_sfcast)),
#            area_ha = area*0.0001)
# hoh_WIP_maj_sfcastvect <- vect(hoh_WIP_maj_sfcast)

hoh_WIP_maj_sfcastvect <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_sfcastvect.gpkg")
plot(hoh_WIP_maj_sfcastvect)





# #No filtering using original raster
# hoh_WIP_bin_poly <- as.polygons(hoh_WIP_bin)
# hoh_WIP_bin_poly_sfcast <- hoh_WIP_bin_poly |> 
#     st_as_sf() |>
#     st_cast("POLYGON")
# hoh_WIP_bin_poly_sfcast <- hoh_WIP_bin_poly_sfcast |> 
#     mutate(area = as.numeric(st_area(hoh_WIP_bin_poly_sfcast)),
#            area_ha = area*0.0001)
# ex <- c(397704.064206046, 401182.136902581, 5285243.88837961, 5287127.84442357)

#hoh_WIP_maj_vect_holes <- fillHoles(hoh_WIP_maj_vect)
#plot(hoh_WIP_maj_vect_holes)
#writeVector(hoh_WIP_maj_sfcastvect, filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority5castvect.shp", overwrite = T)


# reading in the polygons with the sf package
# f <- st_read("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority3.shp")
# 
# #Casting creates a polygon type vs. multipolygon type and we can sort the different sizes here
# f_cast <- st_cast(f, "POLYGON")
# #f_cast$area <- st_area(f_cast) #calculating area for each polygon
# f_cast <- f_cast |>
#     mutate(area = st_area(f_cast)) |>
#     mutate(area_ha = area*0.0001)
#write as a geopackage because it's easier to work with in R
#sf::st_write(f_cast, "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority5_polygons.gpkg", append = F)
#read back in 
#f_cast <- st_read("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority5_polygons.gpkg")
#hoh_WIP_maj_polys <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_binary_majority5_polygons.gpkg")

#plot(hoh_WIP_maj_polys) #takes a while
#plot(hoh_WIP_maj_polys['focal_moda'])

#convert to tibble for ease of checking data
hoh_WIP_maj_polys_tib <-  hoh_WIP_maj_sfcastvect |> as_tibble() |>
    filter(focal_modal == 2)

#Need to make a cutoff value and find actual quantiles for patch sizes
(hoh_WIP_maj_polys_hist <- hoh_WIP_maj_polys_tib |>
        # Target map unit for NWI is usually 1 acre = 4047m2 but they can map below 
        filter(focal_modal == 2, area >= 64 & area < 4047) |>
        dplyr::reframe(quantile = scales::percent(c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99)),
                       areaper = quantile(area, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99))))
length(hoh_WIP_maj_polys_tib$focal_modal)
hist(hoh_WIP_maj_polys_tib$area, breaks = 100)

#no filter binary
hoh_WIP_bin_polys_tib <-  hoh_WIP_bin_sfcastvect |> as_tibble() |>
    filter(WET == 2)

#Need to make a cutoff value and find actual quantiles for patch sizes
(hoh_WIP_bin_polys_hist <- hoh_WIP_bin_polys_tib |>
    # Target map unit for NWI is usually 1 acre = 4047m2 but they can map below 
    filter(WET == 2, area >= 64) |>
    dplyr::reframe(quantile = scales::percent(c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99)),
                     areaper = quantile(area, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99))))
length(hoh_WIP_bin_polys_tib$WET)
hist(hoh_WIP_bin_polys_tib$area, breaks = 100000)

# back to polygons - take out the ones that are less 2x2pix (8x8m = 64m**2)
#probably want this in 
#### Function for size filtering
sizefilter <- function(polygons){
    
    polygons_filtered <- polygons |> filter(if_any(1) == 2,
                               if_any(2) >= 64) #|>#all wetlands above 64 m2, 64m2
    
    writeVector(polygons_filtered, filename = paste0("SOIL CARBON/CrypticCarbonMaps/", substitute(polygons),
                                                                   sub(".*vect","",substitute(polygons)), "", 
                                                                   '.gpkg'), overwrite = T)
    return(polygons_filtered)
}

hoh_WIP_bin_polys_ab64 <- vect("SOIL CARBON/CrypticCarbonMaps/hoh_WIP_bin_sfcastvect.gpkg")#sizefilter(hoh_WIP_bin_sfcastvect)
hist(hoh_WIP_bin_polys_ab64$area, breaks = 1000)

# hoh_WIP_maj_polys_ab64 <- hoh_WIP_maj_sfcastvect |> dplyr::filter(focal_modal == 2,
#                                                              area >= 64) |>#all wetlands above 64 m2, 64m2
#     writeVector("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_polys_ab64.gpkg", overwrite = T)
# hoh_WIP_maj_polys_ab64_bel4047 <- hoh_WIP_maj_sfcastvect |> dplyr::filter(focal_modal == 2,
#                                                                      area >= 64 & area <4047) |> # all wetlands above 64 and below 4047 m2
#     writeVector("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_polys_ab64_bel4047.gpkg", overwrite = T)
# hoh_WIP_maj_polys_ab4047 <- hoh_WIP_maj_sfcastvect |> dplyr::filter(focal_modal == 2,
#                                                                area > 4047) |># all wetlands above 4047
#     writeVector("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_polys_ab4047.gpkg", overwrite = T)

#### read the wetland majority polys back in as terra SpatVector objects ####
hoh_WIP_maj_polys_ab64 <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_polys_ab64.gpkg")
hoh_WIP_maj_polys_ab64_bel4047 <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_polys_ab64_bel4047.gpkg")
hoh_WIP_maj_polys_ab4047 <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_maj_polys_ab4047.gpkg")

hist(hoh_WIP_maj_polys_ab64_bel4047$area, breaks = 1000)


#### These are the WIP binary thresholds that include NWI ####
#This might replace the SOC numbers in the manuscript that were not filtered ################################ 

# # This is the above 64m2 WIP wetlands that should have aggregated based on mode of >0.50 pixels in 5x5 window
#CC_WIPmaj_mask64 <- mask(hoh_C_mask0, hoh_WIP_maj_polys_ab64, touches = F, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_mask.tif", overwrite = T)
#CC_WIPmaj_mask64_upl <- mask(hoh_C_mask0, hoh_WIP_maj_polys_ab64, inverse = T, touches = F, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_mask_upl.tif", overwrite = T)
CC_WIPmaj_mask64_upl <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_mask_upl.tif")
CC_WIPmaj_mask64 <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_mask.tif")
#plot(CC_WIPmaj_mask64_upl)
# 
# # This is the above 64m2 and below 4047m2 WIP wetlands that should have aggregated based on mode of >0.50 pixels in 5x5 window
#CC_WIPmaj_mask64_4047 <- mask(hoh_C_mask0, hoh_WIP_maj_polys_ab64_bel4047, touches = F, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_bel4047_mask.tif", overwrite = T)
#CC_WIPmaj_mask64_4047_upl <- mask(hoh_C_mask0, hoh_WIP_maj_polys_ab64_bel4047, inverse = T, touches = F)
CC_WIPmaj_mask64_4047_upl <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_bel4047_mask_upl.tif")
CC_WIPmaj_mask64_4047<- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab64_bel4047_mask.tif")
plot(CC_WIPmaj_mask64_4047_upl)
# 
# # This is the above 4047m2 WIP wetlands that should have aggregated based on mode of >0.50 pixels in 5x5 window
#CC_WIPmaj_mask4047 <- mask(hoh_C_mask0, hoh_WIP_maj_polys_ab4047, touches = F,filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab4047_mask.tif", overwrite = T)
#CC_WIPmaj_mask4047_upl <- mask(hoh_C_mask0, hoh_WIP_maj_polys_ab4047, touches = F, inverse = T)#, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab4047_mask_upl.tif", overwrite = T)
CC_WIPmaj_mask4047_upl <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab4047_mask_upl.tif")
CC_WIPmaj_mask4047 <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPab4047_mask.tif")
plot(CC_WIPmaj_mask4047)

# #This is the forest canopy cover mask to find forested wetlands and put SOC in them ###################################################################
# CC_WIPmaj_AGB50_mask <- mask(CC_WIPmaj_mask, (GEE_HOHTCCproj > 50), maskvalues = FALSE, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPmaj_TCC_mask.tif", overwrite = T)
# plot(CC_WIPmaj_AGB50_mask)


#### Non filtered WIP THIS IS THE ONE TO USE NOT MAJORITY FILTERED####
#CC_WIPbin_mask64 <- mask(hoh_C_mask0, hoh_WIP_bin_polys_ab64, touches = F,filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIP_bin_ab64_mask.tif", overwrite = T)
CC_WIPbin_mask64 <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIP_bin_ab64_mask.tif")
plot(CC_WIPbin_mask64)

####function for masking the Hoh SOC map with filtered majority polygons and the Total Canopy Cover layer ####
# . Period, matches a single character of any single character, except the end of a line.
# * Asterisk, matches 0 or more characters in front of the asterisk.
#SOC_AGB <- function (HOH_SOC, NWI, MAJ_POLYS, TCC) {
# CC_NWI_mask <- terra::mask(HOH_SOC, NWI, inverse = T,
#                            filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#                                              sub(".*polys","",substitute(MAJ_POLYS)), "_NWI", 
#                                              '.tif'),
#                            overwrite = T)
# plot(CC_NWI_mask)
# print("check1")
# CC_mask <- terra::mask(CC_NWI_mask, MAJ_POLYS, 
#                        filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#                                          sub(".*polys","",substitute(MAJ_POLYS)), 
#                                          '.tif'),
#                        overwrite = T)
# plot(CC_mask)
# print("check2")
# TCC_mask50 <- terra::mask(CC_mask, (TCC > 50), 
#                           maskvalues = FALSE, 
#                           filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#                                             sub(".*polys","",substitute(MAJ_POLYS)), "_NWI_TCC50", '.tif'), 
#                           overwrite = T)
# plot(TCC_mask50)
# print("check3")
# TCC_mask60 <- terra::mask(CC_mask, (TCC > 60), 
#                           maskvalues = FALSE, 
#                           filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#                                             sub(".*polys","",substitute(MAJ_POLYS)), "_NWI_TCC60", '.tif'), 
#                           overwrite = T)
# plot(TCC_mask60)
# print("check4")
# TCC_mask70 <- terra::mask(CC_mask, (TCC > 70), 
#                           maskvalues = FALSE, 
#                           filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#                                             sub(".*polys","",substitute(MAJ_POLYS)), "_NWI_TCC70", '.tif'), 
#                           overwrite = T)
# plot(TCC_mask70)
# print("check5")
# 
# cat(paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#            sub(".*polys","",substitute(MAJ_POLYS)), 
#            '.tif'), "\n",
#     paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#            sub(".*polys","",substitute(MAJ_POLYS)), "_NWI", 
#            '.tif'),"\n",
#     paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#            sub(".*polys","",substitute(MAJ_POLYS)), "_NWI_TCC50", '.tif'), "\n",
#     
#     paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#            sub(".*polys","",substitute(MAJ_POLYS)), "_NWI_TCC60", '.tif'), "\n",
#     
#     paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_", 
#            sub(".*polys","",substitute(MAJ_POLYS)), "_NWI_TCC70", '.tif'), "\n")
# #}


#### NWI masking of the hoh SOC ####
#for some reason masking does not write the correct raster
hoh_NWI_rpj <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP_RPJ.shp")
#CC_inNWI <-terra::mask(hoh_C_mask0, hoh_NWI_rpj, inverse = F, filename = "SOIL CARBON/CrypticCarbonMaps/SOC_inNWI.tif", overwrite = T) #SOC in NWI wetlands
CC_inNWI <- rast("SOIL CARBON/CrypticCarbonMaps/SOC_inNWI.tif")
#writeRaster(CC_NWI, filename = "SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_MAJ_POLYS_NWI.tif", overwrite = T)
CC_outNWI <- rast("SOIL CARBON/CrypticCarbonMaps/SOC_outNWI.tif")  #minus the NWI from SOC map
plot(CC_outNWI)


# CC_outNWI |>
#          terra::mask(hoh_WIP_bin_polys_ab64, touches = F,
#                     filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_",
#                                       sub(".*polys","",substitute(hoh_WIP_bin_polys_ab64)), "noNWI",
#                                       '.tif'),
#                     overwrite = T) |>
#         terra::mask((GEE_HOHTCCproj > 50),
#                     maskvalues = FALSE,
#                     filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_",
#                                       sub(".*polys","",substitute(hoh_WIP_bin_polys_ab64)), "_noNWI_TCC50", '.tif'),
#                     overwrite = T) |>
#         terra::mask((GEE_HOHTCCproj > 60),
#                     maskvalues = FALSE,
#                     filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_",
#                                       sub(".*polys","",substitute(hoh_WIP_bin_polys_ab64)), "_noNWI_TCC60", '.tif'),
#                     overwrite = T) |>
#         terra::mask((GEE_HOHTCCproj > 70),
#                     maskvalues = FALSE,
#                     filename = paste0("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_",
#                                       sub(".*polys","",substitute(hoh_WIP_bin_polys_ab64)), "_noNWI_TCC70", '.tif'),
#                     overwrite = T)

CC_WIPmaj_mask_noNWI_ab64 <- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64.tif")
CC_WIPmaj_mask_noNWI_ab64_bel4047 <- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64_bel4047.tif")
CC_WIPmaj_mask_noNWI_ab4047 <- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab4047.tif")

#### No majority filter WIP and no NWI overlap/NWI removed ####
CC_WIPbin_mask64_noNWI <- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPbin_mask__ab64noNWI.tif")
plot(CC_WIPbin_mask64_noNWI)
#### These are all the WIP filtered majority 5x5 wetlands with more than 50% forest cover and their SOC ####
#SOC_AGB(hoh_C_mask0, hoh_NWI_rpj, hoh_WIP_maj_polys_ab64, GEE_HOHTCCproj)
#SOC_AGB(hoh_C_mask0, hoh_NWI_rpj, hoh_WIP_maj_polys_ab64_bel4047, GEE_HOHTCCproj)
#SOC_AGB(hoh_C_mask0, hoh_NWI_rpj, hoh_WIP_maj_polys_ab4047, GEE_HOHTCCproj)

# Bring them back as objects?
#define new function without writing to memory
C_map_simp_nowrt <- function(C_map){
    gt <- (C_map > -999)
    area_tot <- sum(values(cellSize(gt, unit = "ha", mask = T)), na.rm = T)
    C_mean <- mean(values(C_map), na.rm = T) #mean value of all values of Mg/ha cells
    carbon_cell <- C_map*cellSize(gt, unit = "ha", mask = T) # carbon in Mg per cell which is then added up 
    #There will be small numbers here because of Mg/ha
    TotalC_sum <- sum(values(carbon_cell), na.rm =T)
    #writeRaster(C_map, paste0("SOIL CARBON/CrypticCarbonMaps/", (deparse(substitute(C_map))), '.tif'), overwrite = T) #"SOIL CARBON/CC_FULL_noRIV_NWI_WIP_mask.tif"
    cat("Total area =", area_tot, 
        "\nAverage Carbon Stock (Mg/ha) =", C_mean, 
        "\ntotal Carbon (Tg) =", TotalC_sum/1e6)
}

C_map_simp_nowrt(CC_inNWI) # The SOC and area of the NWI 
#C_map_simp_nowrt(CC_WIPmaj_mask_noNWI_ab64) #taken from the outNWI map so no NWI overlap
#C_map_simp_nowrt(CC_WIPmaj_mask64) #should have NWI overlap

#### No filtered WIP ####
C_map_simp_nowrt(CC_WIPbin_mask64)
C_map_simp_nowrt(CC_WIPbin_mask64_noNWI)

# CC_WIPmaj_mask__ab64<- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64.tif") # WIP filtered minus NWI 
# CC_WIPmaj_mask__ab64_NWI<- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask_MAJ_POLYS_NWI.tif")
# CC_WIPmaj_mask__ab64_NWI_TCC50<- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64_NWI_TCC50.tif")
# CC_WIPmaj_mask__ab64_NWI_TCC60 <- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64_NWI_TCC60.tif ")
# CC_WIPmaj_mask__ab64_NWI_TCC70<- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64_NWI_TCC70.tif ")

#### Apply the SOC stock calculation ####
#First with all WIP filtered 5x5 that includes NWI
#C_map_simp_nowrt(CC_WIPmaj_mask64) #should be the highest area and SOC 
# C_map_simp_nowrt(CC_WIPmaj_mask64_4047) #should be really small
# C_map_simp_nowrt(CC_WIPmaj_mask4047) # Should be slightly lower than 64
# #Now do uplands of these wetland filters 
# C_map_simp_nowrt(CC_WIPmaj_mask64_upl) #should be the lowest area and SOC 
# C_map_simp_nowrt(CC_WIPmaj_mask64_4047_upl) #should be really large
# C_map_simp_nowrt(CC_WIPmaj_mask4047_upl) # Should be slightly higher than 64
# 
# 
# C_map_simp_nowrt(CC_WIPmaj_mask64)
# C_map_simp_nowrt(CC_WIPmaj_mask__ab64) # WIP filtered minus NWI 
# #C_map_simp_nowrt(CC_WIPmaj_mask__ab64_NWI)
# C_map_simp_nowrt(CC_WIPmaj_mask__ab64_NWI_TCC50)
# C_map_simp_nowrt(CC_WIPmaj_mask__ab64_NWI_TCC60)
# C_map_simp_nowrt(CC_WIPmaj_mask__ab64_NWI_TCC70)
# 


#### Will need to repeat with the SOC Uncertainty Layers ########################################################################

#cool it works!
# CC_WIPmaj_mask64_AGB50_mask  <- rast("SOIL CARBON/CrypticCarbonMaps/CC_WIPmaj_mask__ab64.tif")
# plot(CC_WIPmaj_mask64_AGB50_mask)


# #This is the NWI mask to remove the NWI wetlands 
# CC_WIPmaj_AGB50_NWI_mask <- mask(CC_WIPmaj_AGB50_mask, hoh_NWI_rpj, inverse = T, filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPmaj_TCC_NWI_mask.tif", overwrite = T)
# plot(CC_WIPmaj_AGB50_NWI_mask, main = "majority and FCC filtered mean SOC")
#This is the SOC uncertainty interval difference in the wetlands Majority filter, >50% Forest Canopy, Outside of NWI Wetland Carbon
# CC_WIPmaj_AGB50_NWI_mask_uncert <- mask(CC_intdiff, (CC_WIPmaj_AGB50_NWI_mask > -999), maskvalues = NA,  filename = "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_WIPmaj_TCC_NWI_mask_uncert.tif", overwrite = T)
# plot(CC_WIPmaj_AGB50_NWI_mask_uncert, main = "majority and FCC filtered SOC uncertainty")


#### Zonal stats using extract on WIP majority filters ####
    #### above 64m2 ####
# hoh_WIP_bin_polys_ab64_zonext <- hoh_C_mask0 |>
#     terra::extract(hoh_WIP_bin_polys_ab64, fun = "mean",  touches = F, na.rm = T, bind = T) |>
#     rename(extract_mean_SOC = lyr1)
# 
# hoh_WIP_bin_polys_ab64_zonext <- hoh_WIP_bin_polys_ab64_zonext |> 
#     mutate(extract_stock_SOC = area_ha*extract_mean_SOC) #|>
# writeVector(hoh_WIP_bin_polys_ab64_zonext, filename = "SOIL CARBON/CrypticCarbonMaps/hoh_WIP_bin_polys_ab64_zonext.gpkg", overwrite = T)
hoh_WIP_bin_polys_ab64_zonext <- vect("SOIL CARBON/CrypticCarbonMaps/hoh_WIP_bin_polys_ab64_zonext.gpkg")

sum(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC, na.rm = T)/1000000 #This doesn't reflect the exclusion of the NWI since we used the full SOC map
hist(sqrt(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC), breaks = 1000)
plot(log((hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC)), hoh_WIP_bin_polys_ab64_zonext$area_ha)

#### NWI polygon removal ####
# This is after I extracted all the carbon values so I can go straight to calculations with re-extracting
# NWI_simp <- hoh_NWI_rpj[,5]
#hoh_WIP_bin_polys_ab64_zonext_noNWI <- erase(hoh_WIP_bin_polys_ab64_zonext, NWI_simp) #takes a long time 
writeVector(hoh_WIP_bin_polys_ab64_zonext_noNWI, filename = "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_bin_polys_ab64_zonext_noNWI.gpkg", overwrite = T)
hoh_WIP_bin_polys_ab64_zonext_noNWI <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Specifying Wetlands/hoh_WIP_bin_polys_ab64_zonext_noNWI.gpkg")
# #### NWI re-calculate area and distribution ####
# NWI_simp$WET <- 2
# 
# NWIconvert <- function(NWI){
#     sfpolys <- st_as_sf(NWI)
#     sfcast <- sfpolys |> st_cast("POLYGON") 
#     sfcast <- sfcast |>
#     mutate(area = as.numeric(st_area(sfcast)),
#            area_ha = area*0.0001)
#     final_polys <- vect(sfcast)
# }
# NWI_recalc <- NWIconvert(NWI_simp)
# NWI_recalc|> filter(is.na(area)) |> as_tibble()
# NWI_simp_filt <- sizefilter(NWI_recalc)

NWI_simp_filt <- vect("SOIL CARBON/CrypticCarbonMaps/NWI_recalcNWI_recalc.gpkg")
#### quantiles, maybe check out cut function for calculations
NWI_simp_filt |> as_tibble() |>
        # Target map unit for NWI is usually 1 acre = 4047m2 but they can map below 
        dplyr::reframe(quantile = scales::percent(c(0.01, 0.25, 0.5, 0.75, 0.80, 0.96, 0.9999, 0.99999999)),
                       area_ha_per =   quantile(area_ha, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.96426, 0.9999, 0.99999999), na.rm= T))
                       #extract_stock_SOCper = quantile(extract_stock_SOC, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.96426, 0.9999, 0.99999999), na.rm= T),
                       #extract_mean_SOCper = quantile(extract_mean_SOC, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.96426, 0.9999, 0.99999999), na.rm= T)) |>
        #mutate(percent_total_SOC = scales::percent(extract_stock_SOCper/(sum(hoh_WIP_maj_polys_ab64_zonext$extract_stock_SOC, na.rm = T))) ))

percentileNWI <- ecdf(NWI_simp_filt$area_ha)
Acre_percentile_noNWI <- percentileNWI(0.4047)


#NWI zonal stats
# NWI_simp_filt <- hoh_C_mask0 |>
#     terra::extract(NWI_simp_filt, fun = "mean",  touches = F, na.rm = T, bind = T) |>
#     rename(extract_mean_SOC = lyr1)
# 
# NWI_simp_filt <- NWI_simp_filt |> 
#     filter( extract_mean_SOC != is.na(extract_mean_SOC)) |>
#     mutate(extract_stock_SOC = area_ha*extract_mean_SOC)
# writeVector(NWI_simp_filt, filename = "SOIL CARBON/CrypticCarbonMaps/NWI_simp_filt_SOC.gpkg", overwrite = T)
NWI_simp_filt <- vect("SOIL CARBON/CrypticCarbonMaps/NWI_simp_filt_SOC.gpkg")

sum(NWI_simp_filt$extract_stock_SOC, na.rm = T)/1000000 #This doesn't reflect the exclusion of the NWI since we used the full SOC map
hist(sqrt(NWI_simp_filt$extract_stock_SOC), breaks = 1000)


#     #### above 64m2 below 4047m2 ####
# hoh_WIP_maj_polys_ab64_bel4047zonext <- hoh_C_mask0 |>
#     terra::extract(hoh_WIP_maj_polys_ab64_bel4047, fun = "mean",  touches = F, na.rm = T, bind = T) |>
#     rename(extract_mean_SOC = lyr1)
# 
# hoh_WIP_maj_polys_ab64_bel4047zonext <- hoh_WIP_maj_polys_ab64_bel4047zonext |> 
#     mutate(extract_stock_SOC = area_ha*extract_mean_SOC)
# 
# sum(hoh_WIP_maj_polys_ab64_bel4047zonext$extract_stock_SOC, na.rm = T)/1000000
# hist((hoh_WIP_maj_polys_ab64_bel4047zonext$extract_stock_SOC), breaks = 1000)
# 
# (hoh_WIP_maj_polys_ab64_bel4047zonext |> as_tibble() |>
#         # Target map unit for NWI is usually 1 acre = 4047m2 but they can map below 
#         dplyr::reframe(quantile = scales::percent(c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999)),
#                        area_haper =   quantile(area_ha, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999), na.rm= T),
#                        extract_stock_SOCper = quantile(extract_stock_SOC, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999), na.rm= T),
#                        extract_mean_SOCper = quantile(extract_mean_SOC, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999), na.rm= T)) |>
#         mutate(percent_total_SOC = scales::percent(extract_stock_SOCper/(sum(hoh_WIP_maj_polys_ab64_bel4047zonext$extract_stock_SOC, na.rm = T))) )
# )
# 
#     #### above 4047m2 ####
# hoh_WIP_maj_polys_ab4047zonext <- hoh_C_mask0 |>
#     terra::extract(hoh_WIP_maj_polys_ab4047, fun = "mean",  touches = T, na.rm = T, bind = T) |>
#     rename(extract_mean_SOC = lyr1)
# 
# hoh_WIP_maj_polys_ab4047zonext <- hoh_WIP_maj_polys_ab4047zonext |> 
#     mutate(extract_stock_SOC = area_ha*extract_mean_SOC)
# 
# sum(hoh_WIP_maj_polys_ab4047zonext$extract_stock_SOC, na.rm = T)/1000000 #sum of total SOC 
# hist((hoh_WIP_maj_polys_ab4047zonext$extract_stock_SOC), breaks = 1000) #histogram of SOC stock 
# 
# (hoh_WIP_maj_polys_ab4047zonext |> as_tibble() |>
#         # Target map unit for NWI is usually 1 acre = 4047m2 but they can map below 
#         dplyr::reframe(quantile = scales::percent(c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999)),
#                        area_haper =   quantile(area_ha, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999), na.rm= T),
#                        extract_stock_SOCper = quantile(extract_stock_SOC, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999), na.rm= T),
#                        extract_mean_SOCper = quantile(extract_mean_SOC, c(0.01, 0.25, 0.5, 0.75, 0.80, 0.99, 0.9999), na.rm= T)) |>
#         mutate(percent_total_SOC = scales::percent(extract_stock_SOCper/(sum(hoh_WIP_maj_polys_ab4047zonext$extract_stock_SOC, na.rm = T))) )
# )

#### Check totals between polys and raster approaches ####

C_map_simp_nowrt(CC_WIPmaj_mask64) #polys area, mean SOC conc, total SOC Tg 
sum(hoh_WIP_bin_polys_ab64_zonext_noNWI$extract_stock_SOC, na.rm = T)/1000000 #Zonal/Extract SOC total Tg
sum(hoh_WIP_bin_polys_ab64_zonext_noNWI$area_ha, na.rm = T) #zonal/extract area ha 

# library(poweRlaw)
# dat <- as_tibble(hoh_WIP_bin_polys_ab64_zonext_noNWI)
# dat$seq <- seq(1, length(dat$WET), 1)
# plot(cumsum(dat$extract_stock_SOC), dat$seq)
# hist(log(dat$extract_stock_SOC))
# plot(cumsum(hoh_WIP_bin_polys_ab64_zonext_noNWI$area_ha))
# wetlands <- displ$new(cumsum(floor(hoh_WIP_bin_polys_ab64_zonext_noNWI$area)))
# plot(wetlands)
#They're pretty close 
# Total area = 5755.864 
# Average Carbon Stock (Mg/ha) = 275.4858 
# total Carbon (Tg) = 1.585652
# #
# > sum(hoh_WIP_maj_polys_ab64_zonext$extract_stock_SOC, na.rm = T)/1000000 #Zonal/Extract SOC total Tg
# [1] 1.584739
# > sum(hoh_WIP_maj_polys_ab64_zonext$area_ha, na.rm = T) #zonal/extract area ha 
# [1] 5752.523

############################################################################################################
#### SOC No NWI totals by area percentile ####
############################################################################################################
leaflet() |> addTiles() |> 
    addPolygons(data = terra::project(hoh_WIP_bin_polys_ab64_zonext_noNWI, "EPSG:4326"), color = "red") |>
    addPolygons(data = terra::project(hoh_WIP_bin_polys_ab64_zonext, "EPSG:4326"), color = "blue")

percentilenoNWI <- ecdf(hoh_WIP_bin_polys_ab64_zonext_noNWI$area_ha)
Acre_percentile_noNWI <- percentilenoNWI(0.4047)

SOC64_NoNWI <- hoh_WIP_bin_polys_ab64_zonext_noNWI |> as_tibble() |>
    # mutate(bins = cut(hoh_WIP_bin_polys_ab64_zonext_noNWI$area, breaks = quantile(area, probs = c(0.01, 0.25, 0.5, 0.75, 0.96426, 0.999, 1)), labels = F, include.lowest = T),
    mutate(bins = gtools::quantcut(area_ha, q = c(0, 0.05, 0.25, 0.5, 0.75, Acre_percentile_noNWI, 1)),
           quantiles = case_when(bins == "0.0064" ~ "0-5%",
                                 bins == "(0.0064,0.008]" ~ "5-25%",
                                 bins == "(0.008,0.0112]" ~ "25-50%",
                                 bins == "(0.0112,0.0256]" ~ "50-75%",
                                 bins == "(0.0256,0.405]" ~ paste0("75-", round(Acre_percentile_noNWI, 2), "%"),
                                 bins == "(0.405,401]" ~ paste0(round(Acre_percentile_noNWI, 2),"-100%"))) |> 
                                 #bins == 7 ~ "96.426-99.999%",
                                 #.default = "100%")) |>
    dplyr::group_by(quantiles) |>
    #arrange(match(quantiles, c("0-5%","5-25%","25-50%","50-75%","75-96.4%","96.4- 100%"))) |>
    dplyr::reframe(SOC_stock_sumTg = sum(extract_stock_SOC)/1e6,
                   SOC_stock_meanMg = mean(extract_mean_SOC),
                   SOC_stock_stderrMg = sd(extract_mean_SOC)/sqrt(length((area_ha))),
                   area_count = length((area_ha)),
                   area_sum =  sum(round(area_ha, 4)),
                   area_max = max(round(area_ha, 4)),
                   area_min = min(round(area_ha, 4)),
                   area_mean = mean(area_ha),
                   area_median = median(area_ha),
                   bins = first(bins)) |>
    arrange(match(quantiles, c("0-5%","5-25%","25-50%","50-75%", paste0("75-", round(Acre_percentile_noNWI, 2), "%"), paste0(round(Acre_percentile_noNWI, 2),"-100%")))) |>
    mutate(cumsumSOC = cumsum(SOC_stock_sumTg),
           cumsumArea = cumsum(area_sum),
           cumsumCount = cumsum(area_count)) |>
    
    unite("range", area_min:area_max, remove = F, sep = "-")

sum(SOC64_NoNWI$area_sum[1:length(SOC64_NoNWI$area_sum)-1])/sum(SOC64_NoNWI$area_sum) # 13% of the area is made up of wetlands smaller than 1 acre
sum(SOC64_NoNWI$SOC_stock_sumTg[1:length(SOC64_NoNWI$area_sum)-1])/sum(SOC64_NoNWI$SOC_stock_sumTg) # 13% of the SOC is in these wetlands
SOC64_NoNWI$area_sum[6]/sum(SOC64_NoNWI$area_sum) # 87% of the area is in the biggest wetlands
SOC64_NoNWI$SOC_stock_sumTg[6]/sum(SOC64_NoNWI$SOC_stock_sumTg) # 87% of the SOC is in the biggest wetlands

#cumsum
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC)/1e6)
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$area_ha))

# #pareto distribution
# library(fitdistrplus)
# library(actuar)
# fp <- fitdist(hoh_WIP_bin_polys_ab64_zonext$area_ha, "pareto", start=list(shape = 1, scale = 500))


#### SOC with NWI totals by area percentile ####
percentilewNWI <- ecdf(hoh_WIP_bin_polys_ab64_zonext$area_ha)
Acre_percentile_wNWI <- percentilewNWI(0.4047)

SOC64_wNWI <- hoh_WIP_bin_polys_ab64_zonext |> as_tibble() |>
    # mutate(bins = cut(hoh_WIP_bin_polys_ab64_zonext_noNWI$area, breaks = quantile(area, probs = c(0.01, 0.25, 0.5, 0.75, 0.96426, 0.999, 1)), labels = F, include.lowest = T),
    mutate(bins = gtools::quantcut(area_ha, q = c(0, 0.05, 0.25, 0.5, 0.75, Acre_percentile_wNWI, 1)),
           quantiles = case_when(bins == "0.0064"  ~ "0-5%",
                                 bins == "(0.0064,0.008]" ~ "5-25%",
                                 bins == "(0.008,0.0112]" ~ "25-50%",
                                 bins == "(0.0112,0.0256]" ~ "50-75%",
                                 bins == "(0.0256,0.405]" ~ "75-96.4%",
                                 bins == "(0.405,401]"  ~ "96.4- 100%")) |> 
    #bins == 7 ~ "96.426-99.999%",
    #.default = "100%")) |>
    dplyr::group_by(quantiles) |>
    #arrange(match(quantiles, c("0-5%","5-25%","25-50%","50-75%","75-96.4%","96.4- 100%"))) |>
    dplyr::reframe(SOC_stock_sumTg = sum(extract_stock_SOC)/1e6,
                   SOC_stock_meanMg = mean(extract_mean_SOC),
                   SOC_stock_stderrMg = sd(extract_mean_SOC)/sqrt(length((area_ha))),
                   area_count = length((area_ha)),
                   area_sum =  sum(round(area_ha, 4)),
                   area_max = max(round(area_ha, 4)),
                   area_min = min(round(area_ha, 4)),
                   area_mean = mean(area_ha),
                   area_median = median(area_ha),
                   bins = first(bins)) |>
    arrange(match(quantiles, c("0-5%","5-25%","25-50%","50-75%","75-96.4%","96.4- 100%"))) |>
    mutate(cumsumSOC = cumsum(SOC_stock_sumTg),
           cumsumArea = cumsum(area_sum),
           cumsumCount = cumsum(area_count)) |>
   
    unite("range", area_min:area_max, remove = F, sep = "-")

sum(SOC64_wNWI$area_sum) # area total
sum(SOC64_wNWI$SOC_stock_sumTg) #SOC total
sum(SOC64_wNWI$area_sum[1:length(SOC64_wNWI$area_sum)-1])/sum(SOC64_wNWI["area_sum"]) # 14% of the area is made up of wetlands smaller than 1 acre
sum(SOC64_wNWI$SOC_stock_sumTg[1:length(SOC64_wNWI$area_sum)-1])/sum(SOC64_wNWI$SOC_stock_sumTg) # 13% of the SOC is in these wetlands
SOC64_wNWI$area_sum[6]/sum(SOC64_wNWI$area_sum) # 86% of the area is in the biggest wetlands
SOC64_wNWI$SOC_stock_sumTg[6]/sum(SOC64_wNWI$SOC_stock_sumTg) # 87% of the SOC is in the biggest wetlands



#cumsum
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC)/1e6)
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$area_ha))


# #pareto distribution
# library(fitdistrplus)
# library(actuar)
# fp <- fitdist(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC, "pareto", start=list(shape = 1, scale = 500))

#### NWI SOC totals by area percentile ####
#NWI_simp_filt |> filter(is.na(extract_stock_SOC)) |> as_tibble() # check if the NAs are present
# The NAs are removed due to being in the river

percentileNWI <- ecdf(NWI_simp_filt$area_ha)
Acre_percentile_NWI <- percentileNWI(0.4047)

NWISOC64 <- NWI_simp_filt |> as_tibble() |>
    # mutate(bins = cut(hoh_WIP_bin_polys_ab64_zonext_noNWI$area, breaks = quantile(area, probs = c(0.01, 0.25, 0.5, 0.75, 0.96426, 0.999, 1)), labels = F, include.lowest = T),
    mutate(bins = gtools::quantcut(area_ha, q = c(0, 0.05, 0.25, 0.5, Acre_percentile_NWI, 1)),
           quantiles = case_when(bins ==  "[0.00661,0.0337]"  ~ "0-5%",
                                 bins == "(0.0337,0.0812]" ~ "5-25%",
                                 bins == "(0.0812,0.158]" ~ "25-50%",
                                 #bins == "(0.158,0.405]" ~ "50-75%",
                                 bins ==  "(0.158,0.405]" ~ paste0("50-", round(Acre_percentile_NWI, 2), "%"),
                                 bins == "(0.405,589]"    ~ paste0(round(Acre_percentile_NWI, 2),"-100%"))) |> 
    #bins == 7 ~ "96.426-99.999%",
    #.default = "100%")) |>
    dplyr::group_by(quantiles) |>
    #\arrange(match(quantiles, c("0-5%","5-25%","25-50%","50-75%","75-96.4%","96.4- 100%"))) |>
    dplyr::reframe(SOC_stock_sumTg = sum(extract_stock_SOC)/1e6,
                   SOC_stock_meanMg = mean(extract_mean_SOC),
                   SOC_stock_stderrMg = sd(extract_mean_SOC)/sqrt(length((area_ha))),
                   area_count = length((area_ha)),
                   area_sum =  sum(round(area_ha, 4)),
                   area_max = max(round(area_ha, 4)),
                   area_min = min(round(area_ha, 4)),
                   area_mean = mean(area_ha),
                   area_median = median(area_ha),
                   bins = first(bins)) |>
    arrange(match(quantiles, c("<1%", "0-5%","5-25%","25-50%",paste0("50-", round(Acre_percentile_NWI, 2), "%"),paste0(round(Acre_percentile_NWI, 2),"-100%")))) |>
    mutate(cumsumSOC = cumsum(SOC_stock_sumTg),
           cumsumArea = cumsum(area_sum),
           cumsumCount = cumsum(area_count)) |>
    
    unite("range", area_min:area_max, remove = F, sep = "-")

sum(NWISOC64$area_sum[1:length(NWISOC64$area_sum)-1])/sum(NWISOC64["area_sum"]) # 13% of the area is made up of wetlands smaller than 1 acre
sum(NWISOC64$SOC_stock_sumTg[1:length(NWISOC64$area_sum)-1])/sum(NWISOC64$SOC_stock_sumTg) # 10% of the SOC is in these wetlands
NWISOC64$area_sum[length(NWISOC64$area_sum)]/sum(NWISOC64$area_sum) # 87% of the area is in the biggest wetlands
NWISOC64$SOC_stock_sumTg[length(NWISOC64$area_sum)]/sum(NWISOC64$SOC_stock_sumTg) # 90% of the SOC is in the biggest wetlands

#cumsum
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$extract_stock_SOC)/1e6)
plot(cumsum(hoh_WIP_bin_polys_ab64_zonext$area_ha))



# #pareto distribution
# library(fitdistrplus)
# library(actuar)
# fp <- fitdist(hoh_WIP_bin_polys_ab64_zonext$area_ha, "pareto", start=list(shape = 1, scale = 500))

#### Histogram ####
hoh_WIP_bin_polys_ab64_zonext_tib <- hoh_WIP_bin_polys_ab64_zonext |> as_tibble()

ggplot(SOC64_wNWI) +
    geom_col(aes(x = (quantiles), y = area_count)) +
    labs(title = "area count") + 
    theme(legend.position = 'right',
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))
ggplot(SOC64_wNWI) +
    geom_col(aes(x = (quantiles), y = (SOC_stock_sumTg))) +
    labs(title = "SOC_stock_sumTg") + 
    theme(legend.position = 'right',
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))






############# Rest of the size classes ############# ############# ############# ############# ############# #############
# SOC64_4047 <-hoh_WIP_maj_polys_ab64_bel4047zonext |> as_tibble() |>
#     mutate(bins = cut(hoh_WIP_maj_polys_ab64_bel4047zonext$area, breaks = quantile(area, c(0.01, .20, 0.25, 0.5, 0.75, 0.95, 1)), labels = F, include.lowest = T),
#            quantiles = case_when(bins == 1 ~ "1%",
#                                  bins == 2 ~ "10%",
#                                  bins == 3 ~ "25%",
#                                  bins == 4 ~ "50%",
#                                  bins == 5 ~ "75%",
#                                  bins == 6 ~ "95%")) |>
#     dplyr::group_by(quantiles) |> 
#     dplyr::reframe(SOC_stock_sumTg = sum(extract_stock_SOC)/1e6,
#                    SOC_stock_meanMg = mean(extract_mean_SOC),
#                    SOC_stock_stderrMg = sd(extract_mean_SOC)/sqrt(length((area_ha))),
#                    area_count = length((area_ha)),
#                    area_sum =  sum(round(area_ha, 4)),
#                    area_max = max(round(area_ha, 4)),
#                    area_min = min(round(area_ha, 4)),
#                    area_mean = mean(area_ha),
#                    area_median = median(area_ha)) |>
#     unite("range", area_min:area_max, remove = F, sep = "-")
# 
# SOC4047 <-hoh_WIP_maj_polys_ab4047zonext |> as_tibble() |>
#     mutate(bins = cut(hoh_WIP_maj_polys_ab4047zonext$area, breaks = quantile(area, probs = c(0.01, .20, 0.25, 0.5, 0.75, 0.95, 1)), labels = F, include.lowest = T),
#            quantiles = case_when(bins == 1 ~ "1%",
#                                  bins == 2 ~ "10%",
#                                  bins == 3 ~ "25%",
#                                  bins == 4 ~ "50%",
#                                  bins == 5 ~ "75%",
#                                  bins == 6 ~ "95%")) |>
#     dplyr::group_by(quantiles) |> 
#     dplyr::reframe(SOC_stock_sumTg = sum(extract_stock_SOC)/1e6,
#                    SOC_stock_meanMg = mean(extract_mean_SOC),
#                    SOC_stock_stderrMg = sd(extract_mean_SOC)/sqrt(length((area_ha))),
#                    area_count = length((area_ha)),
#                    area_sum =  sum(round(area_ha, 4)),
#                    area_max = max(round(area_ha, 4)),
#                    area_min = min(round(area_ha, 4)),
#                    area_mean = mean(area_ha),
#                    area_median = median(area_ha)) |>
#     unite("range", area_min:area_max, remove = F, sep = "-")
# 
# (SOC_zonext <- SOC64 |> full_join(SOC64_4047, by = "quantiles", suffix = c("_64", "_64_4047")) |>
#     full_join(SOC4047,  by = "quantiles") |>
#     rename(SOC_stock_sumTg_4047 = SOC_stock_sumTg,
#            SOC_stock_meanMg_4047 = SOC_stock_meanMg,
#            SOC_stock_stderrMg_4047 = SOC_stock_stderrMg,
#            area_sum_4047 = area_sum,
#            area_max_4047 = area_max,
#            area_min_4047 = area_min,
#            area_count_4047 = area_count,
#            area_mean_4047 = area_mean,
#            area_median_4047 = area_median,
#            range_4047 = range) |>
#     mutate(SOC64_perc = SOC_stock_sumTg_64/sum(SOC_stock_sumTg_64),
#            area64_perc = area_sum_64/sum(area_sum_64)))
# colnames(SOC_zonext)
#probably need to select specific columns to plot with dplyr::select then pivot to get columns side by side
(piv <- SOC_zonext |> select(c("quantiles", "SOC64_perc", "area64_perc")) |>
    tidyr::pivot_longer(-quantiles, values_to =  "SOCandArea") |> 
    filter(name == "SOC64_perc" | name == "area64_perc"))
#### Graphing time ####
ggplot(piv, aes(x = quantiles, y = SOCandArea, fill = name)) +
    #geom_bar(aes(y = SOC_stock_sum_64), stat = "identity", position = "dodge") +
    #geom_bar(aes(y = SOC_stock_sum_64_4047), stat = "identity", position = "dodge") +
    geom_bar(stat = "identity", position = "dodge")

#order for the bar plot
size_classes644047 <- c("0.0064-0.008",  "0.008-0.0128",  "0.0128-0.0288", "0.0288-0.0704", "0.0704-0.2432", "0.2432-0.4032")
size_classes4047 <- c("0.4048-0.4912","0.4912-0.68","0.6832-1.256","1.2576-2.9088","2.9136-17.736","17.7856-261.8448")

#standard error function
data_summary <- function(data, varname, groupnames)

ggplot(SOC_zonext, aes(x = range_64, y = SOC_stock_sumTg_64)) + 
    geom_bar(stat = "identity", position = "dodge")
ggplot(SOC_zonext, aes(x = range_64, y = SOC_stock_meanMg_64)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin= SOC_stock_meanMg_64-SOC_stock_stderrMg_64, ymax= SOC_stock_meanMg_64+SOC_stock_stderrMg_64), position=position_dodge(.9))
ggplot(SOC_zonext, aes(x = range_64, y = area_median_64)) + 
    geom_bar(stat = "identity", position = "dodge")

ggplot(SOC_zonext, aes(x = range_64_4047, y = SOC_stock_sumTg_64_4047)) + 
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_discrete(limits = size_classes)
ggplot(SOC_zonext, aes(x = range_64_4047, y = SOC_stock_meanMg_64_4047)) + 
    geom_bar(stat = "identity", position = "dodge")+
    geom_errorbar(aes(ymin= SOC_stock_meanMg_64_4047-SOC_stock_stderrMg_64_4047, ymax= SOC_stock_meanMg_64_4047+SOC_stock_stderrMg_64_4047), position=position_dodge(.9))
ggplot(SOC_zonext, aes(x = range_64_4047, y = area_median_64_4047)) + 
    geom_bar(stat = "identity", position = "dodge")

ggplot(SOC_zonext, aes(x = range_4047, y = SOC_stock_sumTg_4047)) + 
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_discrete(limits = size_classes4047)
ggplot(SOC_zonext, aes(x = range_4047, y = area_median_4047)) + 
    geom_bar(stat = "identity", position = "dodge")+
    scale_x_discrete(limits = size_classes4047)
ggplot(SOC_zonext, aes(x = range_4047, y = SOC_stock_meanMg_4047)) + 
    geom_bar(stat = "identity", position = "dodge")+
    scale_x_discrete(limits = size_classes4047) +
    geom_errorbar(aes(ymin= SOC_stock_meanMg_4047-SOC_stock_stderrMg_4047, ymax= SOC_stock_meanMg_4047+SOC_stock_stderrMg_4047), position=position_dodge(.9))



# tib <- tibble(x = 1:100, y = 1, z = x**2)
# tib <- mutate(tib, cut = cut(tib$x, breaks = quantile(tib$x, probs = c(0, 0.1,  0.5, 0.9, 1)), labels = F, right = T, dig.lab = 10, include.lowest = T))
# view(tib)
