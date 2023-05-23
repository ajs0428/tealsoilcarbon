library(terra)
library(sf)
library(rgdal)
library(leaflet)
library(tidyterra)
library(tidyverse)
library(tidyr)
library(tmap)
library(ggplot2)
library(basemaps)
library(maptiles)
library(parallel)
library(spatialEco)
library(landscapemetrics)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")

#### Do this before starting: Import WIP ####
hoh_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
#mas_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/Mashel_2022_V01/Mashel_2022_V01.tif")
#col_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")

plot(hoh_WIP)
#terra::zoom(hoh_WIP)

########################################################################################################################
#### Section for extracting data from rasters ####
########################################################################################################################

# all_csv <- read.csv("SOIL CARBON/ANALYSIS/hoh_CHN_1m30cm90cm120cm_Stocks_WIPupd.csv")
# all_pts <- vect(all_csv, geom = c('lon', 'lat'))
# set.crs(all_pts, "EPSG:4326")
# all_pts_prj <- terra::project(all_pts, "EPSG:26910")
# #all_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "HOH")
# plot(hoh_WIP, col = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'))
# plot(all_pts_prj, add = T)
# # # mas_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "MAS")
# # # col_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "COL")
# # # 
# hoh_pts_ext <- terra::zonal(hoh_WIP, all_pts_prj)
# hoh_pts_ext$NAME <- (all_pts_prj$sample_name)
# hoh_pts_ext$GEO_DF <- hoh_dat_norm$GEO
# all_csv$WIP_NEW <- hoh_pts_ext$WET
# # hoh_csv$treeheight <- hoh_pts_ext
# write.csv(all_csv, file = "SOIL CARBON/ANALYSIS/hoh_CHN_1m30cm90cm120cm_Stocks_WIPupd.csv")
# # mas_pts_ext <- terra::extract(mas_WIP, mas_pts)
# col_pts_ext <- terra::extract(col_WIP, col_pts)
# hcm <- c( col_pts_ext$Band_1, hoh_pts_ext$WET, mas_pts_ext$WET)
# all_csv$WIP_INV <- hcm
# write.csv(all_csv, file = "ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")

########################################################################################################################
########################################################################################################################
#### DON'T START HERE unless necessary ####
########################################################################################################################
#### Do this before starting: Import MNDWI ####
hoh_MNDWI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")
#mas_MNDWI <- rast("SOIL CARBON/PATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM_R.tif")
#col_MNDWI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif")
#GEE <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_spec_5yr_sea2.tif")
#DTW_unmask <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/MainChannelDTW.tif")
#plot(hoh_MNDWI> 0)
#need these
hoh_WIP_mask <- hoh_WIP #already masked #mask(hoh_WIP, (hoh_MNDWI>-0.30),  maskvalues = 1, updatevalue = NA)
#mas_WIP_mask <- mask(mas_WIP, (mas_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
#col_WIP_mask <- mask(col_WIP, (col_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)


########################################################################################################################
#### SOC map masking #### 
#### Don't start here unless redoing the SOC mask
########################################################################################################################
## import UNMASKED carbon stock map ##
#hoh_C <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16.tif")
#mas_C <- rast("SOIL CARBON/MAS_CARBON_8_9_22_RanSLP.tif")
#col_C <- rast("SOIL CARBON/COL_CARBON_8_9_22_RanSLP.tif")
#plot(hoh_C)

#### Masking Function ####
mask_func <- function(MNDWI, CARBON, fileName){
    MNDWI_mask <- MNDWI > -0.30
    CARBON_0mask <- CARBON <0
    
    CARBON_0 <- mask(CARBON, CARBON_0mask, maskvalues = 1, updatevalue =0)
    CARBON_MNDWI <- mask(CARBON_0, MNDWI_mask, maskvalues = 1, updatevalue = NA)
    writeRaster(CARBON_MNDWI, filename = fileName, overwrite = T)
    CARBON_MNDWI0 <- rast(fileName)
    return(CARBON_MNDWI0)
}

LITHOL <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassMask.tif")
#LITHOL_mask <- mask_func(hoh_MNDWI, LITHOL, "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassMask.tif")
#plot(LITHOL_mask)
#### Carbon masking ####
hoh_C_mask0 <- mask_func(hoh_MNDWI, hoh_C, "SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16mask0.tif")
plot(hoh_C_mask0)
#mas_C_mask0 <- mask_func(mas_MNDWI, mas_C, "MAS_CARBON_8_9_22_RSmask0.tif")
#col_C_mask0 <- mask_func(col_MNDWI, col_C, "COL_CARBON_8_9_22_RSmask0.tif")

########################################################################################################################
####Can START HERE For latest masked carbon output ####
########################################################################################################################


hoh_WIP_mask <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
#mas_WIP_mask <- mask(mas_WIP, (mas_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
#col_WIP_mask <- mask(col_WIP, (col_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)

hoh_C_mask0 <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16mask0.tif")
hoh_C_30cm <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon30CM_LMEsqrt^2_NEWGEO_2_16.tif")
#mas_C_mask0 <- rast("SOIL CARBON/MAS_CARBON_8_9_22_RSmask0.tif")#mask_func(mas_MNDWI, mas_C, "MAS_CARBON_7_31_22_mask0.tif")
#col_C_mask0 <- rast("SOIL CARBON/COL_CARBON_8_9_22_RSmask0.tif")#mask_func(col_MNDWI, col_C, "COL_CARBON_7_31_22_mask0.tif")

plot(hoh_C_mask0)
#plot(mas_C_mask0)
#plot(col_C_mask0)

# substr(deparse(substitute(hoh_C_mask0)), 1,3)
# paste0(deparse(substitute(hoh_C_mask0)), "mask")


#### soilgrids250m ####

#CC_area <- as.polygons(hoh_C_mask0 > -999)
#CC_WGS84 <- terra::project(CC_area , "epsg:4326")
#plot(CC_area)

hoh_sg <- rast("SOIL CARBON/OTHER_DATA/Hoh_soilgrids_mask0correction.tif")#terra::mask(hoh_sg, (hoh_C_mask0 > -1000), filename = "SOIL CARBON/OTHER_DATA/Hoh_soilgrids_mask0correction.tif", overwrite = T)
plot(hoh_sg)


sg_unc <- rast("SOIL CARBON/OTHER_DATA/soilgrids_uncertainty.tif")
plot(sg_unc)
sg_unc_rpj <- terra::project(sg_unc, crs(hoh_poly))
hoh_sg_unc <- terra::crop(sg_unc_rpj, hoh_poly)
m <- c(60, 32767, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
hoh_sg_uncmask <- hoh_sg_unc |> 
    terra::classify(rclmat) |>
    terra::resample(hoh_C_mask0) |>
    terra::mask((hoh_C_mask0 > -999))
    
# #hoh_sg_rspl <- resample(hoh_sg, hoh_WIP)
#hoh_sg_mask0 <- mask_func(hoh_MNDWI, hoh_sg, "SOIL CARBON/OTHER_DATA/Hoh_soilgrids_mask0.tif")

# hoh_sg_crop <- crop(hoh_sg, CC_area)
# hoh_sg_mask <- mask(hoh_sg_crop, CC_area)
# plot(hoh_sg_mask)
# 
# #ext(hoh_sg_rspl) <- ext(hoh_WIP)
# hoh_sg_WIP <- mask(hoh_sg_crop, (hoh_WIP_mask>=0.5), maskvalues = 0, updatevalue = NA, filename ="SOIL CARBON/OTHER_DATA/Hoh_WIP_wet_soilgrids.tif", overwrite = T)
# hoh_sg_bet <- mask(hoh_sg_crop, (hoh_WIP_mask> 0.1 & hoh_WIP_mask< 0.5 ), maskvalues = 0, updatevalue = NA, filename ="SOIL CARBON/OTHER_DATA/Hoh_WIP_bet_soilgrids.tif", overwrite = T)
# hoh_sg_upl <- mask(hoh_sg_crop, (hoh_WIP_mask<=0.1 ), maskvalues = 0, updatevalue = NA, filename ="SOIL CARBON/OTHER_DATA/Hoh_WIPupl_soilgrids.tif", overwrite = T)
# 
# plot(hoh_sg_WIP)

#### Forest Cover ####
#hohFC <- rast("SOIL CARBON/SPATIAL LAYERS/GEE/hoh_tree_mask_2015.tif")
#hohFC_rpj <- rast("SOIL CARBON/SPATIAL LAYERS/GEE/hoh_tree_mask_2015_rpj.tif")#terra::project(hohFC, hoh_WIP, filename = "SPATIAL LAYERS/GEE/hoh_tree_mask_2015_rpj.tif")
#plot(hohFC_rpj)


GEE_HOHTCCproj <- rast("AGB/HOH/GEE_hohTCC.tif_proj.tif")
#GEE_HOHTCCproj <- terra::project(GEE_HOHTCC, hoh_WIP, filename = "AGB/HOH/GEE_hohTCC.tif_proj.tif")
plot(GEE_HOHTCCproj>= 50)
hist(GEE_HOHTCCproj)

for_mask <- function(SOC, forlay){
    SOC_rspl <- terra::resample(SOC, forlay)
    forested <- terra::mask(SOC_rspl, (forlay >=50), maskvalues = 0, updatevalue = NA,
                     filename = paste0("NWCA Data/",substr(deparse(substitute(forlay)), 1,3),deparse(substitute(SOC)), '.tif'), overwrite = T )
    return(forested)
    }

#### Uhran Harmonized Soil Carbon Stocks ####
#conus_deep_mean <- rast("NWCA Data/CONUS_Stock_Nov_20/CONUS_Deep_Stock_Mean.tif")
#conus_full_mean <- rast("NWCA Data/CONUS_Stock_Nov_20/CONUS_Full_Stock_Mean.tif")
hoh_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.gpkg")

hoh_uhran <- rast("NWCA Data/hoh_harmonized_uhran.tif")/100
hoh_uhran_max <- rast("NWCA Data/hoh_harmonized_uhran_max.tif")/100
hoh_uhran_min <- rast("NWCA Data/hoh_harmonized_uhran_min.tif")/100

for_hoh_uhran_mean <- rast("NWCA Data/GEEhoh_uhran.tif")#for_mask(hoh_uhran, GEE_HOHTCCproj)
for_hoh_uhran_max <- rast("NWCA Data/GEEhoh_uhran_max.tif")#for_mask(hoh_uhran_max, GEE_HOHTCCproj)
for_hoh_uhran_min<- rast("NWCA Data/GEEhoh_uhran_min.tif")#for_mask(hoh_uhran_min, GEE_HOHTCCproj)

#shallow Uhran
hoh_uhran_shalmean <- rast("NWCA Data/hoh_harmonized_uhran_shal_mean.tif")/100
hoh_uhran_shalmin <- rast("NWCA Data/hoh_harmonized_uhran_shal_min.tif")/100
hoh_uhran_shalmax <- rast("NWCA Data/hoh_harmonized_uhran_shal_max.tif")/100

for_hoh_uhran_shalmean <- for_mask(hoh_uhran_shalmean, GEE_HOHTCCproj)
for_hoh_uhran_shalmax <- for_mask(hoh_uhran_shalmax, GEE_HOHTCCproj)
for_hoh_uhran_shalmin <- for_mask(hoh_uhran_shalmin, GEE_HOHTCCproj)

#### Uhran/NWCA replacement in SoilGrids ####
hoh_uhran_rspl <- resample(hoh_uhran, hoh_sg)
hoh_uhran_sg <- terra::ifel(is.na(hoh_uhran_rspl), hoh_sg, hoh_uhran_rspl, filename = "SOIL CARBON/CrypticCarbonMaps/hoh_uhran_with_soilgrids.tif")

hoh_uhran_shalrspl <- resample(hoh_uhran_shalmean, hoh_sg)
hoh_uhran_sg_shal <- terra::ifel(is.na(hoh_uhran_shalrspl), hoh_sg, hoh_uhran_shalrspl, filename = "SOIL CARBON/CrypticCarbonMaps/hoh_uhran_shal_with_soilgrids.tif")
plot(hoh_uhran_sg_shal)


########################################################################################################################
#### Carbon masking statistics ####
########################################################################################################################


########################################################################################################################
#### Masked Layers from Cryptic Carbon ####
########################################################################################################################
#CC_FULL <- rast("SOIL CARBON/CrypticCarbon_LMEnonlog10.tif")
#CC_1M <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_16.tif")
#CC_30CM <- rast("SOIL CARBON/CrypticCarbon30cm_LMEnonlog10.tif")

########################################################################################################################
#### The actual Carbon Function ####
########################################################################################################################
C_func <- function(carbon_masked, WIP, forestCover){
        #area 
        area_test <- WIP > -999 #placeholder for cell size all cells = 1
        x <- xres(carbon_masked) #hoh = 4
        y <- yres(carbon_masked) # hoh = 4
        resol <- (x*y)/10000 # Hoh = 0.0016
 
        
        total_area <- sum(values(cellSize(area_test, unit = "ha", mask = TRUE)), na.rm = T)#Total Area in 'ha' THIS NEEDS TO BE CONSISTENT
        C_mean <- mean(values(carbon_masked), na.rm = T) #mean value of all values of Mg/ha cells
        carbon_cell <- carbon_masked*cellSize(area_test, unit = "ha", mask = TRUE) # carbon in Mg per cell which is then added up 
        #There will be small numbers here because of Mg/ha
        TotalC_sum <- sum(values(carbon_cell), na.rm =T) #Total carbon in entire area

        
        #The WIP probability section masks
        WIP_upl_mask <- WIP<=0.1 #The wetlands and betweeen go to 0, uplands go to 1
        WIP_wet_mask <- WIP >=0.5 #The wetlands go to 1, upland and between areas go to 0
        WIP_between <- (WIP<0.5 & WIP>0.1) #The area less than 0.5 and greater than 0.1 goes to 1
        
        
        #making maps of the upland, wetland, and between areas
        C_mask0_uplMg <- mask(carbon_masked, WIP_upl_mask, maskvalues = 0, updatevalue = NA, 
                              filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_uplMg)), '.tif'),
                              overwrite = T)
        C_mask0_wetMg <- mask(carbon_masked, WIP_wet_mask, maskvalues = 0, updatevalue = NA, 
                              filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_wetMg)), '.tif'),
                              overwrite = T)
        C_mask0_betweenMg <- mask(carbon_masked, WIP_between, maskvalues = 0, updatevalue = NA, 
                                  filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_betweenMg)), '.tif'),
                                  overwrite = T)
        
        #Taking the carbon amt in each cell (Mg) and masking by the WIP sections
        C_mask0_upl_cell <- mask(carbon_cell, WIP_upl_mask, maskvalues = 0, updatevalue = NA) # all wetlands masked out and upland carbon values are left
        C_mask0_wet_cell <- mask(carbon_cell, WIP_wet_mask, maskvalues = 0, updatevalue = NA) # all uplands are masked out and wetland carbon values are left
        C_mask0_between_cell <- mask(carbon_cell, WIP_between, maskvalues = 0, updatevalue = NA) #maybe should change to 1?
        
        #Summing up the total of the carbon amt in each cell from the WIP section masks
        C_upl <- global(C_mask0_upl_cell, fun ="sum", na.rm = T)[[1]]
        C_wet <- global(C_mask0_wet_cell, fun ="sum", na.rm = T)[[1]] 
        C_bet <- global(C_mask0_between_cell, fun ="sum", na.rm = T)[[1]]
        
        #number of cells in each
        # C_upl_count<- freq((WIP_upl_mask == 1), value = 1)[,3] #- min(cells(hoh_C_mask0_upl)) # This is the # of cells in UPL areas: 
        # C_wet_count <- freq((WIP_wet_mask == 1), value = 1)[,3]# - min(cells(hoh_C_mask0_wet)) # This is the sum of all Carbon in UPL areas:  
        # C_bet_count <- freq((WIP_between ==1), value = 1)[,3] #MASK VALUES =1
        
        
        upl_area <- sum(values(cellSize(C_mask0_upl_cell >-999, unit = "ha", mask = TRUE)), na.rm = T)#(C_upl_count*16)/10000 #62088.9 #<- expanse(hoh_C_mask0_upl, unit = "ha") # 62088.9ha
        wet_area <- sum(values(cellSize(C_mask0_wet_cell >-999, unit = "ha", mask = TRUE)), na.rm = T) #7417.701#<- expanse(hoh_C_mask0_wet, unit = "ha") #7417.701 ha
        bet_area <- sum(values(cellSize(C_mask0_between_cell >-999, unit = "ha", mask = TRUE)), na.rm = T)
        
        
        ## The forested wetland masking NEEDS WORK## 
        #Forest cover defined by the Cowardin/NWI is 30% for dominant overstory vegetation 
        forested <- forestCover >=50 #the forested areas are 1, nonforest are 0
        #wipmask <- mask(WIP, WIP_upl_mask, maskvalues= 1, updatevalue = NA)
        #fwmask <- mask(WIP_wet_mask, forested, maskvalues = 0, updatevalue = NA)
        #WIP_wet_fnf <- mask(WIP, forested, maskvalues = 0, updatevalue = NA) # The upland areas are 0 and are updated to NA giving WIP 
        C_mask0_wet_forestedMg <- mask(C_mask0_wetMg, forested, maskvalues = 0, updatevalue = NA,
                                       filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_wet_forestedMg)), '.tif'),
                                       overwrite = T)
        C_mask0_forestedWet_cell <- mask(C_mask0_wet_cell, forested, maskvalues = 0, updatevalue = NA)
        C_forW <- global(C_mask0_forestedWet_cell, fun ="sum", na.rm = T)[[1]]
        #C_forW_count <- freq((fwmask >0), value = 1)[,3]
        forW_area <- sum(values(cellSize(C_mask0_forestedWet_cell > -999, unit = "ha", mask = TRUE)), na.rm = T)
        
       
        #amount in uplands
        upl_amt <- C_upl
        #average amount in uplands?
        avg_upl <- mean(values(C_mask0_uplMg), na.rm = T)#(C_upl*(1/0.0016))/C_upl_count
        #amount in wetlands
        wet_amt <- C_wet 
        #average amount in wetlands
        avg_wet<- mean(values(C_mask0_wetMg, na.rm = T))#(C_wet*(1/0.0016))/C_wet_count 
        #amount in between
        bet_amt <- C_bet
        #average amount in between
        avg_bet <-  mean(values(C_mask0_betweenMg, na.rm = T))#(C_bet*(1/0.0016))/C_bet_count 
        #amount in forested wetlands
        forW_amt <- C_forW
        #average amount in forested wetlands
        avg_forW <- mean(values(C_mask0_wet_forestedMg, na.rm = T))#(C_forW*(1/0.0016))/C_forW_count
        forW_percC <- (C_forW)/TotalC_sum
        forW_percA <- forW_area/total_area
        
        
        #C_sum <- TotalC_sum/total_area
        
        wet_percC <- (C_wet)/TotalC_sum #C_wet_area[[1]]/C_area #This is the wet area carbon proportion of total carbon
        upl_percC <- C_upl/TotalC_sum # This is the upl area carbon proportion of the total carbon
        bet_percC <- (C_bet)/TotalC_sum # This is the between area carbon proportion of the total carbon
        
        wet_percA <- wet_area/total_area #This is the wetland area proportion of total
        upl_percA <- upl_area/total_area #This is the upland area proportion of total
        bet_percA <- bet_area/total_area #This is the upland area proportion of total
        
        
        cat("upland area =", upl_area, 
            "\nwet_area =", wet_area, 
            "\nforested wetland =", forW_area,
            "\nbetween area = ", bet_area, 
            "\nTotal Area =", total_area,
            "\nsum of parts = ", (upl_area + wet_area + bet_area),
            "\namount in uplands Mg, " , upl_amt,
            "\nTg= ", ",", upl_amt/1e6, 
            "\naverage amount in uplands Mg/ha= ", avg_upl,
            "\namount in wetlands Mg = ", wet_amt,
            "\nTg= ", wet_amt/1e6, 
            "\naverage amount in wetlands Mg/ha= ", avg_wet,
            "\namount in between Mg = ", bet_amt,
            "\nTg= ", bet_amt/1e6, 
            "\naverage amount in between Mg/ha= ", avg_bet,
            "\nThe total soil Carbon in Mg = ", TotalC_sum[[1]],
            "\nTg= ",TotalC_sum[[1]]/1e6, 
            "\nThe overall average soil Carbon Mg/ha is= ", C_mean,
            "\nwetland proportion of total carbon= ", wet_percC[[1]],
            "\nwetland proportion of land area= ", wet_percA[[1]],
            "\nupland proportion of total carbon= ", upl_percC[[1]],
            "\nupland proportion of land area= ", upl_percA[[1]], 
            "\nbetween land proportion of total carbon= ", bet_percC[[1]],
            "\nbetween land proportion of land area= ", bet_percA[[1]],
            "\namount in forested wetlands Mg = ", forW_amt,
            "\nTg= ", forW_amt/1e6,
            "\naverage amount in forested wetlands Mg/ha= ", avg_forW,
            "\nforested wetland proportion of total carbon= ", forW_percC[[1]],
            "\nforested wetland proportion of land area= ", forW_percA[[1]]
        )

    }



C_func(hoh_C_mask0, hoh_WIP_mask, GEE_HOHTCCproj)


#soilgrids
C_func(hoh_sg, hoh_WIP_mask, GEE_HOHTCCproj)
C_func(hoh_sg_uncmask, hoh_WIP_mask, GEE_HOHTCCproj)

#30cm
C_func(hoh_C_30cm, hoh_WIP_mask, GEE_HOHTCCproj)

####################################################################################################################################
#### Uncertainty masking statistics ####
####################################################################################################################################
CC_5_95 <- rast("bootstrapped/intervals.tif")
CC_intdiff <- rast("bootstrapped/95_interval_diff_strat2.tif")
CC_sd <- rast("bootstrapped/SOC_stdev.tif")
CC_30sd <- rast("bootstrapped/SOC_30stdev.tif")

C_func(CC_sd, hoh_WIP_mask, GEE_HOHTCCproj)
C_func(CC_30sd, hoh_WIP_mask, GEE_HOHTCCproj)

####This is WIP wetland Carbon  ####
CC_1M_WET <- rast("SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wetMg.tif")#terra::mask(hoh_C_mask0, (hoh_WIP_mask>= 0.5), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wetMg.tif", overwrite = T)
plot(CC_1M_WET)
#30cm WIP Wetland SOC
CC_30CM_WET <- terra::mask(hoh_C_30cm, (hoh_WIP_mask>= 0.5), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_30CMmask0_wetMg.tif", overwrite = T)
plot(CC_30CM_WET, main = "30CM")
####This is WIP wetland Carbon uncertainty 95 diff ####
CC_1M_WET_uncertainty <- rast("SOIL CARBON/CC1M_C_mask0_wetMg_uncertainty_strat.tif")#terra::mask(CC_intdiff, (hoh_WIP_mask>= 0.5), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CC1M_C_mask0_wetMg_uncertainty_strat.tif", overwrite = T)
plot(CC_1M_WET_uncertainty)
####This is WIP wetland Carbon uncertainty STDEV ####
CC_1M_WET_stdev <- rast("SOIL CARBON/CC1M_C_mask0_wetMg_stdev_strat.tif")#terra::mask(CC_sd, (hoh_WIP_mask>= 0.5), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CC1M_C_mask0_wetMg_stdev_strat.tif", overwrite = T)
plot(CC_1M_WET_stdev, main = "CC 1M SD")
#30cm STDEV WIP Wetland SOC
CC_30CM_WETstdev <- rast("SOIL CARBON/CrypticCarbonMaps/CC_C_30CMmask0_wetMg_stdev.tif")#terra::mask(CC_30sd, (hoh_WIP_mask>= 0.5), maskvalues = 0, updatevalue = NA, filename = ("SOIL CARBON/CrypticCarbonMaps/CC_C_30CMmask0_wetMg_stdev.tif"), overwrite = T)
plot(CC_30CM_WETstdev, main = "30CM SD")

####Forested WIP Wetland SOC Map ####
CC_C_mask0_wet_forestedMg <- rast("SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wet_forestedMg.tif")#mask(CC_1M_WET, GEE_HOHTCCproj>= 50, maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wet_forestedMg.tif", overwrite = T)
plot(CC_C_mask0_wet_forestedMg, main = "Forested WIP Wetland SOC Map 1M")
CC_C_mask0_wet_forestedMg_uncert <- rast("SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wet_forestedMg_uncert_strat.tif")#terra::mask(CC_1M_WET_uncertainty, GEE_HOHTCCproj>=50, maskvalues = 0, updatevalue = NA,  filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wet_forestedMg_uncert_strat.tif", overwrite = TRUE)
plot(CC_C_mask0_wet_forestedMg_uncert, main = "Forested WIP Wetland SOC Map 1M 95%")

#### Forested WIP Wetland SOC STDEV ####
CC_C_mask0_wet_forestedMg_stdev <- rast("SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wet_forestedMg_stdev_strat.tif")#terra::mask(CC_1M_WET_stdev, GEE_HOHTCCproj>=50, maskvalues = 0, updatevalue = NA,  filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_wet_forestedMg_stdev_strat.tif", overwrite = TRUE)
plot(CC_C_mask0_wet_forestedMg_stdev, main = "Forested WIP Wetland SOC STDEV 1M")

####This is WIP wetland MINUS Uhran wetland Carbon  ####
test <- hoh_uhran > -12
uhran_mask <- rast("NWCA Data/uhran_mask.tif")
plot(uhran_mask, col = "black")
#30cm WIP Wetland SOC minus Uhran
CC_30CM_WET_NoUhran <- terra::mask(CC_30CM_WET, uhran_mask, maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_30CMmask0_wetMg_nouhran.tif", overwrite = T)
plot(CC_30CM_WET_NoUhran, main = "30CM No Uhran")
#1m WIP Wetalnd SOC minus Uhran
CC_1M_WET_NoUhran <- terra::mask(CC_1M_WET, uhran_mask, maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_1Mmask0_wetMg.tif_nouhran.tif", overwrite = T)
plot(CC_1M_WET_NoUhran, main = "1M No Uhran")
####This is WIP wetland Carbon uncertainty STDEV minus Uhran ####
CC_1M_WET_stdev_NoUhran <- terra::mask(CC_1M_WET_stdev, uhran_mask, maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CC1M_C_mask0_wetMg_stdev_strat_nouhran.tif", overwrite = T)
plot(CC_1M_WET_stdev_NoUhran, main = "1M CC SD, no uhran")
#30cm STDEV WIP Wetland SOC minus Uhran
CC_30CM_WETstdev_NoUhran <- terra::mask(CC_30CM_WETstdev, uhran_mask, maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/CC_C_30CMmask0_wetMg.tif", overwrite = T)
plot(CC_30CM_WETstdev_NoUhran, main = "30CM SD no uhran")
#1m Forested WIP SOC minus Uhran
CC_1M_WET_NoUhran_for <- terra::mask(CC_C_mask0_wet_forestedMg, uhran_mask, maskvalues = 1, updatevalue = NA, filename ="CC_C_mask0_wet_forestedMg_nouhran.tif")
plot(CC_1M_WET_NoUhran_for, main = "1m Forested WIP SOC minus Uhran")
#1m Forested WIP SOC minus Uhran
CC_1M_WET_NoUhran_for_stdev <- terra::mask(CC_C_mask0_wet_forestedMg_stdev, uhran_mask, maskvalues = 1, updatevalue = NA, filename ="CC_C_mask0_wet_forestedMg_nouhran_stdev.tif")
plot(CC_1M_WET_NoUhran_for_stdev, main = "1m Forested WIP SOC minus Uhran SD")



#### Take Lithology/Geology and mask Riverine and Non-Riverine wetlands for SOC ####
LITHOL <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_100k_reclassified.tif")

#### This is the Riverine Wetland SOC 
RIV_WET <- terra::mask(CC_1M_WET, (LITHOL== "alluvium_marine_water"), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/RiverineWetland_MeanSOC.tif", overwrite = T)
NonRIV_WET <- terra::mask(CC_1M_WET, (LITHOL== "alluvium_marine_water"), maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/NonRiverineWetland_MeanSOC.tif", overwrite = T)
#Uncertainty Riverine and Non Riverine Wetland SOC
RIV_WET_uncertainty <- terra::mask(CC_1M_WET_uncertainty, (LITHOL== "alluvium_marine_water"), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/RiverineWetland_UncertaintySOC.tif", overwrite = T)
NonRIV_WET_uncertainty <- terra::mask(CC_1M_WET_uncertainty, (LITHOL== "alluvium_marine_water"), maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/NonRiverineWetland_UncertaintySOC.tif", overwrite = T)
#STDEV Riverine and Non Riverine Wetland SOC
RIV_WET_stdev <- terra::mask(CC_1M_WET_stdev, (LITHOL== "alluvium_marine_water"), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/RiverineWetland_stdevSOC.tif", overwrite = T)
NonRIV_WET_stdev <- terra::mask(CC_1M_WET_stdev, (LITHOL== "alluvium_marine_water"), maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/NonRiverineWetland_stdevSOC.tif", overwrite = T)
#30cm Riverine and Palustrine/Non-riverine
RIV_WET_30 <- terra::mask(CC_30CM_WET, (LITHOL== "alluvium_marine_water"), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/RiverineWetland_30CMMeanSOC.tif", overwrite = T)
NonRIV_WET_30 <- terra::mask(CC_30CM_WET, (LITHOL== "alluvium_marine_water"), maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/NonRiverineWetland_30CMMeanSOC.tif", overwrite = T)
#30cm STDEV Riverine and Palustrine/Non-riverine
RIV_WET_30stdev <- terra::mask(CC_30CM_WETstdev, (LITHOL== "alluvium_marine_water"), maskvalues = 0, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/RiverineWetland_30CMMeanSOC.tif", overwrite = T)
NonRIV_WET_30stdev <- terra::mask(CC_30CM_WETstdev, (LITHOL== "alluvium_marine_water"), maskvalues = 1, updatevalue = NA, filename = "SOIL CARBON/CrypticCarbonMaps/NonRiverineWetland_30CMMeanSOC.tif", overwrite = T)






#### NWI mask here ####

NWI_extract <- function(WA_NWI, studypoly){
    studypolyrpj <- terra::project(studypoly, crs(WA_NWI))
    studyNWI <- terra::crop(WA_NWI, studypolyrpj)
    studyNWI_mask <- terra::mask(studyNWI, studypoly)
    writeVector(studyNWI_mask, filename = paste0("NWI/",substr(deparse(substitute(studypoly)), 1,3),
                                                 "NWI_poly.gpkg"), overwrite = T)
    return(studyNWI_mask)
}

#### NWI Mask C layers ####
hoh_NWI_rpj <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP_RPJ.shp") #NWI polygons
hoh_NWI_WIPmask <- terra::mask(hoh_WIP_mask, hoh_NWI_rpj, filename = "SOIL CARBON/OTHER_DATA/Hoh_NWI_WIPvalues.tif", overwrite = T)# This is the WIP values in NWI polygons
hoh_NWI_Cmask <- terra::mask(hoh_C_mask0, hoh_NWI_rpj, filename = "SOIL CARBON/OTHER_DATA/Hoh_NWI_Cvalues.tif", overwrite = T) # This is SOC values in NWI Polygons
#hoh_NWI_rpj_noRIV <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_NORIV.shp")#hoh_NWI_rpj[hoh_NWI_rpj$WETLAND_TY %in% c("Freshwater Forested/Shrub Wetland", "Freshwater Pond", "Freshwater Emergent Wetland")]
#hoh_NWI_RIVonly <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_RIVonly.shp")#hoh_NWI_rpj[hoh_NWI_rpj$WETLAND_TY %in% c("Riverine")]
#writeVector(hoh_NWI_rpj, filename ="/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP_RPJ.shp")

#### WA Dept of Ecology NWI layer ####
    # used by Halabisky et al 2022
    # does not contain stream polygons
    # no deepwater 
# But this does mostly map river channel which I attempt to mask out with the MNDWI 
# The removal of streams is more easily interpreted instead of potential riparian habitat, which most are not
#WA_Eco_NWI <- vect("NWI/WA_NWI_Final_Export.gpkg")
#hoh_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.shp")
#hoh_poly <- terra::project(hoh_poly, "EPSG:2927")
#hoh_Eco_NWI <- terra::mask(WA_Eco_NWI, hoh_poly)
hoh_Eco_NWI <- vect("NWI/Hoh_NWI_WA_Eco.gpkg")#terra::project(hoh_Eco_NWI, "EPSG:26910")
plot(hoh_Eco_NWI)
#writeVector(hoh_Eco_NWI, filename = "NWI/Hoh_NWI_WA_Eco.gpkg")

#hoh_NWI_WIPmask <- terra::mask(hoh_WIP_mask, hoh_NWI_rpj, filename = "SOIL CARBON/OTHER_DATA/Hoh_NWI_WIPvalues.tif", overwrite = T)# This is the WIP values in NWI polygons
hoh_NWI_Cmask_eco <- rast("SOIL CARBON/CrypticCarbonMaps/Hoh_NWI_Eco_Cvalues.tif")#terra::mask(hoh_C_mask0, hoh_Eco_NWI, filename = "SOIL CARBON/OTHER_DATA//Hoh_NWI_Eco_Cvalues.tif", overwrite = T) # This is SOC values in NWI Polygons
plot(hoh_NWI_Cmask_eco)

#New uncertainty maps are in process
hoh_NWI_Cmask_eco_uncertainty <- terra::mask(CC_intdiff, hoh_Eco_NWI, filename = "SOIL CARBON/CrypticCarbonMaps/Hoh_NWI_Eco_Cvalues_uncertainty.tif", overwrite = T) # This is SOC values in NWI Polygons
hoh_NWI_Cmask_eco_stdev <- terra::mask(CC_sd, hoh_Eco_NWI, filename = "SOIL CARBON/CrypticCarbonMaps/Hoh_NWI_Eco_Cvalues_stdev.tif", overwrite = T) # This is SOC values in NWI Polygons

#### NWI Uncertainty Mask C layers ####
#hoh_NWI_Cmask_uncertainty <- terra::mask(CC_intdiff, hoh_NWI_rpj, filename = "SOIL CARBON/CrypticCarbonMaps/Hoh_NWI_Cvalues_uncertainty.tif", overwrite = T) # This is SOC values in NWI Polygons

#These are all NWI wetlands with carbon values
#CC_FULL_NWI_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_FULL_NWI_CARBON_mask0.tif") #rast("OTHER_DATA/CC_FULL_NWI_CARBON_mask0.tif")
#CC_1M_NWI_mask <- rast("SOIL CARBON/OTHER_DATA/Hoh_NWI_Cvalues.tif") #rast("OTHER_DATA/CC_1M_NWI_CARBON_mask0.tif")
#CC_30CM_NWI_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_30CM_NWI_CARBON_mask0.tif") #rast("OTHER_DATA/CC_30CM_NWI_CARBON_mask0.tif")

# #These are NWI wetlands (no RIV) with carbon values
# CC_FULL_NWInoriv_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_FULL_NWI_NORIV_CARBON_mask0.tif") #rast("OTHER_DATA/CC_FULL_NWI_CARBON_mask0.tif")
# CC_1M_NWInoriv_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_1M_NWI_NORIV_CARBON_mask0.tif") #rast("OTHER_DATA/CC_1M_NWI_CARBON_mask0.tif")
# CC_30CM_NWInoriv_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_30CM_NWI_NORIV_CARBON_mask0.tif") #rast("OTHER_DATA/CC_30CM_NWI_CARBON_mask0.tif")
# 
# #These are WIP wetland carbon minus NWI_noRIV wetland carbon 
# CC_1M_NWInoriv_maskWIP <- mask(CC_1M_WET, (CC_1M_NWInoriv_mask> -999), 
#                                maskvalues = 1, updatevalue = NA, 
#                                filename = "SOIL CARBON/CC_1M_NWInoriv_maskWIP_C_WET.tif", overwrite = T)
# bm <- get_tiles(CC_1M_NWInoriv_maskWIP, "Esri.WorldImagery")
# plotRGB(bm)
# plot(terra::project(hoh_WIP_mask, "epsg:4326)"), add = T)


# #Rapid Carbon Assesssment
# install.packages("soilDB")
# library(soilDB)
# 
# hoh_poly_wgs84 <- terra::project(hoh_poly, "EPSG:4326")
# terra::ext(hoh_poly_wgs84)
# raca <- fetchRaCA(bbox = c(-123.85,47.59 , -124.51,47.88))
# df <- data.frame(x = raca$pedons$x, y = raca$pedons$y)
# pts <- terra::vect(df, geom = c("x", "y"), crs = "EPSG:4326")
# mapview::mapview(st_as_sf(pts), map.types = "Esri.WorldImagery")
##### These are the WIP wetland carbon minus NWI ALL wetlands -> CRYPTIC? ####
CC_1M_NWI_All_maskWIP <- mask(CC_1M_WET, hoh_Eco_NWI, inverse = T,
                                #maskvalues = 1, updatevalue = NA, 
                                filename = "SOIL CARBON/CrypticCarbonMaps/CC_1M_WAEcoNWI_All_maskWIP_C_WET.tif", overwrite = T)
##### These are the WIP wetland carbon minus NWI ALL wetlands Uncertainty -> CRYPTIC Unertatinty? ####
CC_1M_NWI_All_maskWIP_uncertainty <- mask(CC_1M_WET_uncertainty, hoh_Eco_NWI, inverse = T,
                              #maskvalues = 1, updatevalue = NA, 
                              filename = "SOIL CARBON/CC_1M_NWI_All_maskWIP_C_WET_uncertainty.tif", overwrite = T)
#### These are the Forested WIP wetland SOC minus NWI ALL wetlands 
CC_1M_NWI_All_maskWIP_forested <- mask(CC_C_mask0_wet_forestedMg, hoh_Eco_NWI, inverse = T,
                              #maskvalues = 1, updatevalue = NA, 
                              filename = "SOIL CARBON/CrypticCarbonMaps/CC_Forested_1M_WAEcoNWI_All_maskWIP_C_WET.tif", overwrite = T)
#### These are the Forested WIP wetland SOC minus NWI ALL wetlands uncertainty interval
CC_1M_NWI_All_maskWIP_forested_uncertainty <- mask(CC_C_mask0_wet_forestedMg_uncert, hoh_Eco_NWI, inverse = T,
                              #maskvalues = 1, updatevalue = NA, 
                              filename = "SOIL CARBON/CC_Forested_1M_NWI_All_maskWIP_C_WET_uncert.tif", overwrite = T)


#### These are the plots of the different maskings of NWI ####
plot(CC_1M_WET, main = "ALL WIP Wetlands Carbon")
plot(CC_1M_NWI_All_maskWIP, main = "WIP wetlands - All WA Eco NWI") #
plot(CC_1M_NWI_All_maskWIP_forested, main = "Forested WIP minus All WA Eco NWI ")
#plot(CC_1M_NWInoriv_maskWIP, main = "WIP - NWI No RIV")


#### SoilGrids Masking with NWI for comparison with WIP ####
    #NWI wetlands with SoilGrids SOC

hoh_sg_nwi <- terra::mask(hoh_sg, hoh_Eco_NWI, filename = "SOIL CARBON/CrypticCarbonMaps/SoilGrids_NWI_SOC.tif", overwrite = T)
hoh_sg_nwiunc <- terra::mask(hoh_sg_uncmask, hoh_Eco_NWI, filename = "SOIL CARBON/CrypticCarbonMaps/SoilGrids_NWI_uncSOC.tif", overwrite = T)
plot(hoh_sg_nwi)




#save these for masking out NWI Riverine
#CC_FULL_RIV_only <- mask(CC_FULL, hoh_NWI_RIVonly[hoh_NWI_RIVonly$Shape_Area>-9])
#CC_noRIV_NWI_mask <- mask(CC_FULL, CC_FULL_RIV_only>-999, maskvalues = 1, updatevalue = NA)

# #These are WIP carbon maps without the NWI Riverine wetlands
# CC_FULL_noRIV_NWI_WIP_mask <- rast("SOIL CARBON/CC_FULL_noRIV_NWI_WIP_mask.tif")
# CC_1M_noRIV_NWI_WIP_mask <- rast("SOIL CARBON/CC_1M_noRIV_NWI_WIP_mask.tif")#mask(CC_1M_noRIV_NWI_WIP_mask, hoh_WIP_mask<0.5, maskvalues = 1, updatevalue = NA)
# CC_30CM_noRIV_NWI_WIP_mask<- rast("SOIL CARBON/CC_30CM_noRIV_NWI_WIP_mask.tif")


#### NLCD ####
# NLCD <- rast("NLCD/2019/NLCD/NLCD_2019_Land_Cover_L48_20210604_EhrjVzX3LPtBKSpTgmDN.tiff")
# NLCD_rpj <- NLCD |> terra::project("EPSG:26910", "near") |>
#     terra::crop(CC_area, mask = T, touches = T) |> 
#     terra::resample(CC_1M, "near") |>
#     writeRaster("NLCD/2019/NLCD2019_rspl.tif", overwrite = T)
m <- rbind(c(90, 90), c(95, 95))
NLCD <- rast("NLCD/2019/NLCD2019_rspl.tif")
NLCD_WET <- NLCD |> terra::classify(m, others = NA)
plot(NLCD_WET)
NLCD_WETmask_sg <-mask(hoh_sg, NLCD_WET, filename = "SOIL CARBON/CrypticCarbonMaps/NLCD_WET_SG_SOC.tif", overwrite = T)
NLCD_WETmask_sgunc <-mask(hoh_sg_uncmask, NLCD_WET, filename = "SOIL CARBON/CrypticCarbonMaps/NLCD_WET_SG_UncSOC.tif", overwrite = T)
NLCD_WETmaskC <- rast("SOIL CARBON/CrypticCarbonMaps/NLCD_WET_SOC.tif")


#These are WIP wetland carbon minus NLCD no open water wetland carbon 
#CC_1M_NLCD_no_OW_maskWIP <- rast("SOIL CARBON/CC_1M_NLCD_no_OW_maskWIP.tif")#mask(CC_1M_WET, (NLCD_maskC> -999), 
#maskvalues = 1, updatevalue = NA, 
#filename = "SOIL CARBON/CC_1M_NLCD_no_OW_maskWIP.tif", overwrite = T)
#plot(CC_1M_NLCD_no_OW_maskWIP)


# #### USDA Rapid Carbon Assessment ####
# library(lattice)
# library(soilDB)
# library(aqp)
# library(rasterVis)
# library(viridisLite)
# # color palettes and manipulation
# library(RColorBrewer)
# library(colorspace)
# RaCA <- mukey.wcs(CC_1M, db = c("gSSURGO", res = 30))
# plot(RaCA)
# m <- unique(values(RaCA))
# m
# 
# #extract rat for thematic mapping
# rat <- cats(RaCA)[[1]]
# 
# #variable of interest 
# vars <- c("om_r", "dbovendry_r")
# 
# # get / aggregate specific horizon-level properties from SDA
# # be sure to see the manual page for this function
# p <-  get_SDA_property(property = vars,
#                        method = "Dominant Component (Numeric)", 
#                        mukeys = as.integer(rat$mukey),
#                        top_depth = 0,
#                        bottom_depth = 100)
# head(p)
# 
# p$areasymbol <- factor(p$areasymbol)
# rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = F, all.x = T)
# levels(RaCA) <- rat
# # list variables in the RAT
# names(cats(RaCA)[[1]])
# activeCat(RaCA) <- 'om_r'
# plot(RaCA)
# 
# activeCat(RaCA) <- 1
# RaCA.stack <- catalyze(RaCA)
# RaCA.stack <- RaCA.stack[[vars]]
# 
# levelplot(
#     RaCA.stack[['om_r']], 
#     main = 'Organic Matter 100cm',
#     margin = FALSE, 
#     scales = list(draw = FALSE), 
#     col.regions = viridis,
#     maxpixels = 1e5
# )


########################################################################################################################
#####simple carbon sum calculator ####
########################################################################################################################

C_map_simp<- function(C_map){
    gt <- (C_map > -999)
    area_tot <- sum(values(cellSize(gt, unit = "ha", mask = TRUE)), na.rm = T)
    C_mean <- mean(values(C_map), na.rm = T) #mean value of all values of Mg/ha cells
    carbon_cell <- C_map*cellSize(gt, unit = "ha") # carbon in Mg per cell which is then added up 
    #There will be small numbers here because of Mg/ha
    TotalC_sum <- sum(values(carbon_cell), na.rm =T)
    #writeRaster(C_map, paste0("SOIL CARBON/CrypticCarbonMaps/", (deparse(substitute(C_map))), '.tif'), overwrite = T) #"SOIL CARBON/CC_FULL_noRIV_NWI_WIP_mask.tif"
    cat("Total area =", area_tot, 
        "\nAverage Carbon Stock (Mg/ha) =", C_mean, 
        "\ntotal Carbon (Tg) =", TotalC_sum/1e6)
}

agb <- rast("AGB/HOH/Hudak_Hoh_2013.tif")*0.5
plot(agb)
C_map_simp(agb)
C_map_simp(hoh_C_mask0)
#C_map_simp(NLCD_maskC) #This is NLCD wetlands with carbon
#C_map_simp(CC_FULL_NWI_mask)#this is all NWI Wetlands with carbon
C_map_simp(hoh_NWI_Cmask)# All NWI wetland carbon with Riv
C_map_simp(hoh_NWI_Cmask_uncertainty)#C_map_simp(CC_30CM_NWI_mask)

#soilgrids uncertainty
C_map_simp(hoh_sg_uncmask)
#soil grids NWI
C_map_simp(hoh_sg_nwi)
C_map_simp(hoh_sg_nwiunc)
# soilgrids NLCD
C_map_simp(NLCD_WETmask_sg) #soil grids NLCD 30cm
C_map_simp(NLCD_WETmask_sgunc)
#### Uhran NWCA ####
C_map_simp(hoh_uhran) #mean
C_map_simp(hoh_uhran_max) #max
C_map_simp(hoh_uhran_min) #min


C_map_simp(for_hoh_uhran_mean)
C_map_simp(for_hoh_uhran_max)
C_map_simp(for_hoh_uhran_min)

#### WIP MINUS Uhran NWCA ####
C_map_simp(CC_1M_WET_NoUhran)  
C_map_simp(CC_1M_WET_stdev_NoUhran)
#forested
C_map_simp(CC_1M_WET_NoUhran_for)
C_map_simp(CC_1M_WET_NoUhran_for_stdev)

#### Uhran NWCA shallow ####
C_map_simp(hoh_uhran_shalmean) #mean
C_map_simp(hoh_uhran_shalmax) #max
C_map_simp(hoh_uhran_shalmin) #min

C_map_simp(for_hoh_uhran_shalmean)
C_map_simp(for_hoh_uhran_shalmax)
C_map_simp(for_hoh_uhran_shalmin)

#WA Ecology NWI wetland polygons
C_map_simp(hoh_NWI_Cmask_eco) # WA Eco NWI SOC 
C_map_simp(CC_1M_NWI_All_maskWIP) #WIP minus WA Eco NWI SOC 
C_map_simp(CC_1M_NWI_All_maskWIP_forested) #Forested WIP minus WA Eco NWI SOC 
C_map_simp(hoh_NWI_Cmask_eco_uncertainty) #WA Eco SOC uncertainty
C_map_simp(CC_1M_NWI_All_maskWIP_forested_uncertainty) # forested WIP wetlands without NWI


C_map_simp(CC_1M_WET)# "ALL WIP Wetlands Carbon"
C_map_simp(CC_1M_NWI_All_maskWIP)# "WIP - All NWI incl. RIV" ***Makes sense that it is complete WIP
C_map_simp(CC_1M_NWI_All_maskWIP_uncertainty) #Now this has WA Eco NWI

#30CM SOC
C_map_simp(RIV_WET_30)
C_map_simp(NonRIV_WET_30)
#30CM SOC STDEV RIV NON RIV
C_map_simp(RIV_WET_30stdev)
C_map_simp(NonRIV_WET_30stdev)

#Riverine uncertainty
C_map_simp(RIV_WET_uncertainty)
C_map_simp(NonRIV_WET_uncertainty)
#Forested wetlands all and outside NWI
C_map_simp(CC_1M_NWI_All_maskWIP_forested)
C_map_simp(CC_1M_NWI_All_maskWIP_forested_uncertainty)

#### standard deviation SOC accounting ####
C_map_simp(CC_1M_WET_stdev) # 1m WIP Wetland SOC stdev
C_map_simp(RIV_WET_stdev) # 1m WIP Riverine SOC
C_map_simp(NonRIV_WET_stdev) # 1m WIP Non-Riverine SOC
C_map_simp(CC_C_mask0_wet_forestedMg_stdev) # 1m Forested Wetland WIP SOC
C_map_simp(hoh_NWI_Cmask_eco_stdev) # 1m NWI WIP Wetland SOC

#Majority filtered, FCC masked, outside of NWI SOC ####################
C_map_simp(CC_WIPmaj_AGB50_NWI_mask)
C_map_simp(CC_WIPmaj_AGB50_NWI_mask_uncert)

C_map_simp(CC_WIPmaj_mask64)

# C_map_simp(RIV_WET)
# C_map_simp(NonRIV_WET)
# C_map_simp(RIV_WET_uncertainty)
# C_map_simp(NonRIV_WET_uncertainty)



#C_map_simp(CC_1M_NWInoriv_maskWIP)# "WIP - NWI No RIV"
C_map_simp(CC_1M_NWI_mask) #"NWI RIV"

C_map_simp(CC_1M_NLCD_no_OW_maskWIP)

C_map_simp(hoh_sg_crop)
C_map_simp(hoh_sg_WIP)
C_map_simp(hoh_sg_bet)
C_map_simp(hoh_sg_upl)
C_map_simp(agb)
C_map_simp(agb_wet_mask)


#### WIP Classification Uncertainty ####
hoh_WIP_dat <- as.data.frame(values(hoh_WIP_mask$WET, na.rm = T))



ggplot() +
    geom_spatraster(data = hoh_WIP_mask) +
    geom_spatvector(data = hoh_NWI_rpj, fill = NA)

tmap::tm_shape(hoh_WIP_mask) + 
    tm_raster(title = "WIP")

ctrf <- vect("NPCTR/Coastal Temperate Rain Forest of Western North America - Original Distribution/data/data/ctrf/ctrf.shp")
plot(ctrf)
pts <- read.csv("HLEF/HLEF2022LAB-LOCATION.csv")
pts_spat <- vect(pts, geom = c("lon", "lat"))
plot(pts_spat, col = "red")
hlef <- rast("HLEF/SEAK_WIP_9-10-2021_output.tif")
plot(hlef, col = rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')))
points(pts_spat, col = 'red', pch = 30)



