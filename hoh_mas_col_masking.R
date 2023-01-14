library(terra)
library(rgdal)
library(leaflet)
library(tidyterra)
library(tmap)
library(ggplot2)
library(basemaps)
library(maptiles)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")

#### Do this before starting: Import WIP ####
hoh_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
#mas_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/Mashel_2022_V01/Mashel_2022_V01.tif")
#col_WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")

plot(hoh_WIP)
terra::zoom(hoh_WIP)

# th <- rast("SPATIAL LAYERS/Hoh_TreeHeight.tif") 
all_csv <- read.csv("SOIL CARBON/ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")
hoh_csv <- subset(all_csv, all_csv$STUDY_AREA == "HOH")
all_pts <- vect(all_csv, geom = c('lon', 'lat'))
set.crs(all_pts, "EPSG:4326")
all_pts_prj <- terra::project(all_pts, "EPSG:26910")
hoh_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "HOH")
plot(hoh_WIP, col = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'))
# # mas_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "MAS")
# # col_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "COL")
# # 
# hoh_pts_ext <- terra::extract(th, hoh_pts)
# hoh_csv$treeheight <- hoh_pts_ext
# write.csv(hoh_csv, file = "ANALYSIS/HOH_SOILC_8_16_22_TH.csv")
# # mas_pts_ext <- terra::extract(mas_WIP, mas_pts)
# col_pts_ext <- terra::extract(col_WIP, col_pts)
# hcm <- c( col_pts_ext$Band_1, hoh_pts_ext$WET, mas_pts_ext$WET)
# all_csv$WIP_INV <- hcm
# write.csv(all_csv, file = "ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")


#### Do this before starting: Import MNDWI ####
hoh_MNDWI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")
#mas_MNDWI <- rast("SOIL CARBON/PATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM_R.tif")
#col_MNDWI <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif")
GEE <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_spec_5yr_sea2.tif")
DTW_unmask <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/MainChannelDTW.tif")
plot(hoh_MNDWI> 0)
#need these
hoh_WIP_mask <- hoh_WIP #already masked #mask(hoh_WIP, (hoh_MNDWI>-0.30),  maskvalues = 1, updatevalue = NA)
#mas_WIP_mask <- mask(mas_WIP, (mas_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
#col_WIP_mask <- mask(col_WIP, (col_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)



#### DON'T START HERE unless necessary ####
## import UNMASKED carbon stock map ##
hoh_C <- rast("SOIL CARBON/HOHMOD_CARBON_8_10_22_RanSLP.tif")
mas_C <- rast("SOIL CARBON/MAS_CARBON_8_9_22_RanSLP.tif")
col_C <- rast("SOIL CARBON/COL_CARBON_8_9_22_RanSLP.tif")
plot(mas_C< (0))

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

#GEO_mask <- mask_func(hoh_MNDWI, GEOm, "SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_250k_reclassMask.tif")
#### Carbon masking ####
#hoh_C_mask0 <- mask_func(hoh_MNDWI, hoh_C, "HOH_CARBON_8_9_22_RSmask0.tif")
#mas_C_mask0 <- mask_func(mas_MNDWI, mas_C, "MAS_CARBON_8_9_22_RSmask0.tif")
#col_C_mask0 <- mask_func(col_MNDWI, col_C, "COL_CARBON_8_9_22_RSmask0.tif")


####Can START HERE For latest masked carbon output ####
hoh_WIP_mask <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
#mas_WIP_mask <- mask(mas_WIP, (mas_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
#col_WIP_mask <- mask(col_WIP, (col_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)

hoh_C_mask0 <-  rast("SOIL CARBON/CrypticCarbon_LMEnonlog10.tif")##rast("HOH_CARBON_8_9_22_RSmask0.tif")#mask_func(hoh_MNDWI, hoh_C, "HOH_CARBON_7_31_22_mask0.tif")
#mas_C_mask0 <- rast("SOIL CARBON/MAS_CARBON_8_9_22_RSmask0.tif")#mask_func(mas_MNDWI, mas_C, "MAS_CARBON_7_31_22_mask0.tif")
#col_C_mask0 <- rast("SOIL CARBON/COL_CARBON_8_9_22_RSmask0.tif")#mask_func(col_MNDWI, col_C, "COL_CARBON_7_31_22_mask0.tif")

plot(hoh_C_mask0)
#plot(mas_C_mask0)
#plot(col_C_mask0)

# substr(deparse(substitute(hoh_C_mask0)), 1,3)
# paste0(deparse(substitute(hoh_C_mask0)), "mask")


#### soilgrids250m ####

CC_area <- as.polygons(hoh_C_mask0 > -999)
CC_WGS84 <- terra::project(CC_area , "epsg:4326")
plot(CC_area)

hoh_sg <- rast("SOIL CARBON/OTHER_DATA/Hoh_soilgrids.tif")
# #hoh_sg_rspl <- resample(hoh_sg, hoh_WIP)
# plot(hoh_sg)
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



#### Carbon masking statistics ####


# Forest Cover ####
#hohFC <- rast("SOIL CARBON/SPATIAL LAYERS/GEE/hoh_tree_mask_2015.tif")
hohFC_rpj <- rast("SOIL CARBON/SPATIAL LAYERS/GEE/hoh_tree_mask_2015_rpj.tif")#terra::project(hohFC, hoh_WIP, filename = "SPATIAL LAYERS/GEE/hoh_tree_mask_2015_rpj.tif")


#### Masked Layers from Cryptic Carbon ####
CC_FULL <- rast("SOIL CARBON/CrypticCarbon_LMEnonlog10.tif")
CC_1M <- rast("SOIL CARBON/CrypticCarbon1M_LMEnonlog10_NEWGEO.tif")
CC_30CM <- rast("SOIL CARBON/CrypticCarbon30cm_LMEnonlog10.tif")

#These are most recent wet, upl, and mesic layers for WIP Carbon
CC_1M_WET <- rast("SOIL CARBON/CC1M_C_mask0_wetMg.tif")

# NWI Mask C layers ####
hoh_NWI_rpj <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP_RPJ.shp")#terra::project(hoh_NWI, crs(hoh_WIP))
hoh_NWI_mask <- rast("SOIL CARBON/OTHER_DATA/Hoh_NWI_WIPvalues.tif")
hoh_NWI_rpj_noRIV <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_NORIV.shp")#hoh_NWI_rpj[hoh_NWI_rpj$WETLAND_TY %in% c("Freshwater Forested/Shrub Wetland", "Freshwater Pond", "Freshwater Emergent Wetland")]
hoh_NWI_RIVonly <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_RIVonly.shp")#hoh_NWI_rpj[hoh_NWI_rpj$WETLAND_TY %in% c("Riverine")]
#writeVector(hoh_NWI_rpj, filename ="/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP_RPJ.shp")


#These are all NWI wetlands with carbon values
CC_FULL_NWI_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_FULL_NWI_CARBON_mask0.tif") #rast("OTHER_DATA/CC_FULL_NWI_CARBON_mask0.tif")
CC_1M_NWI_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_1M_NWI_CARBON_mask0.tif") #rast("OTHER_DATA/CC_1M_NWI_CARBON_mask0.tif")
CC_30CM_NWI_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_30CM_NWI_CARBON_mask0.tif") #rast("OTHER_DATA/CC_30CM_NWI_CARBON_mask0.tif")

#These are NWI wetlands (no RIV) with carbon values
CC_FULL_NWInoriv_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_FULL_NWI_NORIV_CARBON_mask0.tif") #rast("OTHER_DATA/CC_FULL_NWI_CARBON_mask0.tif")
CC_1M_NWInoriv_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_1M_NWI_NORIV_CARBON_mask0.tif") #rast("OTHER_DATA/CC_1M_NWI_CARBON_mask0.tif")
CC_30CM_NWInoriv_mask <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/OTHER_DATA/CC_30CM_NWI_NORIV_CARBON_mask0.tif") #rast("OTHER_DATA/CC_30CM_NWI_CARBON_mask0.tif")

#These are WIP wetland carbon minus NWI_noRIV wetland carbon 
CC_1M_NWInoriv_maskWIP <- mask(CC_1M_WET, (CC_1M_NWInoriv_mask> -999), 
                               maskvalues = 1, updatevalue = NA, 
                               filename = "SOIL CARBON/CC_1M_NWInoriv_maskWIP_C_WET.tif", overwrite = T)
# bm <- get_tiles(CC_1M_NWInoriv_maskWIP, "Esri.WorldImagery")
# plotRGB(bm)
# plot(terra::project(hoh_WIP_mask, "epsg:4326)"), add = T)

#These are the WIP wetland carbon minus NWI ALL wetlands -> CRYPTIC? 
CC_1M_NWI_All_maskWIP <- rast("SOIL CARBON/CC_1M_NWI_All_maskWIP_C_WET.tif")#mask(CC_1M_WET, (CC_1M_NWI_mask> -999), 
#                                maskvalues = 1, updatevalue = NA, 
#                                filename = "SOIL CARBON/CC_1M_NWI_All_maskWIP_C_WET.tif", overwrite = T)

#These are the plots of the different maskings of NWI
plot(CC_1M_WET, main = "ALL WIP Wetlands Carbon")
plot(CC_1M_NWI_All_maskWIP, main = "WIP - All NWI incl. RIV")
plot(CC_1M_NWInoriv_maskWIP, main = "WIP - NWI No RIV")
plot(CC_1M_NWI_mask, main = "NWI with RIV")

#save these for masking out NWI Riverine
#CC_FULL_RIV_only <- mask(CC_FULL, hoh_NWI_RIVonly[hoh_NWI_RIVonly$Shape_Area>-9])
#CC_noRIV_NWI_mask <- mask(CC_FULL, CC_FULL_RIV_only>-999, maskvalues = 1, updatevalue = NA)

#These are WIP carbon maps without the NWI Riverine wetlands
CC_FULL_noRIV_NWI_WIP_mask <- rast("SOIL CARBON/CC_FULL_noRIV_NWI_WIP_mask.tif")
CC_1M_noRIV_NWI_WIP_mask <- rast("SOIL CARBON/CC_1M_noRIV_NWI_WIP_mask.tif")#mask(CC_1M_noRIV_NWI_WIP_mask, hoh_WIP_mask<0.5, maskvalues = 1, updatevalue = NA)
CC_30CM_noRIV_NWI_WIP_mask<- rast("SOIL CARBON/CC_30CM_noRIV_NWI_WIP_mask.tif")

# NLCD ####
# NLCD <- rast("NLCD/2019/NLCD/NLCD_2019_Land_Cover_L48_20210604_EhrjVzX3LPtBKSpTgmDN.tiff")
# NLCD_rpj <- NLCD |> terra::project("EPSG:26910", "near") |>
#     terra::crop(CC_area, mask = T, touches = T) |> 
#     terra::resample(CC_1M, "near") |>
#     writeRaster("NLCD/2019/NLCD2019_rspl.tif", overwrite = T)
# m <- rbind(c(90, 90), c(95, 95))
# NLCD_mask <- NLCD_rpj |> terra::classify(m, others = NA)
# NLCD_maskC <-mask(CC_FULL, NLCD_mask, filename = "SOIL CARBON/NLCD2019_maskC_FULL.tif", overwrite = T)
NLCD_maskC <- rast("SOIL CARBON/NLCD2019_maskC_FULL.tif")


#These are WIP wetland carbon minus NLCD no open water wetland carbon 
CC_1M_NLCD_no_OW_maskWIP <- rast("SOIL CARBON/CC_1M_NLCD_no_OW_maskWIP.tif")#mask(CC_1M_WET, (NLCD_maskC> -999), 
                               #maskvalues = 1, updatevalue = NA, 
                               #filename = "SOIL CARBON/CC_1M_NLCD_no_OW_maskWIP.tif", overwrite = T)
plot(CC_1M_NLCD_no_OW_maskWIP)

#Aboveground Biomass
agb <- rast("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/AGB/HOH/Hudak_AGB_WIPrspl.tif")
agb <- agb*0.5
plot(agb)
agb_crop <- crop(agb, CC_area)
agb_mask <- mask(agb_crop, CC_area)
agb_wet_mask <- mask(agb, (hoh_WIP_mask>=0.5), maskvalues = 0, updatevalue = NA)

#### The actual Carbon Function ####

C_func <- function(carbon_masked, WIP, forestCover){
        #area 
        area_test <- WIP > -999 #placeholder for cell size all cells = 1
        x <- xres(carbon_masked) #hoh = 4
        y <- yres(carbon_masked) # hoh = 4
        resol <- (x*y)/10000 # Hoh = 0.0016
        # if(terra::linearUnits(carbon_masked) == 1){
        #     resol <- resol/10000
        # } else {print("check units")}
        
        total_area <- sum(values(cellSize(area_test, unit = "ha")), na.rm = T)#Total Area in 'ha' THIS NEEDS TO BE CONSISTENT
        C_mean <- mean(values(carbon_masked), na.rm = T) #mean value of all values of Mg/ha cells
        carbon_cell <- carbon_masked*cellSize(area_test, unit = "ha") # carbon in Mg per cell which is then added up 
        #There will be small numbers here because of Mg/ha
        TotalC_sum <- sum(values(carbon_cell), na.rm =T) #Total carbon in entire area
        # #average amount overall
        # C_count <- freq(area_test, value = 1)[[3]] #number of cells, not an area in 'ha'
        # ovr_avg <- (TotalC_sum)/C_count
        
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
        
        
        upl_area <- sum(values(cellSize(C_mask0_upl_cell >-999, unit = "ha")), na.rm = T)#(C_upl_count*16)/10000 #62088.9 #<- expanse(hoh_C_mask0_upl, unit = "ha") # 62088.9ha
        wet_area <- sum(values(cellSize(C_mask0_wet_cell >-999, unit = "ha")), na.rm = T) #7417.701#<- expanse(hoh_C_mask0_wet, unit = "ha") #7417.701 ha
        bet_area <- sum(values(cellSize(C_mask0_between_cell >-999, unit = "ha")), na.rm = T)
        
        
        ## The forested wetland masking NEEDS WORK## 
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
        forW_area <- sum(values(cellSize(C_mask0_forestedWet_cell > -999, unit = "ha")), na.rm = T)
        
       
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
        
        
        cat("upland area =", upl_area, "\nwet_area =", wet_area, 
            "\nforested wetland", forW_area,
            "\nbetween area = ", bet_area, "\nTotal Area =", total_area,
            "sum of parts = ", (upl_area + wet_area + bet_area),
            "\namount in uplands Mg, Tg= ", upl_amt, ",", upl_amt/1e6, 
            "\naverage amount in uplands Mg/ha= ", avg_upl,
            "\namount in wetlands Mg, Tg= ", wet_amt,",", wet_amt/1e6, 
            "\naverage amount in wetlands Mg/ha= ", avg_wet,
            "\namount in between Mg, Tg= ", bet_amt,",", bet_amt/1e6, 
            "\naverage amount in between Mg/ha= ", avg_bet,
            "\nThe total soil Carbon in Mg, Tg is: ", TotalC_sum[[1]],",",TotalC_sum[[1]]/1e6, 
            "\nThe overall average soil Carbon Mg/ha is: ", C_mean,
            "\nwetland proportion of total carbon: ", wet_percC[[1]],
            "\nwetland proportion of land area: ", wet_percA[[1]],
            "\nupland proportion of total carbon: ", upl_percC[[1]],
            "\nupland proportion of land area: ", upl_percA[[1]], 
            "\nbetween land proportion of total carbon: ", bet_percC[[1]],
            "\nbetween land proportion of land area: ", bet_percA[[1]],
            "\namount in forested wetlands Mg, Tg= ", forW_amt,",", forW_amt/1e6,
            "\naverage amount in forested wetlands Mg/ha= ", avg_forW,
            "\nforested wetland proportion of total carbon: ", forW_percC[[1]],
            "\nforested wetland proportion of land area: ", forW_percA[[1]]
        )

    }



C_func(CC_1M, hoh_WIP_mask, hohFC_rpj)
C_func(mas_C_mask0, mas_WIP_mask)
C_func(col_C_mask0, col_WIP_mask)

#### NWI mask here ####

# NWI_extract <- function(WA_NWI, studypoly){
#     studypolyrpj <- terra::project(studypoly, crs(WA_NWI))
#     studyNWI <- terra::crop(WA_NWI, studypolyrpj)
#     writeVector(studyNWI, filename = paste0("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/", 
#                                             substr(deparse(substitute(studypoly)), 1,3),
#                                             "NWI_poly.gpkg"))
# }
# NWI <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/WA_shapefile_wetlands/WA_Wetlands.shp")
# col_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_poly.shp")
# NWI_extract(NWI, col_poly)

# hoh_NWI_rpj <- subset(hoh_NWI_rpj, hoh_NWI_rpj$WETLAND_TY != "Estuarine and Marine Wetland")
# unique((hoh_NWI_rpj$WETLAND_TY))
# hoh_NWI_shape <- hoh_NWI_rpj[, "Shape_Area"]
# plot(hoh_NWI_shape)
# plot(hoh_C_NWI_mask)

#hoh_NWI_mask0 <- mask(hoh_C_NWI_mask, (hoh_MNDWI> -0.30), maskvalues = 1, updatevalue = NA, filename = "HOH_NWI_CARBON_8_10_22_RS_mask0.tif", overwrite = T)
#plot(hoh_NWI_mask0)


#simple carbon sum calculator ####
C_map_simp<- function(C_map){
    gt <- (C_map > -999)
    area_tot <- sum(values(cellSize(gt, unit = "ha")), na.rm = T)
    C_mean <- mean(values(C_map), na.rm = T) #mean value of all values of Mg/ha cells
    carbon_cell <- C_map*cellSize(gt, unit = "ha") # carbon in Mg per cell which is then added up 
    #There will be small numbers here because of Mg/ha
    TotalC_sum <- sum(values(carbon_cell), na.rm =T)
    writeRaster(C_map, paste0("SOIL CARBON/CrypticCarbonMaps/", (deparse(substitute(C_map))), '.tif'), overwrite = T) #"SOIL CARBON/CC_FULL_noRIV_NWI_WIP_mask.tif"
    cat("Total area =", area_tot, 
        "\nAverage Carbon Stock (Mg/ha) =", C_mean, 
        "\ntotal Carbon (Tg)", TotalC_sum/1e6)
}
C_map_simp(NLCD_maskC) #This is NLCD wetlands with carbon
C_map_simp(CC_FULL_NWI_mask)#this is all NWI Wetlands with carbon
C_map_simp(CC_1M_NWI_mask) 
C_map_simp(CC_30CM_NWI_mask)
C_map_simp(CC_1M_NWInoriv_mask)
C_map_simp(CC_FULL_noRIV_NWI_WIP_mask) #inland only no riverine
C_map_simp(CC_1M_noRIV_NWI_WIP_mask) #inland only no riverine 
C_map_simp(CC_30CM_noRIV_NWI_WIP_mask) #inland only no riverine 
C_map_simp(CC_1M_NWInoriv_maskWIP) # This is WIP wetland C minus NWI wetland C areas -> CRYPTIC

C_map_simp(CC_1M_WET)# "ALL WIP Wetlands Carbon"
C_map_simp(CC_1M_NWI_All_maskWIP)# "WIP - All NWI incl. RIV" ***Makes sense that it is complete WIP
C_map_simp(CC_1M_NWInoriv_maskWIP)# "WIP - NWI No RIV"
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












#test plots
wet <- rast("hohC_mask0_wetMg.tif")
bet <- rast("hohC_mask0_betweenMg.tif")
upl <- rast("hohC_mask0_uplMg.tif")
plot(upl)



ggplot() +
    geom_spatraster(data = hoh_WIP_mask) +
    geom_spatvector(data = hoh_NWI_rpj, fill = NA)

tmap::tm_shape(hoh_WIP_mask) + 
    tm_raster(title = "WIP")

ctrf <- vect("NPCTR/Coastal Temperate Rain Forest of Western North America - Original Distribution/data/data/ctrf/ctrf.shp")
