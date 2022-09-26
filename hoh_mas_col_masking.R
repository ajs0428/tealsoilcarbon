library(terra)
library(rgdal)


setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")

#### Import WIP ####
hoh_WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
mas_WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/Mashel_2022_V01/Mashel_2022_V01.tif")
col_WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")

plot(hoh_WIP)
terra::zoom(hoh_WIP)

# th <- rast("SPATIAL LAYERS/Hoh_TreeHeight.tif") 
# all_csv <- read.csv("ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")
# hoh_csv <- subset(all_csv, all_csv$STUDY_AREA == "HOH")
# all_pts <- vect(all_csv, geom = c('lon', 'lat'))
# set.crs(all_pts, "EPSG:4326")
# all_pts_prj <- terra::project(all_pts, "EPSG:26910")
# hoh_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "HOH")
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


#### Import MNDWI ####
hoh_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")
mas_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM_R.tif")
col_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif")
plot(hoh_MNDWI< -0.65 & hoh_MNDWI> -0.30)
#need these
hoh_WIP_mask <- mask(hoh_WIP, (hoh_MNDWI>-0.30),  maskvalues = 1, updatevalue = NA)
mas_WIP_mask <- mask(mas_WIP, (mas_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
col_WIP_mask <- mask(col_WIP, (col_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)


#### DON'T START HERE unless necessary ####
## import UNMASKED carbon stock map ##
hoh_C <- rast("HOHMOD_CARBON_8_10_22_RanSLP.tif")
mas_C <- rast("MAS_CARBON_8_9_22_RanSLP.tif")
col_C <- rast("COL_CARBON_8_9_22_RanSLP.tif")
plot(mas_C< (0))

#### Masking Function ####
mask_func <- function(MNDWI, CARBON, fileName){
    MNDWI_mask <- MNDWI > -0.30
    CARBON_0mask <- CARBON <0
    
    CARBON_0 <- mask(CARBON, CARBON_0mask, maskvalues = 1, updatevalue =NA)
    CARBON_MNDWI <- mask(CARBON_0, MNDWI_mask, maskvalues = 1, updatevalue = NA)
    writeRaster(CARBON_MNDWI, filename = fileName, overwrite = T)
    CARBON_MNDWI0 <- rast(fileName)
    return(CARBON_MNDWI0)
}

#### Carbon masking ####
#hoh_C_mask0 <- mask_func(hoh_MNDWI, hoh_C, "HOHMOD_CARBON_8_10_22_RS_1M_mask0.tif")
#mas_C_mask0 <- mask_func(mas_MNDWI, mas_C, "MAS_CARBON_8_10_22_RS_1M_mask0.tif")
#col_C_mask0 <- mask_func(col_MNDWI, col_C, "COL_CARBON_8_10_22_RS_1M_mask0.tif")


####Can START HERE For latest masked carbon output ####
hoh_C_mask0 <- rast("HOH_CARBON_8_9_22_RSmask0.tif")#mask_func(hoh_MNDWI, hoh_C, "HOH_CARBON_7_31_22_mask0.tif")
mas_C_mask0 <- rast("MAS_CARBON_8_9_22_RSmask0.tif")#mask_func(mas_MNDWI, mas_C, "MAS_CARBON_7_31_22_mask0.tif")
col_C_mask0 <- rast("COL_CARBON_8_9_22_RSmask0.tif")#mask_func(col_MNDWI, col_C, "COL_CARBON_7_31_22_mask0.tif")

plot(hoh_C_mask0)
plot(mas_C_mask0)
plot(col_C_mask0)

# substr(deparse(substitute(hoh_C_mask0)), 1,3)
# paste0(deparse(substitute(hoh_C_mask0)), "mask")


#### soilgrids250m ####
sg <- rast('OTHER_DATA/soilgrids250m.tif')
sg_rpj <- terra::project(sg, crs(hoh_C_mask0))
sg_rpj_rspl <- resample(sg_rpj, hoh_WIP)
hoh_poly <- terra::aggregate(as.polygons((hoh_WIP*0+1)))
hoh_sg <- mask(sg_rpj_rspl, hoh_poly, filename = "OTHER_DATA/Hoh_soilgrids.tif", overwrite =T)
#hoh_sg_rspl <- resample(hoh_sg, hoh_WIP)
plot(hoh_sg)

#ext(hoh_sg_rspl) <- ext(hoh_WIP)
hoh_sg_WIP <- mask(hoh_sg, (hoh_WIP>=0.5), maskvalues = 0, updatevalue = NA, filename ="OTHER_DATA/Hoh_WIP_wet_soilgrids.tif" )

plot(hoh_sg_WIP)

#total SoilGrids carbon
hoh_sg_1 <- hoh_sg >0
resol <- ((xres(hoh_sg)*yres(hoh_sg))/10000)
hoh_sg_area<- sum(values(hoh_sg_1)*resol, na.rm = T)
hoh_sg_C <- global(hoh_sg*resol, fun ="sum", na.rm = T)[[1]]
(hoh_sg_C_Tg <- hoh_sg_C/1e6) #total
(hoh_sg_C/hoh_sg_area) #average Mg/ha

#wetland SoilGrids Carbon
hoh_sg_WIP_1 <- hoh_sg_WIP > 0
hoh_sgWIP_area<- sum(values(hoh_sg_WIP_1)*resol, na.rm = T)
hoh_sgWIP_C <- global(hoh_sg_WIP*resol, fun ="sum", na.rm = T)[[1]]
(hoh_sgWIP_C_Tg <- hoh_sgWIP_C/1e6) #total
(hoh_sgWIP_C/hoh_sgWIP_area) #average Mg/ha

#### NWI mask here ####

NWI_extract <- function(WA_NWI, studypoly){
    studypolyrpj <- terra::project(studypoly, crs(WA_NWI))
    studyNWI <- terra::crop(WA_NWI, studypolyrpj)
    writeVector(studyNWI, filename = paste0("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/", 
                                            substr(deparse(substitute(studypoly)), 1,3),
                                            "NWI_poly.gpkg"))
}
NWI <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/WA_shapefile_wetlands/WA_Wetlands.shp")
col_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_poly.shp")
NWI_extract(NWI, col_poly)
# 
# hoh_poly <- vect("SPATIAL LAYERS/hoh_poly/Hoh_poly.shp")
# hoh_poly_rpj <- terra::project(hoh_poly, crs(NWI))
# hoh_NWI <- terra::crop(NWI, hoh_poly_rpj)


hoh_NWI_rpj <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP.shp")#terra::project(hoh_NWI, crs(hoh_WIP))
#writeVector(hoh_NWI_rpj, filename ="/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/HOH_NWI_CROP.shp")
hoh_NWI_rpj <- subset(hoh_NWI_rpj, hoh_NWI_rpj$WETLAND_TY != "Estuarine and Marine Deepwater")
hoh_NWI_rpj <- subset(hoh_NWI_rpj, hoh_NWI_rpj$WETLAND_TY != "Estuarine and Marine Wetland")
unique((hoh_NWI_rpj$WETLAND_TY))
hoh_NWI_shape <- hoh_NWI_rpj[, "Shape_Area"]
plot(hoh_NWI_shape)
hoh_C_NWI_mask <- terra::mask(hoh_C_mask0, hoh_NWI_shape, filename = "OTHER_DATA/HOH_NWI_CARBON_8_10_22_RS_mask0.tif", overwrite = T)
plot(hoh_C_NWI_mask)


#hoh_NWI_mask0 <- mask(hoh_C_NWI_mask, (hoh_MNDWI> -0.30), maskvalues = 1, updatevalue = NA, filename = "HOH_NWI_CARBON_8_10_22_RS_mask0.tif", overwrite = T)
#plot(hoh_NWI_mask0)

hoh_NWI_mask0 <- rast("HOH_NWI_CARBON_8_10_22_RS_mask0.tif") # This is Carbon in NWI areas only
NWI_1 <- (hoh_NWI_mask0 >= 0)
resol <- ((xres(hoh_NWI_mask0)*yres(hoh_NWI_mask0))/10000)
NWI_area<- sum(values(NWI_1)*resol, na.rm = T)
NWI_C <- global(hoh_NWI_mask0*resol, fun ="sum", na.rm = T)[[1]]
(NWI_C_Tg <- NWI_C/1e6) #total
(NWI_C/NWI_area) #average

#### Carbon masking statistics ####
mask(hoh_C_mask0, (WIP>=0.5 ), maskvalues = 1, updatevalue = NA)

hohFC <- rast("SPATIAL LAYERS/GEE/hoh_tree_mask_2015.tif")
hohFC_rpj <- rast("SPATIAL LAYERS/GEE/hoh_tree_mask_2015_rpj.tif")#terra::project(hohFC, hoh_WIP, filename = "SPATIAL LAYERS/GEE/hoh_tree_mask_2015_rpj.tif")

testwipmask <- mask(hoh_WIP, (hoh_WIP >=0.5), maskvalues= 0, updatevalue = NA)
testhohFCmask <- (hohFC_rpj<50)
testfwmask <- mask(testwipmask, testhohFCmask, maskvalues = 1, updatevalue = NA)

C_func <- function(carbon_masked, WIP, forestCover){
        #area 
        area_test <- carbon_masked > -5 #placeholder for cell size 
        x <- xres(carbon_masked)
        y <- yres(carbon_masked)
        resol <- (x*y)/10000
        # if(terra::linearUnits(carbon_masked) == 1){
        #     resol <- resol/10000
        # } else {print("check units")}
        
        total_area <- sum(values(area_test)*resol, na.rm = T)
        C_mean <- mean(values(carbon_masked), na.rm = T)
        carbon_cell <- carbon_masked*resol # carbon per cell which is then added up
        #There will be small numbers here because of Mg/ha
        TotalC_sum <- sum(values(carbon_cell), na.rm =T) #Total carbon in entire area
        #average amount overall
        C_gt_neg1 <- carbon_masked > -1
        C_count <- freq(C_gt_neg1, value = 1)[[3]]
        ovr_avg <- (TotalC_sum*(1/0.0016))/C_count
        
        
        WIP_upl_mask <- WIP>=0.5 #The wetlands go to 1, uplands go to 0
        WIP_wet_mask <- WIP <=0.5 #The uplands go to 1, wetlands go to 0
        WIP_between <- WIP<=0.75 & WIP>=0.25
        
        
        
        C_mask0_uplMg <- mask(carbon_masked, WIP_upl_mask, maskvalues = 1, updatevalue = NA, 
                              filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_uplMg)), '.tif'),
                              overwrite = T)
        C_mask0_wetMg <- mask(carbon_masked, WIP_wet_mask, maskvalues = 1, updatevalue = NA, 
                              filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_wetMg)), '.tif'),
                              overwrite = T)
        C_mask0_betweenMg <- mask(carbon_masked, WIP_between, maskvalues = 0, updatevalue = NA, 
                                  filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_betweenMg)), '.tif'),
                                  overwrite = T)
        
        
        C_mask0_upl <- mask(carbon_cell, WIP_upl_mask, maskvalues = 1, updatevalue = NA)
        C_mask0_wet <- mask(carbon_cell, WIP_wet_mask, maskvalues = 1, updatevalue = NA)
        C_mask0_between <- mask(carbon_cell, WIP_between, maskvalues = 0, updatevalue = NA)
        
        
        C_upl <- global(C_mask0_upl, fun ="sum", na.rm = T)[[1]]
        C_wet <- global(C_mask0_wet, fun ="sum", na.rm = T)[[1]] 
        C_bet <- global(C_mask0_between, fun ="sum", na.rm = T)[[1]]
        
        
        C_upl_count<- freq(WIP_upl_mask, value = 0)[,3] #- min(cells(hoh_C_mask0_upl)) # This is the # of cells in UPL areas: 
        C_wet_count <- freq(WIP_wet_mask, value = 0)[,3]# - min(cells(hoh_C_mask0_wet)) # This is the sum of all Carbon in UPL areas:  
        C_bet_count <- freq(WIP_between, value = 1)[,3] #MASK VALUES =1
        
        
        upl_area <- (C_upl_count*16)/10000 #62088.9 #<- expanse(hoh_C_mask0_upl, unit = "ha") # 62088.9ha
        wet_area <- (C_wet_count*16)/10000 #7417.701#<- expanse(hoh_C_mask0_wet, unit = "ha") #7417.701 ha
        bet_area <- (C_bet_count*16)/10000
        
        #total_area = ((C_upl_count*16)/10000) + ((C_wet_count*16)/10000)#((C_upl_count*16)/10000) + ((C_wet_count*16)/10000)#upl_area + wet_area #Total area
        
        
        ## The forested wetland masking ## 
        forested <- forestCover >=50 #the forested areas are 1, nonforest are 0
        wipmask <- mask(WIP, WIP_upl_mask, maskvalues= 0, updatevalue = NA)
        fwmask <- mask(wipmask, forested, maskvalues = 0, updatevalue = NA)
        #WIP_wet_fnf <- mask(WIP, forested, maskvalues = 0, updatevalue = NA) # The upland areas are 0 and are updated to NA giving WIP 
        C_mask0_wet_forestedMg <- mask(C_mask0_wetMg, forested, maskvalues = 0, updatevalue = NA,
                                       filename = paste0(substr(deparse(substitute(carbon_masked)), 1,3),deparse(substitute(C_mask0_wet_forestedMg)), '.tif'),
                                       overwrite = T)
        C_mask0_forestedWet <- mask(C_mask0_wet, forested, maskvalues = 0, updatevalue = NA)
        C_forW <- global(C_mask0_forestedWet, fun ="sum", na.rm = T)[[1]]
        C_forW_count <- freq((fwmask >0), value = 1)[,3]
        forW_area <- (C_forW_count*16)/10000
        
        
       
        #amount in uplands
        upl_amt <- C_upl
        #average amount in uplands?
        avg_upl <- (C_upl*(1/0.0016))/C_upl_count
        #amount in wetlands
        wet_amt <- C_wet 
        #average amount in wetlands
        avg_wet<- (C_wet*(1/0.0016))/C_wet_count 
        #amount in between
        bet_amt <- C_bet
        #average amount in between
        avg_bet <- (C_bet*(1/0.0016))/C_bet_count 
        #amount in forested wetlands
        forW_amt <- C_forW
        #average amount in forested wetlands
        avg_forW <- (C_forW*(1/0.0016))/C_forW_count
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
            "\namount in uplands Mg, Tg= ", upl_amt, ",", upl_amt/1e6, 
            "\naverage amount in uplands Mg/ha= ", avg_upl,
            "\namount in wetlands Mg, Tg= ", wet_amt,",", wet_amt/1e6, 
            "\naverage amount in wetlands Mg/ha= ", avg_wet,
            "\namount in between Mg, Tg= ", bet_amt,",", bet_amt/1e6, 
            "\naverage amount in between Mg/ha= ", avg_bet,
            "\nThe total soil Carbon in Mg, Tg is: ", TotalC_sum[[1]],",",TotalC_sum[[1]]/1e6, 
            "\nThe overall average soil Carbon Mg/ha is: ", ovr_avg,
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



C_func(hoh_C_mask0, hoh_WIP_mask, hohFC_rpj)
C_func(mas_C_mask0, mas_WIP_mask)
C_func(col_C_mask0, col_WIP_mask)



th <- rast("SPATIAL LAYERS/Hoh_TreeHeight.tif")
plot(th)
