library(terra)
library(rgdal)


setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")

hoh_WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
mas_WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/Mashel_2022_V01/Mashel_2022_V01.tif")
col_WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")

# plot(mas_WIP, add = T)
# 
# all_csv <- read.csv("ANALYSIS/ALL_SOILC_8_6_22.csv")
# all_pts <- vect(all_csv, geom = c('lon', 'lat'))
# set.crs(all_pts, "EPSG:4326")
# all_pts_prj <- terra::project(all_pts, "EPSG:26910")
# hoh_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "HOH")
# mas_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "MAS")
# col_pts <- terra::subset(all_pts_prj, all_pts_prj$STUDY_AREA == "COL")
# 
# hoh_pts_ext <- terra::extract(hoh_WIP, hoh_pts)
# mas_pts_ext <- terra::extract(mas_WIP, mas_pts)
# col_pts_ext <- terra::extract(col_WIP, col_pts)
# hcm <- c( col_pts_ext$Band_1, hoh_pts_ext$WET, mas_pts_ext$WET)
# all_csv$WIP_INV <- hcm
# write.csv(all_csv, file = "ANALYSIS/ALL_SOILC_8_7_22_NEWWIP.csv")

hoh_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")
mas_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_MNDWI_SUM_R.tif")
col_MNDWI <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_MNDWI_SUM.tif")
plot(hoh_MNDWI> -0.30)

hoh_WIP_mask <- mask(hoh_WIP, (hoh_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
mas_WIP_mask <- mask(mas_WIP, (mas_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)
col_WIP_mask <- mask(col_WIP, (col_MNDWI>-0.30), maskvalues = 1, updatevalue = NA)


#import carbon stock map
hoh_C <- rast("HOHMOD_CARBON_8_10_22_RanSLP.tif")
mas_C <- rast("MAS_CARBON_8_10_22_RanSLP_1M.tif")
col_C <- rast("COL_CARBON_8_10_22_ASM_1M.tif")
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
hoh_C_mask0 <- mask_func(hoh_MNDWI, hoh_C, "HOHMOD_CARBON_8_10_22_RS_1M_mask0.tif")
mas_C_mask0 <- mask_func(mas_MNDWI, mas_C, "MAS_CARBON_8_10_22_RS_1M_mask0.tif")
col_C_mask0 <- mask_func(col_MNDWI, col_C, "COL_CARBON_8_10_22_RS_1M_mask0.tif")

hoh_C_mask0 <- rast("HOH_CARBON_8_9_22_RSmask0.tif")#mask_func(hoh_MNDWI, hoh_C, "HOH_CARBON_7_31_22_mask0.tif")
mas_C_mask0 <- rast("MAS_CARBON_8_9_22_RSmask0.tif")#mask_func(mas_MNDWI, mas_C, "MAS_CARBON_7_31_22_mask0.tif")
col_C_mask0 <- rast("COL_CARBON_8_9_22_RSmask0.tif")#mask_func(col_MNDWI, col_C, "COL_CARBON_7_31_22_mask0.tif")

plot(hoh_C_mask0)
plot(mas_C_mask0)
plot(col_C_mask0)


#### maybe do the NWI mask here ####

# NWI <- vect("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/NWI/WA_shapefile_wetlands/WA_Wetlands.shp")
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
hoh_C_NWI_mask <- terra::mask(hoh_C_mask0, hoh_NWI_shape)
plot(hoh_C_NWI_mask)


hoh_NWI_mask0 <- mask(hoh_C_NWI_mask, (hoh_MNDWI> -0.30), maskvalues = 1, updatevalue = NA, filename = "HOH_NWI_CARBON_8_10_22_RS_mask0.tif", overwrite = T)
plot(hoh_NWI_mask0)

hoh_NWI_mask0 <- rast("HOH_NWI_CARBON_8_10_22_RS_mask0.tif")
NWI_1 <- (hoh_NWI_mask0 >= 0)
resol <- ((xres(hoh_NWI_mask0)*yres(hoh_NWI_mask0))/10000)
NWI_area<- sum(values(NWI_1)*resol, na.rm = T)
NWI_C <- global(hoh_NWI_mask0*resol, fun ="sum", na.rm = T)[[1]]



C_func <- function(carbon_masked, WIP){
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
    carbon_cell <- carbon_masked*resol
    TotalC_sum <- sum(values(carbon_cell), na.rm =T)
    
    
    WIP_wet_mask <- WIP <=0.5 #The uplands go to 1, wetlands go to 0
    WIP_upl_mask <- WIP>=0.5 #The wetlands go to 1, uplands go to 0
    WIP_between <- WIP<=0.75 & WIP>=0.25
    
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
    
    #C_sum <- TotalC_sum/total_area
    
    wet_percC <- (C_wet)/TotalC_sum #C_wet_area[[1]]/C_area #This is the wet area carbon proportion of total carbon
    upl_percC <- C_upl/TotalC_sum # This is the upl area carbon proportion of the total carbon
    bet_percC <- (C_bet)/TotalC_sum # This is the between area carbon proportion of the total carbon
    wet_percA <- wet_area/total_area #This is the wetland area proportion of total
    upl_percA <- upl_area/total_area #This is the upland area proportion of total
    bet_percA <- bet_area/total_area #This is the upland area proportion of total
    
    
    cat("upland area =", upl_area, "\nwet_area =", wet_area,
        "\nbetween area = ", bet_area, "\nTotal Area =", total_area,
        "\namount in uplands Mg, Tg= ", upl_amt, ",", upl_amt/1e6, "\naverage amount in uplands Mg/ha= ", avg_upl,
        "\namount in wetlands Mg, Tg= ", wet_amt,",", wet_amt/1e6, "\naverage amount in wetlands Mg/ha= ", avg_wet,
        "\namount in between Mg, Tg= ", bet_amt,",", bet_amt/1e6, "\naverage amount in between Mg/ha= ", avg_bet,
        "\nThe total soil Carbon in Mg, Tg is: ", TotalC_sum[[1]],",",TotalC_sum[[1]]/1e6, 
        "\nwetland proportion of total carbon: ", wet_percC[[1]],
        "\nwetland proportion of land area: ", wet_percA[[1]],"\nupland proportion of total carbon: ", upl_percC[[1]],
        "\nupland proportion of land area: ", upl_percA[[1]], "\nbetween land proportion of total carbon: ", bet_percC[[1]],
        "\nbetween land proportion of land area: ", bet_percA[[1]])
}


C_func(hoh_C_mask0, hoh_WIP_mask)
C_func(mas_C_mask0, mas_WIP_mask)
C_func(col_C_mask0, col_WIP_mask)

