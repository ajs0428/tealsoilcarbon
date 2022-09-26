library(terra)
library(rgdal)


setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON")
# #setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/")
# 
# 
# #import MNDWI_sum layer for masking
# MNDWI_mask_pre <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_MNDWI_SUM_R.tif")
# plot(MNDWI_mask_pre)
# MNDWI_mask <- MNDWI_mask_pre > -0.30
# 
# plot(MNDWI_mask)
# 
# #import carbon stock map
# hoh_C <- rast("HOH_CARBON_7_31_22.tif")
# plot(hoh_C)
# 
# #make masks for the river and shore
# hoh_C_mask <- mask(hoh_C, MNDWI_mask, maskvalues = 1, updatevalue = 0, #maybe update to NA?
#                    filename = "HOH_CARBON_7_31_22_mask.tif",
#                    overwrite = TRUE)
# plot(hoh_C_mask)
# 
# #hide any zeros
# zero_C_mask <- hoh_C <0
# plot(zero_C_mask)
# 
# hoh_C_mask0 <- mask(hoh_C_mask, zero_C_mask, maskvalues = 1, updatevalue = 0, #maybe update to NA?
#                     filename = "HOH_CARBON_7_31_22_mask0.tif",
#                     overwrite = TRUE)

#### START HERE ####
hoh_C_mask0 <- rast("HOH_CARBON_8_9_22_RSmask0.tif") #CHANGE THIS AFTER LOOKING AT MODEL AGAIN

plot(hoh_C_mask0)

# test <- hoh_C_mask0 > -5
# plot(test)
# x <- xres(test)
# y <- yres(test)
# resol <- (x*y)/10000
# test_area <- sum(values(test)*resol, na.rm = T)
# test_carbon_cell <- hoh_C_mask0*resol
# test_TotalC_sum <- sum(values(test_carbon_cell), na.rm =T)
# test_C_mask0_upl <- mask(test_carbon_cell, WIP_upl_mask, maskvalues = 1, updatevalue = NA) #need from below
# C_upl <- global(test_C_mask0_upl, fun ="sum", na.rm = T)[[1]]
# upl_amt <- C_upl/test_area 
#### Sum of Carbon and area 

#area <- #expanse(area_tif, unit = "ha") #This is total area Takes forever <- 69413.24 ha 
area_tif <- rast("area")
test_area <- terra::cellSize(area_tif, unit = "ha")
plot(test_area)
total_area_cellsum <- sum(values(test_area), na.rm = T) #This is total area measured by terra: 69413.24ha

size_of_cell_ha <- 0.0016

#find the carbon in Mg in each cell
hoh_C_mask0_cell <- hoh_C_mask0*size_of_cell_ha #carbon AMOUNT in each cell
(hoh_C_mask0_cell_sum <- sum(values(hoh_C_mask0_cell), na.rm = T)) #Total Carbon Mg
(hoh_C_mask0_cell_sumTg <- hoh_C_mask0_cell_sum/1e6) #total amount in Teragrams


#C_area <- sum(values(hoh_C_mask0), na.rm = T)#global(hoh_C_mask0, fun ="sum", na.rm = T) #this is total carbon added up
C_mean <- mean(values(hoh_C_mask0), na.rm = T) #This is the mean value of Carbon (Mg/ha) in each cell

#THE SUM TOTAL ALL AREA 
#C_sum <- C_area/69413.24 #69594.74 #178.193 7/31/22 Revised #171.226Gg AREA REVISED FROM 16m^2 cells method 69413.24 # 171443.3 or 171.443 Gg



#WIP Masks for carbon in wetlands vs uplands

WIP <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_WIP_v8_invert.tif")

WIP_wet_mask <- WIP >=0.25 #The uplands go to 1, wetlands go to 0
WIP_upl_mask <- WIP<=0.75 #The wetlands go to 1, uplands go to 0
WIP_between <- WIP<=0.75 & WIP>=0.25
plot(WIP, main = "WIP")
plot(WIP_upl_mask, main = "upl are 0, wet are 1")
plot(WIP_wet_mask, main = "wet are 0, upl are 1")
plot(WIP_between, main = "WIP between")

hoh_C_mask0_upl <- mask(hoh_C_mask0_cell, WIP_upl_mask, maskvalues = 1, updatevalue = NA,
                        filename = "HOH_CARBON_7_31_22_WIP_upl_maskcell.tif",
                        overwrite = TRUE)
hoh_C_mask0_wet <- mask(hoh_C_mask0_cell, WIP_wet_mask, maskvalues = 1, updatevalue = NA, # This takes the mask values of 1 and turns them into NA, showing Wetlands
                        filename = "HOH_CARBON_7_31_22_WIP_wet_maskcell.tif", #So where WIP_wet_mask has uplands as 1, those go to NA and returns only the wetland carbon values 
                        overwrite = TRUE)
hoh_C_mask0_between <- mask(hoh_C_mask0_cell, WIP_between, maskvalues = 0, updatevalue = NA,
                        filename = "HOH_CARBON_7_31_22_WIP_bet_maskcell.tif",
                        overwrite = TRUE)
plot(hoh_C_mask0_wet, main = "wetland <0.25")
plot(hoh_C_mask0_upl, main = "upland >0.75")
plot(hoh_C_mask0_between)

#CHANGE THESE - These are carbon stocks in areas
C_total <- (global(hoh_C_mask0_cell, fun = "sum", na.rm = T)[[1]])/1e3 #
C_upl <- (global(hoh_C_mask0_upl, fun ="sum", na.rm = T)[[1]])/1e3 #
C_wet <- (global(hoh_C_mask0_wet, fun ="sum", na.rm = T)[[1]])/1e3 # 
C_bet <- (global(hoh_C_mask0_between, fun ="sum", na.rm = T)[[1]])/1e3

WIP_gt0 <- WIP > -1
C_total_count <- freq(WIP_gt0, value = 1)[,3] #total cell count
C_upl_count<- freq(WIP_upl_mask, value = 0)[,3] #- min(cells(hoh_C_mask0_upl)) # This is the # of cells in UPL areas: 
C_wet_count <- freq(WIP_wet_mask, value = 0)[,3]# - min(cells(hoh_C_mask0_wet)) # This is the sum of all Carbon in UPL areas:  
C_bet_count <- freq(WIP_between, value = 1)[,3] #MASK VALUES =1
sum(C_upl_count, C_wet_count, C_bet_count)

(C_total_count*16)/10000 #total area 
(C_upl_count*16)/10000 # This is the upland area calculated from 16m^2 cells: 62251.47
(C_wet_count*16)/10000 # This is the wetland area calculated from 16^2 cells: 7343.275 
(C_bet_count*16)/10000 #between area
#((C_upl_count*16)/10000) + ((C_wet_count*16)/10000) #This is the total area calculated from 16^2 cells: 69594.74

#need to do expanse for the areas but this is a long process

upl_area<- (C_upl_count*16)/10000 #62088.9 #<- expanse(hoh_C_mask0_upl, unit = "ha") # 62088.9ha
wet_area <- (C_wet_count*16)/10000 #7417.701#<- expanse(hoh_C_mask0_wet, unit = "ha") #7417.701 ha
bet_area <- (C_bet_count*16)/10000
#total_area = ((C_upl_count*16)/10000) + ((C_wet_count*16)/10000)#((C_upl_count*16)/10000) + ((C_wet_count*16)/10000)#upl_area + wet_area #Total area

#amount in uplands
C_upl/total_area_cellsum #153.678Gg Revised 7/31/22 #143.982Gg or using the different expanse method -> #total_area # 144.164Gg
#average amount in uplands?
(C_upl*1000*(1/0.0016))/C_upl_count #274.1117Mg/ha Revised 7/31/22 #257.5465Mg/ha
#amount in wetlands
C_wet/total_area_cellsum #27.243Gg or using the different expanse method ->#total_area # 27.2785Gg
#average amount in wetlands
(C_wet*1000*(1/0.0016))/C_wet_count # 413.1205Mg/ha
#amount in betweem
C_bet/total_area_cellsum #33.806Gg
#average amount in between
(C_bet*1000*(1/0.0016))/C_bet_count # 370.780Mg/ha
#Total Carbon = 
#(C_upl/total_area_cellsum) + (C_wet_area/total_area_cellsum) # Total is 171.443Gg


(C_wet)/C_total #C_wet_area[[1]]/C_area #This is the wet area carbon proportion of total carbon
        # 0.1591109%
(C_upl)/C_total # This is the upl area carbon proportion of the total carbon
        # 0.8408891%
(C_bet)/C_total # This is the between area carbon proportion of the total carbon
        #19.744%                            
wet_area/total_area_cellsum #This is the wetland area proportion of total
        #0.1067194%
upl_area/total_area_cellsum #This is the upland area proportion of total
        #0.8932806%
bet_area/total_area_cellsum #This is the upland area proportion of total
        #0.1458844%

#Mask the areas that are not floodplin to get the floodplain
GEO <- rast("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_R_GEO_CROP_AGG.tif")
plot(GEO)

#make GEO mask 
GEO_mask <- GEO == "Holocene-Quaternary"
plot(GEO_mask)

hoh_C_RIV_mask <- mask(hoh_C_mask0_wet, GEO_mask, maskvalues = 0, updatevalue = NA, # This takes the mask values of 1 and turns them into NA
                        filename = "HOH_CARBON_GEO_RIV_mask.tif", #So where WIP_wet_mask has uplands as 1, those go to NA and returns only the wetland carbon values 
                        overwrite = TRUE)
plot(hoh_C_RIV_mask)

C_RIV_area <- global(hoh_C_RIv_mask, fun ="sum", na.rm = T) # This is the sum of all Carbon in RIV areas:
RIV_count<- freq(hoh_C_RIv_mask, value = 0)[,3] #- min(cells(hoh_C_mask0_upl)) # This is the # of cells in RIV areas:
RIV_area <- ((RIV_count*16)/10000) #760.6576ha
RIV_area/wet_area #10.25% of wetland area

#amount in Riverine area
C_RIV_area/total_area #4.339Gg/ha
RIV_area/total_area #1.09% of area
(C_RIV_area/total_area)/C_sum # 2.53% of Carbon
(C_RIV_area/total_area)/(C_wet_area/69594.74)





