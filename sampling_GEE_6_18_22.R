library(terra)
library(tmap)
library(rgdal)
library(tmaptools)
library(sf)
library(dplyr)
library(stringr)

setwd("OneDrive - UW/University of Washington/Data and Modeling/")
path <- "SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/"

#Reading in multiband image
test <- rast("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/all_spec_5yr_m00.tif")
plot(test$ST_B10)

#List of images per study area
imglist_hoh <- list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/",
                          pattern = "hoh_.*")
hoh_str <- "hoh_.*"
imglist_mas <- list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/",
                          pattern = "mas_.*")
imglist_col <- list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/",
                          pattern = "col_.*")
test <- rast(paste0("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/", imglist_col[[1]]))
plot(test)

# Read in points and make into the right vector for sampling
table <- read.csv("SOIL CARBON/ANALYSIS/ALL_SOILC_PTS.csv") 
hoh_dat <- subset(pts, STUDY == "HOH")
mas_dat <- subset(pts, STUDY == "MAS")
col_dat <- subset(pts, STUDY == "COL")

#pts <- vect(table, geom = c('lon', 'lat'), crs = "EPSG:2926") 
pts <- st_as_sf(table, coords = c("lon", "lat"), crs = "EPSG:2926")
plot(pts$geometry)

pts_hoh <- vect(subset(pts, STUDY == "HOH"))
pts_mas <- vect(subset(pts, STUDY == "MAS"))
pts_col <- vect(subset(pts, STUDY == "COL"))

#How to extract Thermal(SR10), NDVI, MNDWI, EVI?
# if 'test' is the raster then:  test[[c(9,21:23)]]

#The extract() function can be used to extract raster values at point locations 
#or obtain summary statistics within polygon or areal features. For areal features, 
#you must provide a desired summary metric. Results are stored as SpatVector objects.

imglist <- list.files(path, pattern = hoh_str)

pts_test <- terra::extract((terra::rast(paste0(path, imglist_hoh[[2]])))[[c(9,21:23)]], pts_hoh) 
colnames(pts_test)[2:5] <- c('B10', '1', '2', '3')
g <- paste0('ST_B10', str_extract(imglist_hoh[1], '_m..'))


#function in progress
#need to append data to sample point dataframe
sampling <- function(points, file_str, img_path){
    imglist <- list.files(img_path, pattern = file_str)
    #imglist_mas <- list.files(img_path, pattern = "mas_.*")
    #imglist_col <- list.files(img_path, pattern = "col_.*")
    imgs_paths <- paste0(img_path, imglist)  
    imgs <- terra::sds(terra::rast(img_paths))
    for img in imgs[[1:12]]
    print(imgs)
    
    #dat <- list()
    # for (file in 1:length(imglist)) {
    #     endm <- str_extract(imglist[[file]], '_m..')
    #     B10 <- paste0('ST_B10', endm)
    #     NDVI <- paste0('NDVI', endm)
    #     MNDWI <- paste0('MNDWI', endm)
    #     EVI <- paste0('EVI', endm)
    #     img <- terra::rast(paste0(img_path, imglist[[file]]))
    #     points_sampled <- terra::extract(img[[c(9,21:23)]], points)
    #     colnames(points_sampled)[2:5] <- c(B10, NDVI, MNDWI, EVI)
    #     dat[[file]] <- points_sampled #cbind(dat, points_sampled)
    # }
    # return(dat)
}

test_samp <- sampling(pts_hoh, hoh_str , path)

bind_rows(test_samp)


