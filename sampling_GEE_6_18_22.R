library(terra)
library(tmap)
library(rgdal)
library(tmaptools)
library(sf)
library(dplyr)
library(stringr)
library(purrr)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
path <- "SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/"

#List of images per study area
# imglist <- list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/")
# imglist_hoh <- (list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/",
#                           pattern = "hoh_.*"))
# #hoh_str <- "hoh_.*"
# imglist_mas <- list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/",
#                           pattern = "mas_.*")
# imglist_col <- list.files("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/",
#                           pattern = "col_.*")
# test <- rast(paste0("SOIL CARBON/SPATIAL LAYERS/GEE/all_spec_GEE/", imglist_mas[[1]]))
# plot(test)

# Read in points and make into the right vector for sampling
table <- read.csv("SOIL CARBON/ANALYSIS/ALL_SOILC_PTS.csv") 
#pts <- vect(table, geom = c('lon', 'lat'), crs = "EPSG:2926") 
pts <- st_as_sf(table, coords = c("lon", "lat"), crs = "EPSG:2926")
plot(pts$geometry)

hoh_dat <- subset(table, STUDY == "HOH")
mas_dat <- subset(table, STUDY == "MAS")
col_dat <- subset(table, STUDY == "COL")

#spatial S4 datatype
pts_hoh <- vect(subset(pts, STUDY == "HOH"))
pts_mas <- vect(subset(pts, STUDY == "MAS"))
pts_col <- vect(subset(pts, STUDY == "COL"))

#How to extract Thermal(SR10), NDVI, MNDWI, EVI?
# if 'test' is the raster then:  test[[c(9,21:23)]]

#The extract() function can be used to extract raster values at point locations 
#or obtain summary statistics within polygon or areal features. For areal features, 
#you must provide a desired summary metric. Results are stored as SpatVector objects.

#           pts extract func     raster func    path list vector subset     subset cols samplepts
#               |                   |               |           |               |       |
#               v                   v               v           v               v       v
# pts_test <- terra::extract((terra::rast(paste0(path, imglist_hoh[[2]])))[[c(9,21:23)]], pts_hoh) 
# colnames(pts_test)[2:5] <- c('B10', '1', '2', '3')
# g <- paste0('ST_B10', str_extract(imglist_hoh[1], '_m..'))
# 
# test_list <- list.files(path, full.names = T,pattern = "hoh.*")
# 
# subset_me <- function(x){
#     terra::subset(x, c(9, 21:23))
# }
# 
# temp <- lapply(test_list, rast) %>%
#     lapply(. %>% terra::subset(c(9, 21:23))) %>% #works!
#     lapply(. %>% extract(., pts_hoh)) #%>%
#     #lapply(. %>% colnames(c('ID', 'ST_B10', 'NDVI', 'MNDWI', 'EVI ')))
#    
# 
# 
# test <-  terra::rast(test_list)
# test_sprc <- terra::sprc(test)
# test[[c('SR_B2', 'NDVI')]] # returns the m00 of SR_B2 and NDVI
# terra::sources(test)[[1]][1] #gets the first source file path
# terra::sources(test)[[1]][grepl("hoh.*", terra::sources(test)[[1]])] # gets the list of sources with names

#function in progress
#hoh is longer than other areas 
sampling <- function(points, img_path){
    
    imglist <- list.files(img_path, full.names = T, pattern = "*.tif")
    imglist_hoh <- str_subset(imglist, "hoh.*")
    imglist_mas <- str_subset(imglist, "mas_.*")
    imglist_col <- str_subset(imglist, "col_.*")
    
    #make points
    pts_hoh <- vect(subset(pts, STUDY == "HOH"))
    pts_mas <- vect(subset(pts, STUDY == "MAS"))
    pts_col <- vect(subset(pts, STUDY == "COL"))
    
    #make extracts
    hoh_extracts <-  lapply(imglist_hoh, rast) %>% #terra::rast(paste0(img_path, imglist_hoh)) %>%
        lapply(. %>% terra::subset(c(9, 21:23))) %>%
        lapply(. %>% extract(., pts_hoh)) #%>%
        #lapply(. %>% colnames(c('ID', 'ST_B10', 'NDVI', 'MNDWI', 'EVI ')))
    mas_extracts <- lapply(imglist_mas, rast) %>% #terra::rast(paste0(img_path, imglist_mas)) %>%
        lapply(. %>% terra::subset(c(9, 21:23))) %>%
        lapply(. %>% extract(., pts_mas)) #%>%
        #lapply(. %>% colnames(c('ID', 'ST_B10', 'NDVI', 'MNDWI', 'EVI ')))
    col_extracts <- lapply(imglist_col, rast) %>% #terra::rast(paste0(img_path, imglist_col)) %>%
        lapply(. %>% terra::subset(c(9, 21:23))) %>%
        lapply(. %>% extract(., pts_col))# %>%
        #lapply(. %>% colnames(c('ID', 'ST_B10', 'NDVI', 'MNDWI', 'EVI ')))
    
    
    return(list(hoh_extracts, mas_extracts, col_extracts))
    #pts_samp <- terra::extract(imgs_hoh, points)
    #imgs <- terra::sds(terra::rast(img_paths))
    #for img in imgs[[1:12]]
    #print(imgs)
    
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

all_area_pts_ext <- sampling(pts, path)
hoh_pts_ext <- as.data.frame((test_samp[[1]][[6]]))
mas_pts_ext <- as.data.frame(test_samp[[2]][[6]])
col_pts_ext <- as.data.frame(test_samp[[3]][[6]])

all_area_pts_ext[[]]




