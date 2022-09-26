library(ggplot2)
library(terra)
library(raster)
library(rgdal)
library(plotly)
library(rasterVis)
library(rayshader)
library(magick)
library(rgl)
library(viridis)
library(sf)
#library(stars)
library(RColorBrewer)
library(magick)
library(MetBrewer)
library(webshot)
library(webshot2)

#USE METBREWER PACKAGE FOR PALETTE


setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
#setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/") #Windows

agb_hoh12 <- rast("AGB/WA_Biomass/hoh_2012.tif")
agb_hoh13 <- rast("AGB/WA_Biomass/hoh_2013.tif")
hoh_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.shp")
hoh_poly_rpj <- terra::project(hoh_poly, "EPSG:5070")
#wet_mask <- rast("SOIL CARBON/HOH_CARBON_")
#$upl_mask <- rast('SOIL CARBON/HOH_CARBON_7_20_22_WIP_upl_mask.tif')
#### Main Layers ####
soil_C <- rast("SOIL CARBON/HOH_CARBON_8_9_22_RSmask0.tif")
upl_soil_C <- rast("SOIL CARBON/hohC_mask0_upl.tif")
wet_soil_C <- rast("SOIL CARBON/hohC_mask0_wet.tif")
bet_soil_C <- rast("SOIL CARBON/hohC_mask0_between.tif")
NWI_soil_C <- rast("SOIL CARBON/HOH_NWI_CARBON_8_10_22_RS_mask0.tif")
SG_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_soilgrids.tif")
SG_wet_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_WIP_wet_soilgrids.tif")
#dem <- raster::raster("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_dtm_resamp_7_23.tif")#raster::resample(dem, carbon, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_dtm_resamp_7_23.tif")#
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
#slp <- raster::raster("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/")
#asp <- raster::terrain(dem, opt = "aspect", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_aspect.tif", overwrite = T)
#hs <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_HILLSHADE.tif")#raster::hillShade(slp, asp, 40, 270, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_HILLSHADE.tif", overwrite = T)

#agb_rpj <- project(agb, crs(hoh_poly), filename = "AGB/WA_Biomass/HOH_rpj_AGB_Forest_NW_USA_2015.tif", overwrite = T)
#agb_crop <- terra::crop(agb, hoh_poly_rpj)
#agb_mask <- terra::mask(agb_crop, hoh_poly_rpj)
#plot(agb_hoh13)#agb_rpj <- rast("AGB/WA_Biomass/HOH_rpj_AGB_Forest_NW_USA_2015.tif")
#values(agb_rpj) <- values(agb) 
#agb_hoh <- terra::merge(agb_hoh12, agb_hoh13)
agb_hoh_rpj <- rast("AGB/HOH/Hudak_Lidar_AGB.tif") #terra::project(agb_hoh, crs(WIP), filename = "AGB/HOH/Hudak_Lidar_AGB.tif")
# #wet_mat <- NULL#raster::as.matrix(wet_mask, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_wet_mat.mtx")
# #upl_mat <- raster_to_matrix(upl_mask)
#carbon_mat <- as.matrix(carbon, na.rm = TRUE)#as.matrix(carbon, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/carbon_mat") #as.matrix(carbon)
# 
# 
# #plot(hs, col=grey(0:100/100), legend=FALSE)
# #plot(dem, col=terrain.colors(255, alpha = 0.3), add = T)
# raster::extent(carbon) <- c(0, 800, 0, 600)
# 
# #This works
# rasterVis::plot3D(carbon, rev = F, 
#                   col = colorRampPalette(viridis(10, option = "magma")))
# 
# rgl.snapshot('ANALYSIS/R/Graphics/All_Carbon.png', fmt = 'png')

wid <- (ext(agb_hoh_rpj)$xmax - ext(agb_hoh_rpj)$xmin)/10000
hei <- (ext(agb_hoh_rpj)$ymax - ext(agb_hoh_rpj)$ymin)/10000
#work on this
(agbC <- gplot(agb_hoh_rpj, maxpixels=200000) + geom_tile(aes(fill = value*0.5)) +
    scale_fill_gradientn("Aboveground Carbon \nStock Mg/ha \n \n", 
                         colors = brewer.pal(9, "Greens"), 
                       na.value = "white", 
                       limits = c(0, 800)) +
    theme(legend.position="bottom", legend.box = "horizontal",
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()))
    #theme_void())

(WIP_plot <- gplot(WIP, maxpixels=200000) + geom_tile(aes(fill = value)) +
  scale_fill_gradientn("Wetland To Upland\n", 
                       #colors = met.brewer("Isfahan1", direction = 1, type = "continuous")[3:8], 
                       colors = brewer.pal(9, "Blues"),
                       na.value = "white", 
                      limits = c(0, 1)) + 
  theme(legend.position="bottom", legend.box = "horizontal",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())) #+
  #theme_void())

#### Color schemes ####
allC_col <- scale_fill_gradientn("Soil Carbon Stock Mg/ha", 
                         colors = met.brewer('Demuth', type = "continuous", direction = -1)[3:10], 
                         na.value = "white", 
                         limits = c(0, 800))
uplC_col <- scale_fill_gradientn("Soil Carbon \nStock Mg/ha \n \n", 
                               colors = met.brewer('Demuth', type = "continuous", direction = -1)[3:10], 
                               na.value = "white", 
                               limits = c(0, 800)) 
wetC_col <- scale_fill_gradientn("Soil Carbon \nStock Mg/ha \n \n", 
                               colors = met.brewer('Demuth', type = "continuous", direction = -1)[3:10], 
                               na.value = "white", 
                               limits = c(0, 800))
betC_col <- scale_fill_gradientn("Soil Carbon \nStock Mg/ha \n \n", 
                               colors = met.brewer('Demuth', type = "continuous", direction = -1)[3:10], 
                               na.value = "white", 
                               limits = c(0, 800)) 
NWIC_col <- scale_fill_viridis("Soil Carbon \nStock Mg/ha \n \n", 
                               option = "viridis", 
                               na.value = "white", 
                               limits = c(0, 800)) 
SGC_col <- scale_fill_viridis("Soil Carbon \nStock 30 cm Mg/ha \n \n", 
                               option = "cividis", 
                               na.value = "white", 
                               limits = c(0, 200)) 

#### Plotting function ####
TwoD_plot <- function(layer, col){
    gplot(layer, maxpixels=200000) +geom_tile(aes(fill = value)) +
        col +
        theme(legend.position="bottom", legend.box = "horizontal",
              legend.background = element_rect(fill = "white"),
              legend.key = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = "white"))
}
(soilC_plt <- TwoD_plot(soil_C, allC_col))
(uplsoilC_plt <-TwoD_plot(upl_soil_C, allC_col))
(wetsoilC_plt <-TwoD_plot(wet_soil_C, allC_col))
(betsoilC_plt <-TwoD_plot(bet_soil_C, allC_col))
(nwisoilC_plt <-TwoD_plot(NWI_soil_C, allC_col))
(sgsoilC_plt <-TwoD_plot(SG_soil_C, SGC_col))
(sgwetsoilC_plt <-TwoD_plot(SG_wet_soil_C, SGC_col))



#### Rayshader Plots ####
plot_gg(agbC, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/AGC_HOH_3D.png', fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)

plot_gg(WIP_plot, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 1,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/WIP_3D.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
#save_obj('SOIL CARBON/ANALYSIS/R/Graphics/All_Carbon.obj')

options(rgl.printRglwidget = TRUE)
#### Soil Rayshader ####
plot_gg(soilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
render_snapshot(filename = "SOIL CARBON/ANALYSIS/R/Graphics/hohsoilC_plt_check.png", clear = T)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohsoilC_plt.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
plot_gg(uplsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohuplsoilC_plt.png' ,fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
plot_gg(wetsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohwetsoilC_plt.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
plot_gg(betsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohbetsoilC_plt.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
plot_gg(nwisoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohnwisoilC_plt.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
plot_gg(sgsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohsgsoilC_plt.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)
plot_gg(sgwetsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
        pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohsgwetsoilC_plt.png', fmt = "png",
                width = 4000, height = 3600, webshot = TRUE)




degas_pal <- met.brewer("Degas", 5) 
degas <- create_texture("#591d06", "#96410e", "#e5a335", "#556219", "#418979")
carbon_mat %>%
    sphere_shade(texture = degas) %>%
    #add_water(detect_water(elmat), color = "desert") %>%
    # add_shadow(ray_shade(carbon_mat, zscale = 3), 0.5) %>%
    #add_shadow(ambient_shade(elmat), 0) %>%
    plot_3d(hs, carbon_mat, zscale = 50, triangulate = TRUE) #%>%
    #add_shadow(ray_shade(carbon_mat, zscale = 3), 0.5) %>%
    #add_shadow(ambient_shade(carbon_mat), 0) %>%

render_snapshot()



# x <- gplot(wet_mask) + geom_raster(aes(x = x, y = y, fill = value)) +
#     scale_fill_viridis_c()
# plot_gg(x, multicore=TRUE)
# 
# plot3D(wet_mask)
