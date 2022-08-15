library(ggplot2)
library(terra)
library(raster)
library(rgdal)
library(plotly)
library(rasterVis)
library(rayshader)
library(rgl)
library(viridis)
library(sf)
#library(stars)
library(RColorBrewer)
library(magick)
library(MetBrewer)

#USE METBREWER PACKAGE FOR PALETTE


#setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")
setwd("/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/") #Windows

agb <- raster::raster("AGB/WA_Biomass/AGB_Forest_NW_USA_2015.tif")
hoh_poly <- readOGR("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_POLYGON_7_11_2022/HOH_POLYGON_711.shp")
hoh_poly_rpj <- spTransform(hoh_poly, crs(agb))
wet_mask <- rast("SOIL CARBON/HOH_CARBON_7_20_22_WIP_wet_mask-Anthony's MacBook Pro-2.tif")
upl_mask <- rast('SOIL CARBON/HOH_CARBON_7_20_22_WIP_upl_mask.tif')
carbon <- rast("SOIL CARBON/HOH_CARBON_7_20_22_mask.tif")
dem <- raster::raster("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_dtm_resamp_7_23.tif")#raster::resample(dem, carbon, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_dtm_resamp_7_23.tif")#
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_fill.tif")
#slp <- raster::raster("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/")
#asp <- raster::terrain(dem, opt = "aspect", filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_aspect.tif", overwrite = T)
hs <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_HILLSHADE.tif")#raster::hillShade(slp, asp, 40, 270, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_HILLSHADE.tif", overwrite = T)

#agb_rpj <- project(agb, crs(hoh_poly), filename = "AGB/WA_Biomass/HOH_rpj_AGB_Forest_NW_USA_2015.tif", overwrite = T)
agb_crop_mask <- raster::mask(agb, hoh_poly_rpj, filename = "AGB/WA_Biomass/HOH_rpj_crop_mask_AGB_Forest_NW_USA_2015.tif", overwrite = TRUE)
#agb_rpj <- rast("AGB/WA_Biomass/HOH_rpj_AGB_Forest_NW_USA_2015.tif")
#values(agb_rpj) <- values(agb) 

# #wet_mat <- NULL#raster::as.matrix(wet_mask, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/hoh_wet_mat.mtx")
# #upl_mat <- raster_to_matrix(upl_mask)
carbon_mat <- as.matrix(carbon, na.rm = TRUE)#as.matrix(carbon, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/carbon_mat") #as.matrix(carbon)
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

#work on this
agbC <- gplot(agb_rpj, maxpixels=3500000) + geom_raster(aes(fill = value*0.5)) +
    scale_fill_viridis("Aboveground Carbon \nStock Mg/ha \n \n", 
                       option = "viridis", 
                       na.value = "white", 
                       limits = c(0, 400)) +
    theme_void()

WIP_plot <- gplot(WIP, maxpixels=3500000) + geom_raster(aes(fill = value)) +
  scale_fill_gradientn("Wetland To Upland", 
                       colors = met.brewer("VanGogh3", direction = -1), 
                       na.value = "white", 
                       limits = c(0, 1)) +
  theme(legend.position="Top", legend.box = "horizontal",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) #+ theme_void()

allC <- gplot(carbon, maxpixels=3500000) + geom_raster(aes(fill = value)) +
    scale_fill_gradientn("Soil Carbon Stock Mg/ha", 
                         colors = met.brewer('Nattier', override.order = T, 6), 
                         na.value = "white", 
                         limits = c(0, 800)) +
  theme(legend.position="bottom", legend.box = "horizontal",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) #+ theme_void()

uplC <- gplot(wet_mask, maxpixels=3500000) + geom_raster(aes(fill = value)) +
  scale_fill_viridis("Soil Carbon \nStock Mg/ha \n \n", 
                     option = "magma", 
                     na.value = "white", 
                     limits = c(0, 800)) +
  theme_void()
wetC <- gplot(upl_mask, maxpixels=3500000) + geom_raster(aes(fill = value)) +
  scale_fill_viridis("Soil Carbon \nStock Mg/ha \n \n", 
                     option = "magma", 
                     na.value = "white", 
                     limits = c(0, 800)) +
  theme_void()

plot_gg(agbC, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 1, scale = 125,zoom = 0.6, phi = 30, theta = 30)
render_snapshot('SOIL CARBON/ANALYSIS/R/Graphics/AGC_HOH_3D.png', clear = T)

plot_gg(WIP_plot, shadow_intensity =1, multicore = TRUE, raytrace = FALSE, width = 6, height = 4, 
        pointcontract = 1, scale = 1,zoom = 0.6, phi = 30, theta = 30,
        invert = F, reduce_size = 0.4)
render_snapshot('SOIL CARBON/ANALYSIS/R/Graphics/WIP_3D.png', clear = T)
save_obj('SOIL CARBON/ANALYSIS/R/Graphics/All_Carbon.obj')
plot_gg(allC, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 1, scale = 150,zoom = 0.6, phi = 30, theta = 30,
        invert = F, reduce_size = 0.4)
render_snapshot('SOIL CARBON/ANALYSIS/R/Graphics/All_Carbon3D_Degas.png', clear = T)

plot_gg(uplC, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 125,zoom = 0.6, phi = 30, theta = 30)

plot_gg(wetC, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 125,zoom = 0.6, phi = 30, theta = 30)


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
