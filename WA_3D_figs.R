library(ggplot2)
library(terra)
library(plotly)
library(rasterVis)
library(rayshader)
library(rgl)
library(viridis)
library(sf)
library(RColorBrewer)
library(magick)
library(MetBrewer)
library(webshot2)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")

#### hoh data sources ####
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")#rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
agb <- rast("AGB/HOH/Hudak_AGB_WIPrspl.tif")
agb <- agb*0.5
soil_C <- rast("SOIL CARBON/CrypticCarbonMaps/CrypticCarbon1M_LMEsqrt^2_NEWGEO_2_15.tif")
#upl_soil_C <- rast("SOIL CARBON/hohC_mask0_uplMg.tif")
wet_soil_C <- rast("SOIL CARBON/CrypticCarbonMaps/CC_1M_WET.tif")
bet_soil_C <- rast("SOIL CARBON/CrypticCarbonMaps/CC_C_mask0_betweenMg-AnthonyMBP.tif")
# NWI_soil_C <- rast("SOIL CARBON/HOH_NWI_CARBON_8_10_22_RS_mask0.tif")
# SG_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_soilgrids.tif")
# SG_wet_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_WIP_wet_soilgrids.tif")

#### mas data sources ####
#uncomment for access
# WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/Mashel_2022_V01/Mashel_2022_V01.tif")
# agb <- rast("AGB/Mashel/MAS_AGB_WIPrspl.tif")
# soil_C <- rast("SOIL CARBON/MAS_CARBON_8_9_22_RSmask0.tif")
# upl_soil_C <- rast("SOIL CARBON/masC_mask0_uplMg.tif")
# wet_soil_C <- rast("SOIL CARBON/masC_mask0_wetMg.tif")
# bet_soil_C <- rast("SOIL CARBON/masC_mask0_betweenMg.tif")
#NWI_soil_C <- rast("SOIL CARBON/M") #NEED
#SG_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_soilgrids.tif") #NEED
#SG_wet_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_WIP_wet_soilgrids.tif") #NEED

#### col data sources ####
#uncomment for access
# WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")
# agb <- rast("AGB/COL/COL_AGB_WIPrspl.tif")
# col_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_nwi_poly.shp")
# soil_C <- rast("SOIL CARBON/COL_CARBON_8_9_22_RSmask0.tif")
# upl_soil_C <- rast("SOIL CARBON/colC_mask0_uplMg.tif")
# wet_soil_C <- rast("SOIL CARBON/colC_mask0_wetMg.tif")
# bet_soil_C <- rast("SOIL CARBON/colC_mask0_betweenMg.tif")
#NWI_soil_C <- rast("SOIL CARBON/M") #NEED
#SG_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_soilgrids.tif") #NEED
#SG_wet_soil_C <- rast("SOIL CARBON/OTHER_DATA/Hoh_WIP_wet_soilgrids.tif") #NEED

#dimensions
wid <- (ext(WIP)$xmax - ext(WIP)$xmin)/10000
hei <- (ext(WIP)$ymax - ext(WIP)$ymin)/10000

#### Color schemes ####
allC_col <- scale_fill_gradientn("Soil Carbon Stock Mg/ha", 
                                 colors = met.brewer('Demuth', type = "continuous", direction = -1)[3:10], 
                                 na.value = "white", 
                                 limits = c(0, 800))
WIP_col <- scale_fill_gradientn("Wetland To Upland\n", 
                                colors = brewer.pal(9, "Blues"),
                                na.value = "white", 
                                limits = c(0, 1))
NWIC_col <- scale_fill_viridis("Soil Carbon \nStock Mg/ha \n \n", 
                               option = "viridis", 
                               na.value = "white", 
                               limits = c(0, 800)) 
SGC_col <- scale_fill_viridis("Soil Carbon \nStock 30 cm Mg/ha \n \n", 
                              option = "cividis", 
                              na.value = "white", 
                              limits = c(0, 200)) 

#agb plot
(agbC <- gplot(agb, maxpixels=200000) + geom_tile(aes(fill = value)) + #remember to multiply by 0.5 for carbon
        scale_fill_gradientn("Aboveground Carbon \nStock MgC/ha \n \n", 
                             colors = brewer.pal(8, "Greens"), 
                             na.value = "white", 
                             limits = c(0, 600),
                             breaks = seq(0,600, by = 200)) +
        theme(legend.position="bottom", legend.box = "horizontal",
              panel.background = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank()))

#WIP plot
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


#### 2D carbon Plotting function ####
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

#### Execute 2D plotting ####
(soilC_plt <- TwoD_plot(soil_C, allC_col))
(uplsoilC_plt <-TwoD_plot(upl_soil_C, allC_col))
(wetsoilC_plt <-TwoD_plot(wet_soil_C, allC_col))
(betsoilC_plt <-TwoD_plot(bet_soil_C, allC_col))
#(nwisoilC_plt <-TwoD_plot(NWI_soil_C, allC_col))
#(sgsoilC_plt <-TwoD_plot(SG_soil_C, SGC_col))
#(sgwetsoilC_plt <-TwoD_plot(SG_wet_soil_C, SGC_col))


#### Rayshader Plots ####
# rayshade_func <- function(plot, scale, filename){
#     plot_gg(plot, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
#             pointcontract = 0.5, scale = scale, zoom = 0.8, phi = 35, theta = 315,
#             shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
#     # snapshot3d(filename, fmt = "png",
#     #            width = 4000, height = 3600, webshot = TRUE)
# }

#rayshade_func(agbC, 150, ('SOIL CARBON/ANALYSIS/R/Graphics/AGC_MAS_3D.png'))
#options(rgl.printRglwidget = TRUE)

plot_gg(agbC, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
#render_snapshot(filename = "agcHoh.png", software_render = T)
#rgl.postscript('SOIL CARBON/ANALYSIS/R/Graphics/AGC_Hoh_3D.eps', fmt = "eps")
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/AGC_Hoh_3D.png', fmt = "png",
        width = 6000, height = 5400, webshot = TRUE)
rgl::close3d()
rgl::open3d()
plot_gg(WIP_plot, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 1,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/COL_WIP_3D.png', fmt = "png",
           width = 6000, height = 5400, webshot = TRUE)
rgl::close3d()
#save_obj('SOIL CARBON/ANALYSIS/R/Graphics/All_Carbon.obj')

#### Soil Rayshader ####
plot_gg(soilC_plt, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hoh_soilC_plt.png', fmt = "png",
           width = 6000, height = 5400, webshot = TRUE)
rgl::close3d()
plot_gg(uplsoilC_plt, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/COL_uplsoilC_plt.png' ,fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)
rgl::open3d()
plot_gg(wetsoilC_plt, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/COL_wetsoilC_plt.png', fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)
rgl::open3d()
plot_gg(betsoilC_plt, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/COL_betsoilC_plt.png', fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)
rgl::close3d()
# plot_gg(nwisoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
#         pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
#         shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
# snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohnwisoilC_plt.png', fmt = "png",
#            width = 4000, height = 3600, webshot = TRUE)
# plot_gg(sgsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
#         pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
#         shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
# snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohsgsoilC_plt.png', fmt = "png",
#            width = 4000, height = 3600, webshot = TRUE)
# plot_gg(sgwetsoilC_plt, multicore = TRUE, raytrace = TRUE, width = wid, height = hei, 
#         pointcontract = 0.5, scale = 125,zoom = 0.8, phi = 35, theta = 315,
#         shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
# snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/hohsgwetsoilC_plt.png', fmt = "png",
#            width = 4000, height = 3600, webshot = TRUE)

####Bounding Box cut ####
typeof(ext(soil_C))
e <- ext(405000, 410000, 5295000, 5300000)
vect(e)

#### Other Rayshader ####
Cmap <- soil_C#CC_1M_NWI_mask#rast("SOIL CARBON/CC_1M_NWI_All_maskWIP_C_WET.tif")
Cmap_small <- rast("SOIL CARBON/CrypticCarbonMaps/CC_1M_WET_CROP.tif")

Cmat <- Cmap_small |> raster_to_matrix() #|> resize_matrix(0.5)
wetCmat <- wet_soil_C |> raster_to_matrix() |> resize_matrix(0.5)
betCmat <- bet_soil_C |> raster_to_matrix() |> resize_matrix(0.1)

WIP_small <- rast("SOIL CARBON/CrypticCarbonMaps/WIP_CROP.tif")
WIPmat <- WIP_small |> raster_to_matrix() #|> resize_matrix(0.1)
#WIPmat_neg <- (WIPmat)-5000

#DEM_small <- crop(DEM, DEM_vect, mask = T, filename = "SOIL CARBON/CrypticCarbonMaps/DEM_small.tif", overwrite = T)
DEM_small <- rast("SOIL CARBON/CrypticCarbonMaps/DEM_small.tif")
Dmat <- DEM_small |> raster_to_matrix()
Dambmat <- ambient_shade(Dmat, maxsearch = 100, zscale = 5)
Draymat <- ray_shade(Dmat, zscale = 5, lambert = TRUE)

water_map <- detect_water(Cmat)
ambmat <- ambient_shade(Cmat, maxsearch = 100, zscale = 5)
raymat <- ray_shade(Cmat, zscale = 5, lambert = TRUE)

wipwater_map <- detect_water(WIPmat)
wipambmat <- ambient_shade(WIPmat, maxsearch = 100, zscale = 10)
wipraymat <- ray_shade(WIPmat, zscale = 10, lambert = TRUE)


WIPmat_color <- WIPmat %>%
    height_shade(range = c(0,1),texture = brewer.pal(9, "YlGnBu")) %>%
    add_shadow(raymat, max_darken = 0.9) #%>%
    #add_shadow(ambmat, max_darken = 0.5) %>%
    #plot_map(shadow = F)
    plot_3d(WIPmat, zscale = 0.1, shadowdepth = -50, solid = F,  shadow = F, linewidth = 0.5, 
            zoom=0.75, phi=30,theta=315,fov=5)
    #add_overlay(generate_altitude_overlay(Cmat, WIPmat, 0,0)) %>%
    # plot_3d(WIPmat, zscale = 20, shadow = F, solid = F, 
    #          zoom=0.5, phi=45,theta=-45,fov=70)
render_snapshot('SOIL CARBON/Graphics/AGU2022/WIP_small.png', software_render = T, width = 8000, height = 7200)

Dmat_color <- Dmat %>%
    height_shade(texture = (grDevices::terrain.colors(50))[10:50]) %>% 
    #add_water(water_map, color = "#FFFFFF") %>%
    add_shadow(Draymat, max_darken = 0.5) %>%
    add_shadow(Dambmat, max_darken = 0.5) #%>%
    plot_3d(Dmat, zscale = 5, shadowdepth = -50, solid = F,  shadow = F, linewidth = 0.5, 
            zoom=0.75, phi=30,theta=315,fov=5) 

Cmat_color <- Cmat %>%
    height_shade(range = c(0,700), texture = MetBrewer::met.brewer("Demuth", type = "continuous", direction = -1)[c(4:10)]) %>% 
    #add_water(water_map, color = "#FFFFFF") %>%
    add_shadow(raymat, max_darken = 0.5) %>%
    add_shadow(ambmat, max_darken = 0.5) %>%
    # add_overlay(generate_scalebar_overlay(extent = ext(Cmap), length = 10000, x= 0.7,
    #                                       heightmap = Cmat, text_color = "red", thickness=1000,
    #                                       text_size=3, font = 2, text_offset = 1,
    #                                       latlong=F)) %>%
    # add_overlay(generate_compass_overlay(heightmap = Cmat, x = 0.6, text_size = 3,text_color = "red")) %>%
    # #add_overlay(overlay = WIPmat_color) %>%
    #add_overlay(Cmat) %>%
    #plot_map()
    plot_3d(Cmat, zscale = 15, shadowdepth = -50, solid = F,  shadow = F, linewidth = 0.5, 
            zoom=0.75, phi=30,theta=315,fov=5) 
render_scalebar( position = "N", color_first = "red", color_second = "gray", segments = 4)
#render_compass(position = "S", color_n = "white",)
render_floating_overlay(overlay = WIPmat_color, 
                        heightmap = WIPmat,
                        altitude = -1000, 
                        zscale = 5, remove_na = T, clear_layers = F, horizontal_offset = c(-100, -100))#, 
                     #altitude = -1, zscale = 5, remove_na = T, clear_layers = T)#,
                     #horizontal_offset = c(-1, -1))
render_floating_overlay(overlay = Dmat_color, 
                        heightmap = Dmat,
                        altitude = -2000, 
                        zscale = 5, remove_na = T, 
                        clear_layers = F, horizontal_offset = c(-200, -200))#, 
#altitude = -1, zscale = 5, remove_na = T, clear_layers = T)#,
#horizontal_offset = c(-1, -1))

render_snapshot('SOIL CARBON/Graphics/AGU2022/DEMWIPCarbon small.png', 
                software_render = T, width = 8000, height = 6000)
filename_movie = ('SOIL CARBON/Graphics/AGU2022/DEMWIPCarbon small.gif')
render_movie(filename = filename_movie)


