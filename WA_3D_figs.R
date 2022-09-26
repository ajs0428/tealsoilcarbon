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
library(webshot)

setwd("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/")

#### hoh data sources ####
# WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_2022_fullmodel_v08/Hoh_2022_fullmodel_v08.tif")
# agb <- rast("AGB/HOH/Hudak_AGB_WIPrspl.tif")
# soil_C <- rast("SOIL CARBON/HOH_CARBON_8_9_22_RSmask0.tif")
# upl_soil_C <- rast("SOIL CARBON/hohC_mask0_upl.tif")
# wet_soil_C <- rast("SOIL CARBON/hohC_mask0_wet.tif")
# bet_soil_C <- rast("SOIL CARBON/hohC_mask0_between.tif")
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
WIP <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_NWI_WIP_clip_INV.tif")
agb <- rast("AGB/COL/COL_AGB_WIPrspl.tif")
col_poly <- vect("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/colville_nwi_poly.shp")
soil_C <- rast("SOIL CARBON/COL_CARBON_8_9_22_RSmask0.tif")
upl_soil_C <- rast("SOIL CARBON/colC_mask0_uplMg.tif")
wet_soil_C <- rast("SOIL CARBON/colC_mask0_wetMg.tif")
bet_soil_C <- rast("SOIL CARBON/colC_mask0_betweenMg.tif")
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
(agbC <- gplot(agb, maxpixels=200000) + geom_tile(aes(fill = value*0.5)) + #remember to multiply by 0.5 for carbon
        scale_fill_gradientn("Aboveground Carbon \nStock MgC/ha \n \n", 
                             colors = brewer.pal(9, "Greens"), 
                             na.value = "white", 
                             limits = c(0, 800)) +
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
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/AGC_COL_3D.png', fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)
rgl::close3d()
plot_gg(WIP_plot, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 1,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/COL_WIP_3D.png', fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)
rgl::close3d()
#save_obj('SOIL CARBON/ANALYSIS/R/Graphics/All_Carbon.obj')

#### Soil Rayshader ####
plot_gg(soilC_plt, multicore = TRUE, raytrace = TRUE, width = 6, height = 4, 
        pointcontract = 0.5, scale = 150,zoom = 0.8, phi = 35, theta = 315,
        shadow_intensity = 0.5, soliddepth = 0, reduce_size = 0.5)
snapshot3d('SOIL CARBON/ANALYSIS/R/Graphics/COL_soilC_plt.png', fmt = "png",
           width = 4000, height = 3600, webshot = TRUE)
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

