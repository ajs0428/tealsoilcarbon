library(ggplot2)
library(lme4)
library(lmerTest)
library(tidyverse)
library(terra)
library(tidyterra)
library(ggcorrplot)
library(RColorBrewer)
library(cowplot)


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 

hoh_csv <- read.csv("SOIL CARBON/ANALYSIS/CrypticCarbon_Jlat_lithextract.csv")

#subset for easier viewing and access
hoh_dat <- as_tibble(hoh_csv) %>% dplyr::select(sample_name, CHN_Cstock_Mg_ha, CHN_1m_Cstock_Mg_ha, 
                                                CHN_30m_Cstock_Mg_ha, CHN_90m_Cstock_Mg_ha, CHN_120m_Cstock_Mg_ha,
                                                LITHOLOGY, WIPv8, jlat, jlon ) %>%
    mutate_if(is.character, as.factor) %>%
    stats::setNames(c("NAME", "CARBON_FULL", "CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM",
                      "LITHOL", "WIP", "jlat", "jlon")) %>% 
    as.data.frame()
str(hoh_dat)

##### Model #####
mod <-  lmer(sqrt(CARBON_1M) ~ WIP + (1|LITHOL), data = hoh_dat)

############## Recoding LITHOL to landtype  ##########################################
hoh_dat<- hoh_dat  |> mutate(landtype = case_when(WIP >= 0.5 &LITHOL == "alluvium_marine_water" ~ "Riverine",
                                                  WIP >= 0.5 &LITHOL != "alluvium_marine_water" ~ "Palustrine",
                                                  WIP < 0.5 &WIP>0.1 &LITHOL != "alluvium_marine_water" ~ "Mesic",
                                                  WIP < 0.5 & WIP>0.1 &LITHOL == "alluvium_marine_water" ~ "Riparian Mesic",
                                                  WIP <= 0.1 &LITHOL != "alluvium_marine_water" ~ "Upland",
                                                  WIP <= 0.1 &LITHOL == "alluvium_marine_water" ~ "Riparian Upland",
                                                  TRUE ~ "Upland")) |>
    mutate(landtype_wetup = case_when(WIP >= 0.5 ~ "Wetland",
                                      WIP < 0.5 ~ "Upland")) 

############# Model Figures and Graphs ##########################################

#### LMER specific graph ####
ggplot(hoh_dat, aes(x = WIP*100, 
                    y = sqrt(CARBON_1M))) +
    geom_point(aes(shape = as.factor(WIP>=0.5), fill = LITHOL), color = "black",size = 6, stroke = 1, show.legend = F) +
    geom_line(aes(y=predict(mod), group=LITHOL, color = LITHOL),show.legend = F, size = 1) +
    scale_fill_manual(values = c( "#291F1E",  "#9C9CDB", "#A3333D","#477998"), 
                      labels = c("Alluvium", "Glacial Drift", "Sedimentary", "Glacial Till"),
                      name = "Surficial \nGeology") +
    scale_color_manual(values = c( "#291F1E",  "#9C9CDB", "#A3333D","#477998"), guide = NULL) +
    #=c( "#291F1E",  "#9C9CDB", "#A3333D","#477998"   labels= levels(hoh_dat$LITHOL)) +
    scale_shape_manual(values = c(21, 24), name = NULL, labels = c("Upland", "Wetland")) + 
    ggplot2::scale_size(name = NULL, breaks = NULL, labels = NULL) +
    xlab("WIP Wetland Probability %") + ylab(expression('Square Root Predicted SOC Stock (MgC ha'^-1*')')) +
    xlim(0, 100) +
    ylim(0, 30) +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    #labs(colour = "Random Effect") +
    guides(fill = guide_legend(override.aes = list(shape = 21))) +
    theme(legend.position = 'right', 
          legend.background = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

ggsave(filename= "/Users/Anthony/OneDrive - UW/University of Washington/Writing and Drafting/Cryptic Carbon/LMER_scatter.png",
       width = 9, height = 7.5, units = "in")

PI <- predictInterval(
    mod,
    which = c("full"),
    level = 0.95,
    n.sims = 1000,
    stat = c("median"),
    type = c("linear.prediction"),
    include.resid.var = TRUE,
    returnSims = TRUE,
    seed = 7
)

PI_trans <- PI |> mutate(fit_t = fit**2,
                         upr_t = upr**2,
                         lwr_t = lwr**2,
                         act = hoh_dat$CARBON_1M,
                         fit_t_act = (fit_t - act)**2)
#PI_rmse <- sqrt(sum(PI_trans$fit_t - PI_trans$act)**2)

##### Fit of Predicted vs. Actual ####
ggplot(hoh_dat, aes(y = (fitted(mod))**2, x = (CARBON_1M))) +
    geom_point(color='black', shape=21, aes(fill = WIP*100),
                   # #shape = as.factor(WIP >= 0.5), 
                   size = 7, stroke = 1) +
    scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"), name = "WIP %", n.breaks = 5, limits = c(0, 100)) +
    geom_smooth(aes(y = (fitted(mod))**2, x = (CARBON_1M)), method = "lm", color = "#fa3e3e", size = 1.5, linetype = 5, se = F) +
    # scale_color_manual(name = "Landscape Class", values=c('#E69F00', '#56B4A1' , '#96E072',  '#56B4E9',  '#C1666B' ),
    #                    labels= levels(as.factor(hoh_dat$landtype)))+#c("MioceneEocene", "QuaternaryRiverine", "QuaternaryAlluvium", "QuaternaryClastic")) +
    #geom_ribbon(aes(x = sqrt(CARBON_1M), ymax = PI$upr, ymin = PI$lwr), alpha = 0.1, color = NA) +
    geom_smooth(aes(x = (CARBON_1M), y = (PI$lwr)**2), lty = 3, size = 1.7, se = F, col = "#1664c9") + 
    geom_smooth(aes(x = (CARBON_1M), y =( PI$upr)**2), lty = 3, size = 1.7, se = F, col = "#1664c9") +
    #ggplot2::scale_size(name = NULL, breaks = NULL, labels = NULL) +
    xlab(expression('Actual SOC Stock (MgC ha'^-1*')')) + ylab(expression('Predicted SOC Stock (MgC ha'^-1*')')) +
    geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = "dashed") +
    #labs(colour = "Random Effect") +
    #xlim(0, 32) +
    #ylim(0, 32)  +
    theme(legend.position = "right", 
          #legend.title = "WIP %",
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

ggplot2::ggsave("/Users/Anthony/OneDrive - UW/University of Washington/Writing and Drafting/Cryptic Carbon/PredictvsActual.png",
                width = 9, height = 7.5, units = "in")
# #### Histogram ####
# ggplot(hoh_dat, aes(x = sqrt(CARBON_1M), 
#                          fill = as.factor(LITHOL))) +
#     geom_histogram(position = "stack", bins = 30) + 
#     scale_fill_manual(values=c( '#E69F00','#96E072', '#56B4E9', '#C1666B'),
#                       labels= c("MESIC\n","NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) +
#     # scale_fill_manual(values=c('#96E072', '#56B4E9', '#C1666B'),
#     #                   labels= c("NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) +
#     xlab("Square Root Soil C (MgC/ha)") +
#     theme(legend.position = 'right', 
#           panel.background = element_blank(),
#           panel.grid.major = element_line(colour = "grey80"),
#           axis.ticks = element_blank(),
#           text = element_text(size = 20))


# #### Uncertainty Intervals Figure #####
    # Do this in ArcGIS 
#### Cryptic Carbon Polygons and Size class ####

hoh_WIP_bin_polys_ab64_zonext <- vect("SOIL CARBON/CrypticCarbonMaps/hoh_WIP_bin_polys_ab64_zonext.gpkg")

polys_df <- as_tibble(hoh_WIP_bin_polys_ab64_zonext)

ggplot(polys_df, aes(x = log(area_ha), y = log(extract_stock_SOC))) +
    geom_point(aes(colour=extract_mean_SOC)) + 
    scale_color_continuous(type = "viridis", name = expression('SOC Stock Density (MgC ha'^-1*')')) +
    ylab(expression('Log Transformed SOC Stock (Tg)')) +
    xlab('Log Transformed Individual Wetland Extent (ha)') +
    theme(legend.position = "right", 
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

ggplot2::ggsave("/Users/Anthony/OneDrive - UW/University of Washington/Writing and Drafting/Cryptic Carbon/WetlandSizeClassSOC.png",
                width = 9, height = 6.5, units = "in")


