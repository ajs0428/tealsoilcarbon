library(devtools)
install_bitbucket("brendo1001/ithir/pkg") 
library(ithir)
library(dplyr)
data("oneProfile")
str(oneProfile)

hoh_dat <- read.csv("ANALYSIS/R/hoh_CstockHorizonDepth.csv")
str(hoh_dat)
hoh_dat$horizon_thickness <- NULL
#hoh_dat$sample_name <- NULL


#has to be Column 1 must contain site identifiers. Columns 2 and 3 must contain 
        #upper and lower sample depths, respectively. 
        #Subsequent columns will contain measured values for those depths.
hoh_dat2 <- hoh_dat %>% group_by(sample_name) %>% 
    mutate(Soil.ID = cur_group_id()) %>%
    select(-c(sampID)) %>%
    arrange(Soil.ID) %>%
    relocate(Soil.ID, top_depth, bottom_depth, C_stock_horiz_rev_g_cm2_rock_adj) %>%
    as.data.frame()
hoh_dat2$sample_name <- NULL
names(hoh_dat2) <- c("Soil.ID", "Upper.Boundary", "Lower.Boundary", "C.g.cm2.")
str(hoh_dat2)
hoh_sub <-subset(hoh_dat2, hoh_dat2$Soil.ID ==1)


#Increasing LAM value will make the spline
#more rigid. Decreasing it towards zero will make the spline more flexible such that
#it will follow near directly the observed data.
hoh_eaFit1 <- ea_spline(hoh_dat2, var.name = "C.g.cm2.",  d = t(c(0,30)),
                       lam = 0.5, vlow = 0, show.progress = F)
hoh_eaFit2 <- ea_spline(hoh_dat2, var.name = "C.g.cm2.",  d = t(c(0,150)),
                       lam = 0.5, vlow = 0, show.progress = F)
str(hoh_eaFit1)

#plot_ea_spline()

par(mfrow = c(3, 1))
for (i in 1:3) {
    plot_ea_spline(splineOuts = hoh_eaFit1,  d = t(c(0,30)), maxd = 30, type = i, plot.which = 30,
                   label = "carbon stock") }
for (i in 1:3) {
    plot_ea_spline(splineOuts = hoh_eaFit2,  d = t(c(0,150)), maxd = 150, type = i, plot.which = 30,
                   label = "carbon stock") }


hoh_splC1 <- hoh_eaFit1$harmonised
hoh_splC2 <- hoh_eaFit2$harmonised
#hoh_splC[hoh_splC< 0 ] <- NA

