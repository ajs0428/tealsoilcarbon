library(lme4)
library(lmerTest)
library(terra)
library(sp)
library(dplyr)
library(ggplot2)
library(merTools)
library(glmnet)
library(stats)
library(MASS)
library(ggcorrplot)
library(RColorBrewer)
library(cowplot)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions


setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/')
#setwd('/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/')
wd = '/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling'
#wd = '/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/' 

hoh_csv <- read.csv("SOIL CARBON/ANALYSIS/hoh_CHN_1m30cm90cm120cm_Stocks_WIPupd.csv")
#write_csv(hoh_csv, file = "SOIL CARBON/ANALYSIS/hoh_CHN_1m30cm_Stocks_WIPupd.csv")
#vect(hoh_csv, geom = c('x', 'y'))


#subset for easier viewing and access
hoh_dat <- as_tibble(hoh_csv) %>% dplyr::select(sample_name,
                                            CHN_Cstock_Mg_ha, CHN_1m_Cstock_Mg_ha, CHN_30m_Cstock_Mg_ha,
                                            CHN_90m_Cstock_Mg_ha, CHN_120m_Cstock_Mg_ha,
                                            WIP_NEW, DEM,DTW, PRECIP,SLP,TWI, DTW_RIV,
                                            NDVI_s_0_win,MNDWI_s_0_win, EVI_s_0_win, b10_s_0_win,
                                            NDVI_s_1_spr, MNDWI_s_1_spr, EVI_s_1_spr, b10_s_1_spr,
                                            NDVI_s_2_sum,  MNDWI_s_2_sum, EVI_s_2_sum, b10_s_2_sum,
                                            NDVI_s_3_aut,  MNDWI_s_3_aut, EVI_s_3_aut, b10_s_3_aut, 
                                            GEO_250, b2_s_1_spr, b3_s_2_sum, b1_s_1_spr,  
                                            b5_s_2_sum, b5_s_3_aut, b8_s_2_sum, DTW_RIV) %>%
    #mutate(GEO_8_6_22 =  replace(GEO_8_6_22, GEO_8_6_22 == "Quaternary", "Holocene-Quaternary-Present")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_at(c("DTW_RIV"), as.factor) %>%
    stats::setNames(c("NAME", "CARBON_FULL", "CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM",
                      "WIP", "DEM","DTW","PRECIP","SLP","TWI", "DTW_RIV",
                      "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                      "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                      "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                      "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut", "GEO",
                      "b2_s_1_spr", "b3_s_2_sum", "b1_s_1_spr",  "b5_s_2_sum", 
                      "b5_s_3_aut", "b8_s_2_sum", "DTW_RIV")) %>% 
    as.data.frame()
str(hoh_dat)

carbonMean <- mean(hoh_dat$CARBON_1M)
carbonSD <- sd(hoh_dat$CARBON_1M)

hoh_dat_norm <- hoh_dat
hoh_dat_norm[ , -c(1:4,11, 28)] <- scale(x       = hoh_dat_norm[ , -c(1:4, 11,28)],
                                      center  = TRUE,
                                      scale   = TRUE)

############## Recoding GEO ##########################################
hoh_dat<- hoh_dat  |> mutate(landtype = case_when(WIP >= 0.5 &GEO == "Quat_new" ~ "RIVERINE",
                                                  WIP >= 0.5 &GEO != "Quat_new" ~ "NON RIV WETLAND",
                                                  WIP < 0.5 &WIP>0.1 &GEO != "Quat_new" ~ "MESIC",
                                                  WIP < 0.5 &WIP>0.1 &GEO == "Quat_new" ~ "MESIC",
                                                  WIP <= 0.1 &GEO != "Quat_new" ~ "UPLAND",
                                                  TRUE ~ "NON RIV WETLAND")) |>
    mutate(landtype_wetup = case_when(WIP >= 0.5 ~ "WETLAND",
                                      WIP < 0.5 ~ "UPLAND")) |>
    mutate(landtype_no_mes = case_when(WIP >= 0.5 &GEO == "Quat_new" ~ "RIVERINE",
                                       WIP >= 0.5 &GEO != "Quat_new" ~ "NON RIV WETLAND",
                                       WIP < 0.5 &WIP>0.1 &GEO != "Quat_new" ~ "UPLAND",
                                       WIP < 0.5 &WIP>0.1 &GEO == "Quat_new" ~ "UPLAND",
                                       WIP <= 0.1 &GEO != "Quat_new" ~ "UPLAND",
                                       TRUE ~ "NON RIV WETLAND"))



# R^2 function from Chad - lmer doesn't seem to give it
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
# LASSO Function or at least my attempt at a LASSO function
lasso <- function(df){
    set.seed(1)
    x <- data.matrix(subset(df, select = -c(NAME, CARBON_FULL, CARBON_1M, CARBON_30CM)))#data.matrix(df[, c(WIP,DEM,DTW, GEO,PRECIP,SLP,TWI,
    #"NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
    #"NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
    #"NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
    #"NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut")])
    y <- df$CARBON_1M
    
    cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
    best_lambda <- cv_model$lambda.min
    plot(cv_model)
    print(paste("best lambda:", best_lambda))
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    print(coef(best_model))
    print(summary(best_model))
    # summ <- summary(cv_model)
    # lass0_df <- data.frame(Var = rownames(cv_model)[summ$i],
    #                        LassWeight = summ$x)
    # lass0_df <- lass0_df[order(-lass0_df$LassWeight),]
}


############## Statistical Models ###########################################################

###### Doing LASSO regression to identify variables##############
lasshoh <- lasso(hoh_dat) # for hoh

hoh_mat <- round(cor(hoh_dat[, c("CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM",
                           "WIP", "DEM","DTW","PRECIP","SLP","TWI",
                           "NDVI_win","MNDWI_win", "EVI_win", "TempK_win",
                           "NDVI_spr", "MNDWI_spr", "EVI_spr", "TempK_spr",
                           "NDVI_sum",  "MNDWI_sum", "EVI_sum", "TempK_sum",
                           "NDVI_aut",  "MNDWI_aut", "EVI_aut", "TempK_aut" )]), 1)

#corrplot::corrplot(hoh_mat, method = "number", number.cex= 0.5)

gcp <- ggcorrplot(hoh_mat, hc.order = F, ggtheme = ggplot2::theme_gray, type = "upper",
           lab = F, insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 14)

# Taking the coefs from the LASSO results?

####Big model for backwards step##################
big_mod <- lmer(log10(CARBON_1M) ~  WIP+log10(DEM)+log10(DTW)+ log10(PRECIP)+SLP+TWI+
                NDVI_win+MNDWI_win+ EVI_win+ TempK_win+
                NDVI_spr+ MNDWI_spr+ EVI_spr+ TempK_spr+
                NDVI_sum+  EVI_sum+ TempK_sum+
                NDVI_aut+  MNDWI_aut+ EVI_aut+ TempK_aut  +
                b2_s_1_spr + b3_s_2_sum + b1_s_1_spr + b5_s_2_sum + b5_s_3_aut +
                 b8_s_2_sum + (1|GEO), data = hoh_dat)
big_mod2 <- lmer(log10(CARBON_FULL) ~ WIP + MNDWI_sum +  
                 EVI_sum + GEO + (1|GEO), data = hoh_dat, REML = F)

#(step_res <- MASS::stepAIC(big_mod))
#step(mod, scope=list(lower=Adjusted.score ~ original.score, upper=mod))
(stepA <- lmerTest::step(big_mod2, 
                         keep = c("WIP"),
                         reduce.random=FALSE))
#stepB <- stats::step(big_mod)
final <- get_model(stepA)
# final <- lmer(log10(CARBON_FULL) ~ (PRECIP)  + (1 | GEO), data = hoh_dat, REML = T)
model_summ <- summary(final)
mean(model_summ$residuals^2)

#Testing different models section ####
#test <-  lmer(log10(CARBON_FULL) ~ WIP + (1|GEO), data = hoh_dat, REML = F)
test <-  lmer(log10(CARBON_1M) ~ WIP + (1|GEO), data = hoh_dat_norm, REML = F)
summary(test)
anova(test)
tab_model(test)
sqrt(mean(resid(test)^2))
#lm(log10(CARBON_1M) ~ WIP + MNDWI_sum + EVI_sum + GEO, 
#                       data = hoh_dat, 
#                       REML = F);summary(test)

r.sq(log10(hoh_dat$CARBON_1M), fitted(test))
lm(log10(hoh_dat$CARBON_1M) ~ fitted(test))
mean(log10(hoh_dat$CARBON_1M) - predict(test))


############## Examining covariates ############################
plot((hoh_dat$MNDWI_sum ~ hoh_dat$CARBON_1M))
abline(lm(hoh_dat$MNDWI_sum ~ hoh_dat$CARBON_1M))
summary(lm(hoh_dat$MNDWI_sum ~ hoh_dat$CARBON_1M)) # I don't like the maps of these, too many artifacts
summary(lm(hoh_dat$WIP~ hoh_dat$CARBON_1M))
plot(WIP ~ CARBON_1M, data = hoh_dat)
abline(lm(WIP ~ CARBON_1M, data = hoh_dat))

sjPlot::tab_model(test)

############# Model Figures and Graphs ##########################################

#### LMER specific graph ####
ggplot(hoh_dat, aes(x = WIP, 
       y = log10(CARBON_1M), 
       colour =  GEO,
       shape = as.factor(WIP >= 0.5))) +
    geom_point(aes(size = 3, stroke = 2)) +
    geom_line(aes(y=predict(test), group=GEO)) +
    scale_color_manual(name = "GEOLOGY", values=c("#A3333D", "#291F1E", "#477998", "#9C9CDB"),
                       labels= c("Miocene/\nEocene", "Quaterary\nRiverine", "Quaterary\nAlluvium", "Quaternary\nClastic"))+#c("MioceneEocene", "QuaternaryRiverine", "QuaternaryAlluvium", "QuaternaryClastic")) +
    scale_shape_manual(values = c(16, 2), name = NULL, labels = c("Upland", "Wetland")) + 
    ggplot2::scale_size(name = NULL, breaks = NULL, labels = NULL) +
    xlab("WIP Wetland Probability") + ylab('Log10 Predicted Soil C (Mg/ha)') +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    #labs(colour = "Random Effect") +
    theme(legend.position = 'right', 
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

##### Fit of Predicted vs. Actual ####
ggplot(hoh_dat) +
    geom_point(aes(y = (fitted(test)), x = log10(CARBON_1M), 
                   colour =  as.factor(landtype_no_mes), shape = as.factor(WIP >= 0.5), size = 3, stroke = 2)) +
    # scale_color_manual(name = "Landscape Class", values=c( '#E69F00','#96E072', '#56B4E9', '#C1666B'),
    #                    labels= c("MESIC\n","NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND"))+#c("MioceneEocene", "QuaternaryRiverine", "QuaternaryAlluvium", "QuaternaryClastic")) +
    scale_color_manual(name = "Landscape\nClass", values=c( '#96E072', '#56B4E9', '#C1666B'),
                       labels= c("NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND"))+#c("MioceneEocene", "QuaternaryRiverine", "QuaternaryAlluvium", "QuaternaryClastic")) +
    scale_shape_manual(values = c(16, 2), name = "WIP Above/\nBelow \n0.50", labels = c("Upland", "Wetland")) + 
    ggplot2::scale_size(name = NULL, breaks = NULL, labels = NULL) +
    xlab("Log10 Actual Soil C (Mg/ha)") + ylab('Log10 Predicted Soil C (Mg/ha)') +
    geom_smooth(aes(y = fitted(test), x = log10(CARBON_1M)), method = "lm", se = TRUE) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.5, linetype = "dashed") +
    #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
    labs(colour = "Random Effect") +
    xlim(1.4, 3) +
    ylim(1.4, 3)  +
    theme(legend.position = 'right', 
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))
#### Histogram ####
ggplot(hoh_dat, aes(x = CARBON_1M)) +
    geom_histogram(position = "stack", bins = 30) + 
    # scale_fill_manual(values=c( '#E69F00','#96E072', '#56B4E9', '#C1666B'),
    #                    labels= c("MESIC\n","NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) +
    scale_fill_manual(values=c('#96E072', '#56B4E9', '#C1666B'),
                      labels= c("NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) +
    xlab("Soil C (Mg/ha)") +
    theme(legend.position = 'right', 
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

#### Column Chart ####
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varnames, groupnames){
    require(dplyr)
    require(plyr)
    summary_func <- function(x, col){
        c(mean_ = mean(x[[col]], na.rm=TRUE),
          sd_ = sd(x[[col]], na.rm=TRUE),
          se_ = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])),
          cnt = length(x[[col]]))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varnames[1])
    data_sum2<-ddply(data, groupnames, .fun=summary_func,
                    varnames[2])
    data_sum3<-ddply(data, groupnames, .fun=summary_func,
                    varnames[3])
    data_sum4<-ddply(data, groupnames, .fun=summary_func,
                     varnames[4])
    data_sum5<-ddply(data, groupnames, .fun=summary_func,
                     varnames[5])
    names(data_sum) <- c(groupnames, varnames[1],  paste0("sd_", varnames[1]), paste0("se_",varnames[1]), paste0("cnt",varnames[1]))
    names(data_sum2) <- c(groupnames, varnames[2],  paste0("sd_", varnames[2]), paste0("se_",varnames[2]), paste0("cnt",varnames[2]))
    names(data_sum3) <- c(groupnames, varnames[3],  paste0("sd_", varnames[3]), paste0("se_",varnames[3]), paste0("cnt",varnames[3]))
    names(data_sum4) <- c(groupnames, varnames[4],  paste0("sd_", varnames[4]), paste0("se_",varnames[4]), paste0("cnt",varnames[4]))
    names(data_sum5) <- c(groupnames, varnames[5],  paste0("sd_", varnames[5]), paste0("se_",varnames[5]), paste0("cnt",varnames[5]))
    #nms <- (data_sum[1])
    cmb <- (cbind(data_sum, data_sum2[2:5], data_sum3[2:5], data_sum4[2:5], data_sum5[2:5]))
    #rownames(cmb) <- nms
    return(as.data.frame(cmb))
}
hoh_bar <- data_summary(hoh_dat, varnames = c("CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM", "CARBON_FULL"), groupnames = "landtype_no_mes")
hoh_mesbar <- data_summary(hoh_dat, varnames = c("CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM", "CARBON_FULL"), groupnames = "landtype")
hoh_wubar <- data_summary(hoh_dat, varnames = c("CARBON_1M", "CARBON_30CM", "CARBON_90CM", "CARBON_120CM", "CARBON_FULL"), groupnames = "landtype_wetup")


hoh_bar_t <- t(hoh_bar[,2:10])

ggplot(hoh_bar, aes(x = landtype_no_mes)) +
    geom_bar()

ggplot(hoh_bar, aes(x = landtype_no_mes, y = CARBON_1M)) +
    geom_bar(aes(reorder(landtype_no_mes, -CARBON_1M), CARBON_1M, fill = landtype_no_mes),  position = "dodge", stat = "summary", fun = "mean") + 
    xlab("Landscape Class") + ylab(expression('Soil Carbon Stock (MgC ha'^-1*')')) +
    geom_errorbar(aes(ymin=CARBON_1M-se_CARBON_1M, ymax=CARBON_1M+se_CARBON_1M), width=.2,
                  position=position_dodge(.9)) +
    # scale_fill_manual( values=c( '#E69F00','#96E072', '#56B4E9', '#C1666B'),
    #                    labels= c("MESIC\n","NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) + 
    scale_fill_manual(values=c('#96E072', '#56B4E9', '#C1666B'),
                      labels= c("NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) +
    theme(legend.position = 'none', 
          axis.title.x=element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

#Just the wetland and upland 
ggplot(hoh_wubar, aes(x = landtype_wetup, y = CARBON_1M)) +
    geom_bar(aes(reorder(landtype_wetup, -CARBON_1M), CARBON_1M, fill = landtype_wetup),  position = "dodge", stat = "summary", fun = "mean") + 
    #geom_bar(aes(reorder(landtype_wetup, -CARBON_30CM), CARBON_30CM, fill = landtype_wetup),  position = "dodge", stat = "summary", fun = "mean") + 
    xlab("Landscape Class") + ylab(expression('Soil Carbon Stock (MgC ha'^-1*')')) +
    geom_errorbar(aes(ymin=CARBON_1M-se_CARBON_1M, ymax=CARBON_1M+se_CARBON_1M), width=.2,
                  position=position_dodge(.9)) +
    # scale_fill_manual( values=c( '#E69F00','#96E072', '#56B4E9', '#C1666B'),
    #                    labels= c("MESIC\n","NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) + 
    scale_fill_manual(values=c('#96E072', '#C1666B'),
                      labels= c("\nRIVERINE", "\nUPLAND")) +
    theme(legend.position = 'none', 
          axis.title.x=element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.ticks = element_blank(),
          text = element_text(size = 20))


#### LMER Model function ####

Model_Fun <- function(lmer_or_glmer, carbon_type, data){
    
     if (carbon_type == "FULL"){
        carbon_plot <- data$CARBON_FULL
        if (lmer_or_glmer == "lmer"){
            mod <- lmer(log10(CARBON_FULL) ~ WIP + MNDWI_sum +EVI_sum  +(1|GEO), 
                        data = data, REML = F)
            print(summary(mod)) 
            print(cat("R squared is:", r.sq(log10(data$CARBON_FULL), (fitted(mod)))  ))}
        else if (lmer_or_glmer == "glmer"){
            mod <- glmer(log10(CARBON_FULL) ~ WIP + MNDWI_sum +EVI_sum  +(1|GEO),
                         data = data,
                         family = Gamma(link = identity));summary(mod)
            print(summary(mod)) 
            print(cat("R squared is:", r.sq(log10(data$CARBON_FULL), (fitted(mod)))  ))}
       g<-  ggplot(data) +
            geom_point(aes(y = fitted(mod), x = log10(carbon_plot), colour =  as.factor(GEO), size = 3)) +
            xlab("Full Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
            geom_smooth(aes(y = fitted(mod), x = log10(carbon_plot)), method = "lm", se = FALSE) +
            geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
            #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
            labs(colour = "Random Effect") +
           theme(legend.position = 'none', 
                 panel.background = element_blank(),
                 panel.grid.major = element_line(colour = "grey80"),
                 axis.ticks = element_blank(),
                 text = element_text(size = 20))
       print(g)
       print("DONE!")
       return(mod) 
    }
    
    if (carbon_type == "1M"){
        carbon_plot <- data$CARBON_1M
        if (lmer_or_glmer == "lmer"){
            mod <- lmer(log10(CARBON_1M) ~ WIP + MNDWI_sum +EVI_sum  +(1|GEO), 
                        data = data, REML = F)
            print(summary(mod)) 
            print(cat("R squared is:", r.sq(log10(data$CARBON_1M), (fitted(mod)))))}
        else if (lmer_or_glmer == "glmer"){
            mod <- glmer(log10(CARBON_1M) ~ WIP + MNDWI_sum +EVI_sum  +(1|GEO),
                         data = data,
                         family = Gamma(link = identity));summary(mod)
            print(summary(mod)) 
            print(cat("R squared is:", r.sq(log10(data$CARBON_1M), (fitted(mod)))))}
    g<- ggplot(data) +
        geom_point(aes(y = fitted(mod), x = log10(carbon_plot), colour =  as.factor(GEO), size = 3)) +
        xlab("1m Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
        geom_smooth(aes(y = fitted(mod), x = log10(carbon_plot)), method = "lm", se = FALSE) +
        geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
        #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
        labs(colour = "Random Effect") +
        theme(legend.position = 'none', 
              panel.background = element_blank(),
              panel.grid.major = element_line(colour = "grey80"),
              axis.ticks = element_blank(),
              text = element_text(size = 20))
    print(g)
    print("DONE!")
    return(mod) 
    }
    
    if (carbon_type == "30CM"){
        carbon_plot <- data$CARBON_30CM
        if (lmer_or_glmer == "lmer"){
            mod <- lmer(log10(CARBON_30CM) ~ WIP + MNDWI_sum +EVI_sum  +(1|GEO), 
                        data = data, REML = F)
            print(summary(mod)) 
            print(cat("R squared is:", r.sq(log10(data$CARBON_30CM), (fitted(mod)))  )  )}
        else if (lmer_or_glmer == "glmer"){
            mod <- glmer(log10(CARBON_30CM) ~ WIP + MNDWI_sum +EVI_sum  +(1|GEO),
                         data = hoh_dat,
                         family = Gamma(link = identity));summary(mod)
            print(summary(mod)) 
            print(cat("R squared is:", r.sq(log10(data$CARBON_30CM), (fitted(mod)))))}
    g<- ggplot(data) +
        geom_point(aes(y = fitted(mod), x = log10(carbon_plot), colour =  as.factor(GEO), size = 3)) +
        xlab("30cm Actual Soil C (Mg/ha)") + ylab('Predicted Soil C (Mg/ha)') +
        geom_smooth(aes(y = fitted(mod), x = log10(carbon_plot)), method = "lm", se = FALSE) +
        geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
        #scale_color_manual(labels = c("Riverine", "Non-Riverine"), values = c("blue", "red")) +
        labs(colour = "Random Effect") +
        theme(legend.position = 'none', 
              panel.background = element_blank(),
              panel.grid.major = element_line(colour = "grey80"),
              axis.ticks = element_blank(),
              text = element_text(size = 20))
    print(g)
    print("DONE!")
    return(mod)  
   
    }
    # return(mod)  
    # print(typeof(g))
    # print("DONE!")
}
lmerFULL <- Model_Fun("lmer", "FULL", hoh_dat)
glmerFULL <- Model_Fun("glmer", "FULL", hoh_dat)
lmer1M <- Model_Fun("lmer", "1M", hoh_dat)
glmer1M <- Model_Fun("glmer", "1M", hoh_dat)
lmer30CM <- Model_Fun("lmer", "30CM", hoh_dat)
glmer30CM <- Model_Fun("glmer", "30CM", hoh_dat)



#### Uncertainty ####
set.seed(7)
PI <- predictInterval(
    test,
    hoh_dat,
    which = c("full"),
    level = 0.95,
    n.sims = 1000,
    stat = c("median"),
    type = c("linear.prediction"),
    include.resid.var = TRUE,
    returnSims = FALSE,
    seed = 7,
    .parallel = FALSE,
    .paropts = NULL,
    fix.intercept.variance = FALSE,
    ignore.fixed.terms = NULL
)
PIwet <- predictInterval(
    lmerFULL,
    subset(hoh_dat, WIP> 0.5),
    which = c("full"),
    level = 0.95,
    n.sims = 1000,
    stat = c("median"),
    type = c("linear.prediction"),
    include.resid.var = TRUE,
    returnSims = FALSE,
    seed = 7,
    .parallel = FALSE,
    .paropts = NULL,
    fix.intercept.variance = FALSE,
    ignore.fixed.terms = NULL
)

ggplot(aes(x=1:36, y=fit, ymin=lwr, ymax=upr), data=PI[1:36,]) +
    geom_point() +
    geom_linerange() +
    labs(x="Index", y="Prediction w/ 95% PI") + theme_bw()

PI_conf <- predictInterval(test, hoh_dat[1,], include.resid.var = 0)
newdata <- data.frame(WIP = 0.5, EVI_sum = 0.5, MNDWI_sum = 0.5, GEO = "Pleistocene")

confint.merMod(test, level = 0.95, method = "boot", nsim = 500, oldNames = F)
confint(test, method = "boot")

#trying predict

predict(lmerFULL, newdata, interval = "confidence", level = 0.95)


hoh_dat$upr <- PI$upr[1:36]
hoh_dat$lwr <- PI$lwr[1:36]

upr_mod <- lm(upr ~ log10(CARBON_FULL), data = hoh_dat)
lwr_mod <- lm(lwr ~ log10(CARBON_FULL) , data = hoh_dat)

################################ Map making ##########################################################################


#masked layers
WIPm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")#mask(WIP, (MNDWI_sum>-0.30),  maskvalues = 1, updatevalue = NA) #need to write to raster
#writeRaster(WIPm, filename = "SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_WIP_Mask0_10_2022.tif")
GEOm <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_GEO_250k_reclassMask.tif")
MNDWI_sum_m <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_MNDWIsum_Mask0_10_2022.tif")
EVI_sum_m <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/Hoh_EVIsum_Mask0_10_2022.tif")
#DTW <- rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/MainChannelDTW_m0.tif")
#DTW_RIV <- DTW >50#rast("SOIL CARBON/SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/MainChannelDTW_RIV_m0.tif")
#points <- vect(hoh_csv, geom = c("x", "y"), crs = "EPSG:26910")
#pt_ext <- terra::extract(DTW_RIV, points)
#hoh_csv$DTW_RIV <- pt_ext$MainChannelDTW
#write.csv(hoh_csv, file = "SOIL CARBON/ANALYSIS/hoh_CHN_1m30cm_Stocks_WIPupd.csv")

############## A histogram of maps ##########################################
hist_wip <- hist(WIPm, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm = T)
dat <- data.frame(counts= hist_wip$counts,breaks = hist_wip$mids)
ggplot(dat, aes(x = breaks, y = counts, fill =counts)) +
    geom_bar(stat = "identity") + 
    scale_x_continuous(breaks = seq(-1,1,0.25),
                       labels = seq(-1,1,0.25)) +
    scale_fill_gradientn(colors = rev(brewer.pal(n= 3,name = "YlGnBu"))) + 
    # scale_fill_manual(values=c( '#E69F00','#96E072', '#56B4E9', '#C1666B'),
    #                   labels= c("MESIC\n","NON RIV\nWETLAND", "\nRIVERINE", "\nUPLAND")) +
    theme(legend.position = 'none', 
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 20))

plot(GEOm, col = c("#A3333D", "#291F1E", "#477998", "#9C9CDB"))

############## Predicting model to raster map ########################################################
rs <- c(WIPm, MNDWI_sum_m, EVI_sum_m,GEOm)
names(rs) <- c("WIP", "MNDWI_sum","EVI_sum", "GEO")
#test$xlevels <- union(test$xlevels, levels(GEOm$GEOLOGIC_A))
Pred <- terra::predict(rs, lmer1M, allow.new.levels = T)
Pred_nl <- (10^Pred)
writeRaster(Pred_nl, filename = "SOIL CARBON/CrypticCarbon1M_LMEnonlog10_NEWGEO.tif")



# #### Set up PCA samples #### 
# # Ignore this section
# 
# rr <- rast(ncol=10, nrow=10, names="stratum")
# set.seed(1)
# values(rr) <- round(runif(ncell(rr), 1, 3))
# spatSample(rr, 2, "stratified", xy=TRUE)
# 
# #classify WIP values to stratify by
# m <- c(0, 0.1, 1,
#        0.1, 0.5, 2,
#        0.5, 1, 3)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# WIP$strata <- classify(WIP, rclmat, include.lowest = T)
# 
# WIP_strat_pts <- spatSample(WIP$strata, 1000, method = "stratified", na.rm = T, as.points = T, 
#                             xy = T, weights = T)
# writeVector(WIP_strat_pts, filename = "SOIL CARBON/PCA Points and Data/Hoh1000pts_upbetwet")
