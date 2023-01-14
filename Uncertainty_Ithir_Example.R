library(ithir)
library(raster)
data(hunterCovariates)
names(hunterCovariates) <- c("aacn", "drainage_index", "insolation",
                             "twi", "total_count")
crs(hunterCovariates) <- "+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
hunterCovariates
plot(hunterCovariates)
# Load soil observations
data(HV_subsoilpH)

# Convert to simple features data.frame
library(sf)
subsoil_ph <- st_as_sf(HV_subsoilpH,
                       coords = c('X', 'Y'),
                       crs = "+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")

# Rename the pH column
colnames(subsoil_ph)[1] <- "ph"

# Extract covariates
obs_covariates <- extract(hunterCovariates, as(subsoil_ph, "Spatial"))

# Set the random seed so that you get the same results
set.seed(12345)

# Select 100 rows from subsoil_ph at random
val_rows <- sample(nrow(subsoil_ph), 100)

library(caret)
library(Cubist)

# What parameters does the Cubist model take?
modelLookup(model = "cubist")

# Grid of tuning parameters
grid <- expand.grid(committees = 1, neighbors = 0)

# Fit a single Cubist model using all calibration data
cubist_fit <- train(x = obs_covariates[-val_rows, ],
                    y = subsoil_ph$ph[-val_rows],
                    method = "cubist",
                    trControl = trainControl(method = "none"),
                    tuneGrid = grid)
# Get a summary of the Cubist model
summary(cubist_fit$finalModel)

#Now let’s predict onto the validation samples:
ph_val <- subsoil_ph[val_rows, ]
ph_val$z_cubist <- predict(cubist_fit,
                           newdata = obs_covariates[val_rows, ])
# Predict fitted Cubist model onto covariate grid
z_grid <- raster::predict(hunterCovariates, cubist_fit)
z_grid

# Plot the raster
plot(z_grid, main = "Cubist 60-100 cm pH")

#goodness of fit validation
goof(ph_val$ph, ph_val$z_cubist)


library(ggplot2)
ggplot(ph_val, aes(x = ph, y = z_cubist)) +
    geom_abline(colour = "#CCCCCC", linetype = 2) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    xlim(3.0, 9.75) + ylim(3.25, 9.75)


#Bootstrapping pH
# Subset calibration data from subsoil_ph
ph_cal <- subsoil_ph[-val_rows, ]
cal_covariates <- obs_covariates[-val_rows, ]

# Number of bootstrap realisations
k <- 10

# Empty matrix to store validation predictions
val_preds <- matrix(nrow = nrow(ph_val), ncol = k)

# Empty RasterStack to store rasters of bootstrap realisation predictions
z_realisations <- stack()

#Here’s the actual bootstrapping engine. This sort of thing is readily parallelisable, 
## but we’ll run it in series here. Just don’t set k to a large value!:
# Do r bootstrap realisations
for(i in 1:k) {
    
    # Generate a bootstrap resample of ph_cal rows
    cal_rows <- sample(nrow(ph_cal), nrow(ph_cal), replace = TRUE)
    
    # Fit a Cubist model
    cubist_fit_boot <- train(x = cal_covariates[cal_rows, ],
                             y = ph_cal$ph[cal_rows],
                             method = "cubist",
                             trControl = trainControl(method = "none"),
                             tuneGrid = grid)
    
    # Predict onto validation samples
    val_preds[, i] <- predict(cubist_fit_boot, 
                              newdata = obs_covariates[val_rows, ])
    
    # Predict onto raster
    z_realisations <- stack(z_realisations,
                            raster::predict(hunterCovariates, cubist_fit_boot))
}

# Aggregate the validation predictions
ph_val$z_boot <- apply(val_preds, 1, mean)

# Validate the final bootstrap prediction
goof(ph_val$ph, ph_val$z_boot)

# Plot the bootstrap predictions for one validation site
hist(val_preds[10, ], xlab = "60-100 cm pH", main = NULL)

# Compute the mean of the bootstrap distribution for this validation
# site
abline(v = mean(val_preds[10, ]), lty = 3)

# Compute 90% prediction interval limits as 5th and 95th percentiles
# of the bootstrap distribution for this validation site
abline(v = quantile(val_preds[10, ], probs = c(0.05, 0.95)),
       lty = 3,
       col = "red")

#Now let’s compute the prediction intervals across the whole study area.
# Compute mean of bootstrap realisations
z_mean <- raster::calc(z_realisations, mean)
plot(z_mean, main = "Bootstrapped Cubist 60-100 cm pH")

# Compute prediction interval limits
z_interval <- raster::calc(z_realisations, 
                           function(x) {
                               quantile(x,
                                        probs = c(0.05, 0.95),
                                        na.rm = TRUE)
                           })
names(z_interval) <- c("lower", "upper")
plot(z_interval)

# Compute prediction interval width
z_pi_width <- z_interval[[2]] - z_interval[[1]]
plot(z_pi_width, main = "90-percent prediction interval width")
