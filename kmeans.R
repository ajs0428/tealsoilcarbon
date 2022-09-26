library(terra)
library(stats)
setwd('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling')
grad50 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/grad_50.tif")
grad150 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/grad_150.tif")
grad300 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/grad_300.tif")
dev50 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/dev_50_1.tif")
dev150 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/dev_150_1.tif")
dev300 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/dev_300_1.tif")
#plan50 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/plan")
plan150 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/plan_150.tif")
plan300 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/plan_300.tif")
prof50 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/prof_50.tif")
prof150 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/prof_150.tif")
prof300 <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/prof_300.tif")
dem <- rast("Teal_Carbon - WIP/TealcarbonHoh_inputs/Hoh_all_DTM_NAD83_4m_v2_clip.tif")

plot(grad50)

lys <- c(grad50,grad150, grad300, dev50, dev150, dev300, 
         plan150, plan300, prof50, prof150, prof300, dem )

small <- c(grad50, plan150, prof50, dem)
lysvals <- values(lys)


set.seed(99)
kmncluster <- kmeans(na.omit(small), centers = 5, iter.max = 500, nstart = 5, algorithm="Lloyd")

