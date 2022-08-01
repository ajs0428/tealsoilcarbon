rm(list = ls())

setwd('A:/')

library(raster)
library(rjags)
r.sq <- function(y,y.fitted){
    res <- y-y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}
##Read in data
#dat <- read.csv("C:/Users/ajs0428/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/ANALYSIS/R/Hoh_Mash_combined_data.csv")
dat <- read.csv("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/ANALYSIS/R/Hoh_Mash_combined_data.csv")
dat <- dat[dat$StudyArea == 'HOH', ]

y <- dat$carbon
x <- as.matrix(cbind(1, dat[, 4:5])) # just doing TWI and DTW
n <- length(y)
u.idx <- as.numeric(as.factor(dat$MHCLASS))

mod <- lm(y ~ x[, -c(1)]);summary(mod)
r.sq(y, fitted(mod)) #sanity check on rsq

jags.data <- list('y'= y, 'X'=x, 'p' = ncol(x), 'n'=n)
vars <- c('beta', 'sigma.sq')
model <- jags.model('/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/ANALYSIS/R/random-effects/lm.jag', data = jags.data)
samps <- coda.samples(model, vars, 50000)

plot(samps)

round(summary(samps)[[2]],3)
summary(mod)

## get fitted values
beta.samps <- samps[[1]][,grep("beta",colnames(samps[[1]]))]
sigma.sq.samps <- samps[[1]][,grep("sigma.sq",colnames(samps[[1]]))]

y.samps <- matrix(NA, nrow = n, ncol = nrow(beta.samps))
for(i in 1:nrow(beta.samps)){
    beta.samp.1 <- matrix(beta.samps[i,], ncol = 1)
    sigma.sq.samp.1 <- sigma.sq.samps[i]
    y.samps[,i] <- x%*%beta.samp.1
}

y.fitted <- apply(y.samps, 1, median)

res <- y - y.fitted

plot(y.fitted,res, pch = 19, col = "grey");abline(h = 0, col = "red")

points(y.fitted[u.idx == 1],res[u.idx == 1], col = "red", pch = 19)
points(y.fitted[u.idx == 2],res[u.idx == 2], col = "blue", pch = 19)
points(y.fitted[u.idx == 3],res[u.idx == 3], col = "green", pch = 19)


############################
## random intercept model ##
jags.data <- list("y" = y, "X" = x, "p" = ncol(x), "n" = n, "u.idx" = u.idx, "q" = length(unique(u.idx)))
vars <- c("beta","sigma.sq", "u", "sigma.sq.u")

model <- jags.model("/Users/Anthony/OneDrive - UW/University of Washington/Data and Modeling/SOIL CARBON/ANALYSIS/R/random-effects/hlm.jag", data = jags.data)

samps <- coda.samples(model, vars, 50000)

## get fitted values
beta.samps <- samps[[1]][,grep("beta",colnames(samps[[1]]))]
sigma.sq.samps <- samps[[1]][,grep("sigma.sq",colnames(samps[[1]]))]
u.samps <- samps[[1]][,grep("u\\[",colnames(samps[[1]]))]
y.samps <- matrix(NA, nrow = n, ncol = nrow(beta.samps))

for(i in 1:nrow(beta.samps)){
    beta.samp.1 <- matrix(beta.samps[i,], ncol = 1)
    sigma.sq.samp.1 <- sigma.sq.samps[i]
    u.samp.1 <- u.samps[i,]
    y.samps[,i] <- x%*%beta.samp.1 + u.samp.1[u.idx]
}

y.fitted <- apply(y.samps, 1, median)

res <- y - y.fitted

plot(y.fitted,res, pch = 19, col = "grey");abline(h = 0, col = "red")

points(y.fitted[u.idx == 1],res[u.idx == 1], col = "red", pch = 19)
points(y.fitted[u.idx == 2],res[u.idx == 2], col = "blue", pch = 19)
points(y.fitted[u.idx == 3],res[u.idx == 3], col = "green", pch = 19)

r.sq(y,y.fitted)

plot(beta.samps)
plot(sigma.sq.samps)
plot(u.samps)
