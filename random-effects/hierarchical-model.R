rm(list=ls())
###############
## libraries ##
library(raster)
library(rjags)

###################
## calculate R^2 ##
r.sq <- function(y,y.fitted){
    res <- y - y.fitted
    1-sum(res^2)/sum((y-mean(y))^2)
}

##################
## read in data ##
dat <- read.csv("Hoh_Mash_combined_data.csv")

dat <- dat[dat$StudyArea == "HOH",]

y <- dat$Carbon
X <- as.matrix(cbind(1,dat[,4:5])) ## just considering TWI and DTW
n <- length(y)
u.idx <- as.numeric(as.factor(dat$MHCLASS))

mod <- lm(y ~ X[,-c(1)]);summary(mod)

r.sq(y,fitted(mod))

## fit Bayesian version of same model 
jags.data <- list("y" = y, "X" = X, "p" = ncol(X), "n" = n)
vars <- c("beta","sigma.sq")

model <- jags.model("lm.jag", data = jags.data)

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
    y.samps[,i] <- X%*%beta.samp.1
}

y.fitted <- apply(y.samps, 1, median)

res <- y - y.fitted

plot(y.fitted,res, pch = 19, col = "grey");abline(h = 0, col = "red")

points(y.fitted[u.idx == 1],res[u.idx == 1], col = "red", pch = 19)
points(y.fitted[u.idx == 2],res[u.idx == 2], col = "blue", pch = 19)
points(y.fitted[u.idx == 3],res[u.idx == 3], col = "green", pch = 19)



############################
## random intercept model ##
jags.data <- list("y" = y, "X" = X, "p" = ncol(X), "n" = n, "u.idx" = u.idx, "q" = length(unique(u.idx)))
vars <- c("beta","sigma.sq", "u", "sigma.sq.u")

model <- jags.model("hlm.jag", data = jags.data)

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
    y.samps[,i] <- X%*%beta.samp.1 + u.samp.1[u.idx]
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


###############################
## random coefficients model ##
jags.data <- list("y" = y, "X" = X, "p" = ncol(X), "n" = n, "u.idx" = u.idx, "q" = length(unique(u.idx)))
vars <- c("beta.0","beta.1","beta.2","sigma.sq","sigma.sq.u0","sigma.sq.u1","sigma.sq.u2","u.0","u.1","u.2")

model <- jags.model("hlm-slopes.jag", data = jags.data)

samps <- coda.samples(model, vars, 50000)

## We have a few more parameters to look at here so I'm to break them apart before plotting them
beta.samps <- samps[[1]][,grep("beta",colnames(samps[[1]]))]
sigma.sq.samps <- samps[[1]][,grep("sigma.sq",colnames(samps[[1]]))]

u0.samps <- samps[[1]][,grep("u.0\\[",colnames(samps[[1]]))]
u1.samps <- samps[[1]][,grep("u.1\\[",colnames(samps[[1]]))]
u2.samps <- samps[[1]][,grep("u.2\\[",colnames(samps[[1]]))]

y.samps <- matrix(NA, nrow = n, ncol = nrow(beta.samps))

for(i in 1:nrow(beta.samps)){
    beta.samp.1 <- matrix(beta.samps[i,], ncol = 1)
    sigma.sq.samp.1 <- sigma.sq.samps[i]

    u0.samp.1 <- u0.samps[i,]
    u1.samp.1 <- u1.samps[i,]
    u2.samp.1 <- u2.samps[i,]
    
    y.samps[,i] <-
        (beta.samp.1[1] + u0.samp.1[u.idx])*X[,1] +
        (beta.samp.1[2] + u1.samp.1[u.idx])*X[,2] +
        (beta.samp.1[3] + u2.samp.1[u.idx])*X[,3]
}

y.fitted <- apply(y.samps, 1, median)

r.sq(y,y.fitted)


plot(beta.samps)
plot(sigma.sq.samps)
plot(u0.samps)
plot(u1.samps)
plot(u2.samps)

round(summary(samps)[[2]],2)
