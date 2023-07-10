source("R/setup.R", echo=FALSE)
set.seed(10943)

f3 <- function(x){
  x[1]^2 + x[1]*x[2]*x[3] + x[2]^3/9
}

n <- 500
p <- 10
X <- matrix(runif(n*p), ncol=p)
y <- apply(X, 1, f3)

# Fit models
mod1 <- bass(X, y, maxInt=1)
mod2 <- bass(X, y, maxInt=2)
mod3 <- bass(X, y, maxInt=3)
mod4 <- bass(X, y, maxInt=4)
modc <- bass(X, y, birth.type="coinflip")

# Test models
nt <- 1000
Xt <- matrix(runif(nt*p), ncol=p)
yt <- apply(Xt, 1, f3)

yhat1 <- apply(predict(mod1, Xt), 2, mean)
yhat2 <- apply(predict(mod2, Xt), 2, mean)
yhat3 <- apply(predict(mod3, Xt), 2, mean)
yhat4 <- apply(predict(mod4, Xt), 2, mean)
yhatc <- apply(predict(modc, Xt), 2, mean)

rmspe <- function(d) sqrt(mean(d^2))

rmspe1 <- rmspe(yhat1-yt)
rmspe2 <- rmspe(yhat2-yt)
rmspe3 <- rmspe(yhat3-yt)
rmspe4 <- rmspe(yhat4-yt)
rmspec <- rmspe(yhatc-yt)

tab <- matrix(c(rmspe1,
         rmspe2,
         rmspe3,
         rmspe4,
         rmspec), nrow=1)
colnames(tab) <- c("maxInt = 1", "maxInt = 2", "maxInt = 3", "maxInt = 4", "coinflip")
stargazer(tab)


# Get C_true (with MC)
measure <- function() runif(p)
C0 <- C_mc(f3, measure, nmc=1e6)

# Estimate C
#C1 <- C_bass(mod1)
C2 <- C_bass(mod2)
C3 <- C_bass(mod3)
C4 <- C_bass(mod4)
Cc <- C_bass(modc)

tab2 <- matrix(c(0, rmspe(C0 - C2),
rmspe(C0 - C3),
rmspe(C0 - C4),
rmspe(C0 - Cc)), nrow=1)

stargazer(rbind(tab, tab2))


