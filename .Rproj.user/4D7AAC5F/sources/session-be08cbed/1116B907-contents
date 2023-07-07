source("R/setup.R", echo=FALSE)

# A simple function of two variables
f <- function(x){
  x[1]^2 + x[1]*x[2] + x[2]^3/9
}

# Evaluate "g()" by projecting 6-dims down to 2-dim
N <- 500
p <- 6
X <- lhs::maximinLHS(N, p)
A <- matrix(c(4, 3, 2, 1/3, 1/3, 1/3,
              1/3, 1/3, 1/3, 5, 2, 2), nrow=p, ncol=2)
Z <- X%*%A
y <- apply(Z, 1, f)

# Fit model and get active directions
mod <- bass(X, y)
C <- C_bass(mod)
W <- eigen(C)$vectors

# Get RSMPE values for various projection dimension
rmspe <- rep(NA, 6)
for(i in 1:6){
  mod_tmp <- bass(X%*%W[,1:i], y)
  rmspe[i] <- sqrt(mean((mod_tmp$yhat.mean - y)^2))
}
pct_expl <- (1 - rmspe/sd(y)) * 100
sqrt_eval <- sqrt(eigen(C)$values)

# Make Table
tab <- matrix(NA, nrow=2, ncol=6)
rownames(tab) <- c("Square Root Eigenvalue", "Pct of SD Explained")
tab[1,] <- sqrt_eval/max(sqrt_eval)
tab[2,] <- pct_expl
stargazer::stargazer(tab, type="text")

# Make figures
if(save_results){
  mycol <- function(yy, nn){
    yy <- (yy-min(yy))/diff(range(yy))
    1 + floor(yy*(nn-1))
  }

  png("figs/poly_2dplot.png", height=5, width=5, units="in", res=300)
  bob <- RColorBrewer::brewer.pal(11, "RdBu")
  plot(X%*%W[,1], y, xlab="First active direction", ylab="Model output",
       pch=21, bg=bob[mycol(y, length(bob))], col="black")
  dev.off()

  # This 3d plot requires rgl
  if(require(rgl)){
    options(rgl.printRglwidget = TRUE)
    png("figs/poly_3dplot.png", height=5, width=5, units="in", res=300)
    plot3d(X%*%W[,1], X%*%W[,2], y,
           xlab="First active direction",
           ylab="Second active direction",
           zlab="Model output",
           col=bob[mycol(y, length(bob))], size=0.8, type=c("s"))
    dev.off()
  }
}


