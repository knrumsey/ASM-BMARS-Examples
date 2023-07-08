source("R/setup.R", echo=FALSE)


f <- function(x){
  x[1]^2 + x[1]*x[2] + x[2]^3/9
}

p <- 10
n <- 100
X <- matrix(runif(n*p), ncol=p)
y <- apply(X, 1, f) + rnorm(n, 0, 0.1)

# Use BASS package
mod <- bass(X, y, birth.type="coinflip", nmcmc=20000, nburn=19000)
C <- C_bass(mod, mcmc.use=1:1000)

# Use GP package
mod2 <- mleHetGP(X, y)
C2 <- C_GP(mod2)[[1]]

# Get activity scores (BASS posterior)
ACT <- matrix(NA, nrow=1000, ncol=p)
for(i in 1:1000){
  ACT[i,] <- act_scores(C[[i]])
  print(i)
}

# Get activity scores (GP)
# Note: No way to get posterior samples for GP
ACT2 <- act_scores(C2)

if(save_results){
  png("figs/act_post.png", height=5, width=8, units='in', res=300)
  boxplot(ACT, ylab="Activity Score (1)", xlab="Variable Number")
  points(ACT2, pch="X", col='red', lwd=2)
  legend("topright", c("C_bass (posterior)", "C_GP (point est.)"),
         col=c("gray", "red"), pch=c(NA, "X"), lty=c(1, NA), lwd=c(5, 2))
  dev.off()
}







