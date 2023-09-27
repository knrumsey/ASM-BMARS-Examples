source("R/setup.R", echo=FALSE)

## NOTE!
#' Change B <- 1000 for the results in the paper (takes 100 times as long)
B <- 1000

f <- function(x){
  x[1]^2 + x[1]*x[2] + x[2]^3/9
}

p <- 10
n <- 100
X <- matrix(runif(n*p), ncol=p)
y <- apply(X, 1, f) + rnorm(n, 0, 0.1)

#' Use BASS package
#mod <- bass(X, y, birth.type="coinflip", nmcmc=20000, nburn=19000) # Use this version for best results
mod <- bass(X, y)
C <- C_bass(mod, mcmc.use=1:1000)

# Get activity scores (BASS posterior)
ACT <- matrix(NA, nrow=1000, ncol=p)
for(i in 1:1000){
  ACT[i,] <- act_scores(C[[i]])
  #print(i)
}

# Use GP package
mod2 <- mleHetGP(X, y)
C2 <- C_GP(mod2)[[1]]
tictoc::tic()
C2_ci <- C_GP_ci(mod2, B=B)
t0 <- tictoc::toc()
cat(t0$toc - t0$tic)

# Get activity scores (GP)
# Note: No way to get posterior samples for GP
ACT2_pt <- act_scores(C2)
# Note: 9/27/23 New C_GP_ci function for conf intervals
#' Seems like it returns CI for the eigenvalues (but not the eigenvectors?)
#' To convert to activity scores, we'll use the posterior mean for the eigenvectors
#' and propagate the uncertainty from the eigenvalues. May be a better way to do this,
#' but not easily with the (current) activegp software.
evec2 <- eigen(C2)$vectors[,1]
ACT2 <- matrix(NA, nrow=B, ncol=10)
for(i in 1:B){
  ACT2[i,] <- evec2^2 * C2_ci$eigen_draws[i,1]
}

# Get ground truth
Ctrue <- matrix(0, nrow=10, ncol=10)
Ctrue[1:2, 1:2] <- matrix(c(8/3, 10/9, 10/9, 21/45), nrow=2, byrow=TRUE)
ACT0 <- act_scores(Ctrue)





if(save_results){
  png("figs/act_post_v2.png", height=5, width=8, units='in', res=300)
  boxplot(ACT, ylab="Activity Score (1)", xlab="Variable Number", boxwex=0.25, ylim=range(c(ACT, ACT2)))
  boxplot(ACT2, boxwex=0.25, col='firebrick1', add=TRUE, at= 0.25 + (1:10), border='firebrick')
  points(0.125+1:10, ACT0, pch=7, col='dodgerblue', lwd=1, cex=1.5)
  legend("topright", c("C_bass (posterior)", "C_GP (monte carlo)", "Ground Truth"),
         col=c("gray", "firebrick1", "dodgerblue"),
         pch=c(NA, NA, 7), lty=NA, lwd=c(1, 1, 1), fill=c("gray", "firebrick", NA),
         border=c("black", "firebrick", "white"), cex=1.3,
         bty="n")
  dev.off()
}







