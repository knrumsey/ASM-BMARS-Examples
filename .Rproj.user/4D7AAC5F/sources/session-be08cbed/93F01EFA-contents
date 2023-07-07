#' The objective of this example is to demonstrate that using Bayesian MARS models with a t-likelihood
#' with the GBASS package (rather than BASS or hetGP) can lead to be estimation of Constantine's C
#' and, as a result, the active subspace.
source("R/setup.R", echo=FALSE)
set.seed(12901824)

# simple polynomial function
f <- function(x){
  x[1]^2 + x[1]*x[2] + x[2]^3/9
}


# start simulation
M <- 30                                     # number of simulations (set to 30 to match manuscript)
frobenius <- matrix(NA, nrow=M, ncol=3)     # matrix to store results
n <- 500                                    # number of observations
n_corrupted <- 10                           # number of corrupted observations
p <- 12                                     # number of inputs
C_true <- matrix(0, nrow=p, ncol=p)         # true C matrix
C_true[1:2,1:2] <- matrix(
  1/45*c(120, 50, 50, 21),
  nrow=2,
  byrow=TRUE)

for(m in 1:M){
  # simulate data (small noise-to-signal, but 1% of data are corrupted with high variance white-noise)

  X <- lhs::randomLHS(n, p)
  y <- apply(X, 1, f) + rnorm(n, 0, 0)
  y[1:n_corrupted] <- y[1:n_corrupted] + rnorm(n_corrupted, 0.5)

  # Fit models
  mod1 <- bass(X, y)           #, nmcmc=20000, nburn=18001, thin=2)
  mod2g <- tbass(X, y, df=5)   #, nmcmc=20000, nburn=18001, thin=2)
  mod2 <- gm2bm(mod2g)         # convert for compatibility with concordance package
  mod3 <- mleHomGP(X, y)

  # Calculate C matrices
  C1 <- C_bass(mod1)
  C2 <- C_bass(mod2)
  C3 <- C_GP((mod3))$mat

  # Get frobenius distance
  frobenius[m,1] <- frob_norm(C1 - C_true)
  frobenius[m,2] <- frob_norm(C2 - C_true)
  frobenius[m,3] <- frob_norm(C3 - C_true)
}
if(save_results){
  save(frobenius, file="output/E_frobenius.Rda")
}


# Make Figure
if(save_results){
  d23 <- density(frobenius[,2] - frobenius[,3], kernel="triangular")
  d21 <- density(frobenius[,2] - frobenius[,1], adj=1.5, kernel = "triangular")

  png("figs/frobenius_outliers.png", width=5, height=5, units="in", res=300)
  plot(NULL, xlim=range(c(d21$x, d23$x)), ylim=range(c(d23$y, d21$y)),
       xlab="", ylab="density")
  polygon(d23, border="white", col=adjustcolor("dodgerblue", alpha.f=1.0))
  polygon(d21, border="white", col=adjustcolor("orange", alpha.f=0.5))
  abline(v=0, lwd=2, lty=3, col="firebrick")
  legend("topleft", c("TBASS - GP", "TBASS - BASS"), fill=c(adjustcolor("dodgerblue", alpha.f=1.0), adjustcolor("orange", alpha.f=0.5)), cex=1.5)
  dev.off()

  # Make table
  tab <- apply(frobenius, 2, summary)
  colnames(tab) <- c("BASS", "TBASS", "GP")
  stargazer::stargazer(tab, summary=FALSE)
}




