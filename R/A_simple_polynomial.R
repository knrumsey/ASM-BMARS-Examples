#' This script generates results for the simply polynomial model under a uniform prior.
#' Compares C_bass and C_GP functions for a various values of p
#'   NOTE: This script can take a long time to run for N=5000 and (especially) for
#'         N=50,000. The default settings are for the N=50 case which can run somewhat quickly.
#'         Because of this, this script only partially recreates Figure 1. Full reconstruction
#'         of Figure 1 would require running this script 3 times for N=50, N=5000 and N=50,000
#'         and modifying the code for creating the figure to display all of the results.

source("setup.R", echo=FALSE)

# Toggle parameters
N <- 50
p_vec <- 2^(1:7)
do_GP <- c(T, T, T, T, T, F, F)
tladder <- 1.1^(0:5)

# Function and true C and a1
f <- function(x){
  x[1]^2 + x[1]*x[2] + x[2]^3/9
}
Ctrue <- matrix(0, nrow=max(p_vec), ncol=max(p_vec))
Ctrue[1:2, 1:2] <- matrix(c(8/3, 10/9, 10/9, 21/45), nrow=2, byrow=TRUE)
atrue <- eigen(Ctrue)$vectors[,1]

# Simulation study
METRICS <- array(0, dim=c(2, length(p_vec), 4))
if(N < 1100){
  X <- maximinLHS(N, max(p_vec))
}else{
  X <- randomLHS(N, max(p_vec))
}
Y <- apply(X, 1, f)


for(i in 1:length(p_vec)){
  p <- p_vec[i]
  XX <- X[,1:p]
  print(p)

  # START BASS MODEL
  tic()
  mod0 <- bass(XX, Y, temp.ladder=tladder, verbose=FALSE)
  t1 <- toc(quiet=TRUE)
  tic()
  C0 <- C_bass(mod0)
  t2 <- toc(quiet=TRUE)

  # COMPUTE BASS METRICS
  METRICS[1, i, 1] <- (t1$toc - t1$tic)/length(tladder) # Time per processor
  METRICS[1, i, 2] <- (t2$toc - t2$tic)
  METRICS[1, i, 3] <- sqrt(sum(diag(tcrossprod(C0-Ctrue[1:p, 1:p]))))/p
  METRICS[1, i, 4] <- min(sqrt(sum((eigen(C0)$vectors[,1] - atrue[1:p])^2)), sqrt(sum((eigen(C0)$vectors[,1] + atrue[1:p])^2)))

  # START GP MODEL
  if(do_GP[i]){
    tic()
    mod1 <- mleHomGP(XX, Y)
    t1 <- toc(quiet=TRUE)
    tic()
    C1 <- C_GP(mod1)[[1]]
    t2 <- toc(quiet=TRUE)

    # COMPUTE GP METRICS
    METRICS[2, i, 1] <- (t1$toc - t1$tic)
    METRICS[2, i, 2] <- (t2$toc - t2$tic)
    METRICS[2, i, 3] <- sqrt(sum(diag(tcrossprod(C1-Ctrue[1:p, 1:p]))))/p
    METRICS[2, i, 4] <- min(sqrt(sum((eigen(C1)$vectors[,1] - atrue[1:p])^2)), sqrt(sum((eigen(C1)$vectors[,1] + atrue[1:p])^2)))
  }
}

if(save_results){
  save(METRICS, file=paste0("data/simple_sim_study_N", N, ".rda"))

  # Just a quick plot
  title <- c("Time to fit model", "Time to estimate C", "Error in C", "Error in a1")
  axtitle <- c("Time (seconds)", "Time (seconds)", "", "")

  par(mfrow=c(2,2))
  for(k in 1:4){
    tmp <- unlist(log(METRICS[,,k]))
    tmp <- tmp[-which(abs(tmp) == Inf)]
    plot(log(p_vec), log(METRICS[1,,k]), type='o', col='dodgerblue', ylim=range(tmp), xaxt='n', yaxt='n', xlab='Number of Inputs', ylab=axtitle[k], main=title[k], pch=15, lwd=3, cex=2)
    lines(log(p_vec), log(METRICS[2,,k]), col='orange', lwd=3)
    points(log(p_vec), log(METRICS[2,,k]), col='orange', pch=16, cex=2)
    axis(1, log(p_vec), p_vec)
    axis(2, log(10^(-3:ceiling(log10(max(METRICS[,,k]))))), 10^(-3:ceiling(log10(max(METRICS[,,k])))))
  }
  legend("bottomright",
         c("Bayesian MARS", "Gaussian Process"),
         col=c("dodgerblue", "orange"),
         lwd=3, pch=c(15:16),
         cex=1)
}





