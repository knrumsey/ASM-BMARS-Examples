source("R/setup.R", echo=FALSE)

# Constrained prior, uniform over [0,1]^2 * 1(x_1 > x_2)
C2_true <- matrix(c(19/6,  247/180,
                    247/180, 161/270), nrow=2, byrow=TRUE)

# Simulate data and train BASS model
X <- maximinLHS(500, 2)
Y <- apply(X, 1, f)
mod <- bass(X, Y)

# Approximate contraints using mixture of L boxes
L_vec <- c(1, 2, 3, 5, 10, 20, 30, 50, 100)
err_vec <- rep(NA, length(L_vec))
tim_vec <- rep(NA, length(L_vec))
for(iii in seq_along(L_vec)){
  L <- L_vec[iii]
  C_list <- list()
  C2 <- matrix(0, nrow=2, ncol=2)
  wt <- rep(NA, L)
  tic()
  for(ell in 1:L){
    pr <- list()
    pr[[1]] <- list(dist="uniform", trunc=c(ell, L+1)/(L+1))
    pr[[2]] <- list(dist="uniform", trunc=c(ell-1, ell)/(L+1))
    C_list[[ell]] <- C_bass(mod, prior=pr)
    wt[ell] <- (1 - ell/(L+1))*(1/(L+1))

  }
  wt <- wt/sum(wt)
  for(ell in 1:L){
    C2 <- C2 + C_list[[ell]]*wt[ell]
  }
  tt = toc()
  err_vec[iii] <- sqrt(sum((C2/sum(wt) - C2_true)^2))/2
  tim_vec[iii] <- tt$toc - tt$tic
  print(iii)
}

if(save_results){
  png("figs/constrained_poly_plot.png",
      height=5, width=8, units="in", res=300)
  par(mfrow=c(1,1))
  par(mar=c(5.1, 4.1, 4.1, 5.1), xpd=FALSE)
  plot(L_vec, err_vec, ylim=c(0, max(err_vec)), pch=16, cex=2, xlab="Number of Mixture Components", ylab="Error", cex.lab=1.2, xaxt='n', type='o')
  lines(L_vec, tim_vec/max(tim_vec)*max(err_vec), lty=2)
  points(L_vec, tim_vec/max(tim_vec)*max(err_vec), pch=15, cex=2)
  axis(1, L_vec, L_vec)

  ttt = tim_vec
  axis(4, seq(0, 7, by=1)/max(tim_vec)*max(err_vec), seq(0, 7, by=1))
  mtext("Time (seconds)", side=4, line=3, cex=1.2)
  abline(h=0)
  legend('top', c("Error", "Runtime"), lty=1:2, pch=16:15, cex=2, bty='n')
  dev.off()
}


