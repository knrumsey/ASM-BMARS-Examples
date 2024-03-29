source("R/setup.R", echo=FALSE)
set.seed(121215)

# Load stochastic SIR data
load("data/stochastic_sir_data.Rda")

# Fit quantile regression models
q_vec <- c(0.1, 0.25, 0.5, 0.75, 0.9)
mods <- mclapply(q_vec,
                 function(qq) qbass(X,
                                    y,q=qq,
                                    maxInt=3,
                                    w_prior=list(type="GIG", p=-0.001, a=0, b=0.001, prop_sigma=0.1),
                                    a_lambda=.01, b_lambda=.01,
                                    nmcmc=10000, nburn=8001, thin=2,
                                    Iw0=c(10, 30, 5),
                                    Zw0=c(50, 100, 20, 20)),
                 mc.cores = 5, mc.preschedule = F)

# Get C matrices for each model
modsg <- list()
C_list <- list()
A <- matrix(NA, nrow=length(q_vec), ncol=4)
for(i in seq_along(q_vec)){
  modsg[[i]] <- gm2bm(mods[[i]])
  C_list[[i]] <- C_bass(modsg[[i]])
  A[i,] <- act_scores(C_list[[i]])
}
A <- A/1e6 # Change units

# Make figure
if(save_results){
  png("figs/activity_scores_sir.png", width=5, height=5, units="in", res=300)
  matplot(A, type='o',
          xlab="quantile", xaxt='n',
          ylab="activity score",
          lwd=2,
          pch=16)
  axis(1, 1:5, q_vec)
  legend("top", c("x1", "x2", "x3", "x4"), lty=1:4, lwd=2, col=1:4)
  dev.off()
}
