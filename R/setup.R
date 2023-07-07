library(concordance)
library(BASS)
library(GBASS)
library(hetGP)
library(activegp)
library(lhs)
library(tictoc)
library(stargazer)
library(parallel)
library(RColorBrewer)

# When this flag is true, the results in figs and data will be overwritten
save_results <- FALSE

frob_norm <- function(D){
  1/nrow(D)*sqrt(mean(diag(tcrossprod(D))))
}
