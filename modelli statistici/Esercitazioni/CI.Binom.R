# ------------------------------------------
#   CONFIDENCE INTERVALS FOR PROPORTIONS
# ------------------------------------------

library(fastR2)
library(GenBinomApps)

N <- 100
K <- seq(0,N)

CI.Wald <- CI.Wilson <- CI.Clopper <- array(NA, dim=c(N+1,2))

for(k in K){
  CI.Wald[k+1,] <- wald.ci(k,N, conf.level = 0.95)[1:2]
  CI.Wilson[k+1,] <- wilson.ci(k,N, conf.level = 0.95)[1:2]
  CI.Clopper[k+1,] <- c(clopper.pearson.ci(k,N, alpha=0.05, CI="two.sided")$Lower.limit,
                      clopper.pearson.ci(k,N, alpha=0.05, CI="two.sided")$Upper.limit)
}

plot(K,CI.Wald[,1], type="l", col="red", ylim=c(-0.1,1.1), xlab = "Successi", ylab = "P",
     main="CI per proporzioni")
lines(K,CI.Wald[,2], col="red")
lines(K,CI.Wilson[,1], col="blue")
lines(K,CI.Wilson[,2], col="blue")
lines(K,CI.Clopper[,1], col="green")
lines(K,CI.Clopper[,2], col="green")



#########################
# Copertura dei vari CI #
#########################

n.sim <- 1000
pi <- seq(0,1,0.05)

CI.Wald <- CI.Wilson <- CI.Clopper <- array(NA, dim=c(n.sim,2))
cov.Wald <- cov.Wilson <- cov.Clopper <- c()
succ.Wald <- succ.Wilson <- succ.Clopper <- c()


for(i in 1:length(pi)){
  Ks <- rbinom(n.sim,N,pi[i])
  for(k in 1:n.sim){
    CI.Wald[k,] <- wald.ci(Ks[k],N, conf.level = 0.95)[1:2]
    succ.Wald[k] <- pi[i]>=CI.Wald[k,1] & pi[i]<=CI.Wald[k,2]
    CI.Wilson[k,] <- wilson.ci(Ks[k],N, conf.level = 0.95)[1:2]
    succ.Wilson[k] <- pi[i]>=CI.Wilson[k,1] & pi[i]<=CI.Wilson[k,2]
    CI.Clopper[k,] <- c(clopper.pearson.ci(Ks[k],N, alpha=0.05, CI="two.sided")$Lower.limit,
                    clopper.pearson.ci(Ks[k],N, alpha=0.05, CI="two.sided")$Upper.limit)
    succ.Clopper[k] <- pi[i]>=CI.Clopper[k,1] & pi[i]<=CI.Clopper[k,2]
  }
  cov.Wald[i] <- mean(succ.Wald)
  cov.Wilson[i] <- mean(succ.Wilson)
  cov.Clopper[i] <- mean(succ.Clopper)
}


res.table <- data.frame(cbind(pi,cov.Clopper, cov.Wald, cov.Wilson))

print(res.table)
