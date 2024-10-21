# ------------------------------
#           LEZIONE 2
#      Campionamento Normale
# ------------------------------


######################
# Normale Univariata #
######################


# Simulare dati da una normale
N <- 1000
m0 <- 0
s0 <- 1
sample <- rnorm(N, mean = m0, sd = s0)

par(mfrow=c(1,2))

# Scatter plot delle osservazioni
plot(seq(1:N), sample, pch=16, col="red", main = "Scatter Normale",
     xlab = "Indice", ylab = "Campione")
abline(h=c(m0-s0,m0,m0+s0), col="blue", lty=2, lwd=1.5)

# Densità stimata e vera
plot(density(sample), lwd=1.5, main = "PDF", xlab = "x", ylab = "f(x)")
lines(seq(-4*s0,4*s0,0.001), dnorm(seq(-4*s0,4*s0,0.001), mean = m0, sd = s0),
      col="orange", lty=1, lwd=1.5)

par(mfrow=c(1,1))

# Controllo quantili teorici ed empirici
p <- seq(0.01,1,0.01)
q.emp <- quantile(sample,p)
q.true <- qnorm(p,mean=m0,sd=s0)
plot(q.emp, q.true, col="green", pch=16, lwd=1.5, main = "QQ Plot",
     xlim=c(min(q.emp),max(q.emp)), ylim=c(min(q.emp),max(q.emp)),
     xlab = "Quantili Empirici", ylab = "Quantili Teorici")
abline(a=0, b=1, lty=2)






############################
#    Normale Univariata    #
#       (mean shift)       #
############################


# Simulare dati da una normale
N <- 1000
m0 <- c(0,4)
s0 <- 1
sample <- rnorm(N, mean = c(rep(m0[1], N/2), rep(m0[2], N-N/2)), sd = s0)

par(mfrow=c(1,2))

# Scatter plot delle osservazioni
plot(seq(1:N), sample, pch=16, col="red", main = "Scatter Normale Mean Shift",
     xlab = "Indice", ylab = "Campione", cex=0.7)
abline(h=c(m0[1],m0[2]), col="blue", lty=2, lwd=1.5)


# Densità stimata e vera
plot(density(sample), lwd=1.5, main = "PDF", xlab = "x", ylab = "f(x)")
lines(seq((min(m0)-4*s0),(max(m0)+4*s0),0.001),(1/2)*dnorm(seq(min(m0)-4*s0, max(m0)+4*s0,0.001), mean = m0[1], sd = s0) + (1/2)*dnorm(seq(min(m0)-4*s0, max(m0)+4*s0,0.001), mean = m0[2], sd = s0),
      col="orange", lty=1, lwd=1.5) 






############################
#    Normale Univariata    #
#       (mean drift)       #
############################

# Simulare dati da una normale
N <- 1000
m0 <- seq(0,N-1)
s0 <- 30
sample <- rnorm(N, mean = m0, sd = s0)

par(mfrow=c(1,2))

# Scatter plot delle osservazioni
plot(seq(1:N), sample, pch=16, col="red", main = "Scatter Normale Mean Drift",
     xlab = "Indice", ylab = "Campione", cex=0.7)
abline(a=0, b=1, col="blue", lty=2, lwd=1.5)


# Densità stimata e vera
plot(density(sample), lwd=1.5, main = "PDF", xlab = "x", ylab = "f(x)")






############################
#    Normale Univariata    #
#     (variance shift)     #
############################


# Simulare dati da una normale
N <- 1000
m0 <- c(0)
s0 <- c(1,2)
sample <- rnorm(N, mean = m0, sd = c(rep(s0[1], N/2), rep(s0[2], N-N/2)))

par(mfrow=c(1,2))

# Scatter plot delle osservazioni
plot(seq(1:N), sample, pch=16, col="red", main = "Scatter Normale Variance Shift",
     xlab = "Indice", ylab = "Campione", cex=0.7)
abline(h=m0, col="blue", lty=2, lwd=1.5)


# Densità stimata e vera
plot(density(sample), lwd=1.5, main = "PDF", xlab = "x", ylab = "f(x)")







############################
#    Normale Univariata    #
#     (variance drift)     #
############################


# Simulare dati da una normale
N <- 1000
m0 <- c(0)
s0 <- seq(1,5,length.out =N)
sample <- rnorm(N, mean = m0, sd = s0 )

par(mfrow=c(1,2))

# Scatter plot delle osservazioni
plot(seq(1:N), sample, pch=16, col="red", main = "Scatter Normale Variance Shift",
     xlab = "Indice", ylab = "Campione", cex=0.7)
abline(h=m0, col="blue", lty=2, lwd=1.5)


# Densità stimata e vera
plot(density(sample), lwd=1.5, main = "PDF", xlab = "x", ylab = "f(x)")







############################
#    Normale Univariata    #
#  (with autocorrelation)  #
############################

N <- 100
m0 <- 0
s0 <- 1

x1 <- rnorm(N, mean=m0, sd=s0)
sample <- x1[2:N]+x1[1:(N-1)]

par(mfrow=c(1,3))

# Scatter plot delle osservazioni
plot(seq(1:(N-1)), sample, pch=16, col="red", main = "Scatter Normale Autocorrelazione",
     xlab = "Indice", ylab = "Campione", cex=0.7)
abline(h=m0, col="blue", lty=2, lwd=1.5)

# Altra visualizzazione (più chiara)
plot(x1[1:N-1], sample, pch=16, col="red", main = "Scatter Normale Autocorrelazione",
     xlab = "X", ylab = "Campione", cex=0.7)

# Densità
plot(density(sample), lwd=1.5, main = "PDF", xlab = "x", ylab = "f(x)")
lines(seq(-4*2*s0,4*2*s0,0.001), dnorm(seq(-4*2*s0,4*2*s0,0.001), mean = m0, sd = s0),
      col="orange", lty=1, lwd=1.5)








########################
# Normale Multivariata #
########################

library("mvtnorm")
library("MASS")
library('ash')


N <- 1000
m0 <- c(1,3)
s0 <- c(1,1)
rhos <- c(-0.9, -0.3, 0.3, 0.9)



for(R in rhos){
  
  corr.mat <- matrix(c(s0[1]^2, prod(s0)*R, prod(s0)*R, s0[2]^2), ncol=2)
  sample <- mvrnorm(n = N, mu = m0, Sigma = corr.mat)
  
  par(mfrow=c(1,1))
  
  # Scater Normale Bivariata
  plot(sample[,1], sample[,2], pch=16, col="red", main = paste(expression(rho), "=", R), 
       xlab = "X1", ylab = "X2", cex=0.7)
  
  par(mfrow=c(1,1))
  
  # Densità Normale Bivariata
  bin2d <- bin2(sample, nbin=c(30, 30))
  bin2d.sm <- ash2(bin2d)
  filled.contour(bin2d.sm, xlab = "X1", ylab = "X2", 
                 main = "PDF")
  
}





