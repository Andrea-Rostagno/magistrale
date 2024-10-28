# --------------------------
#      ESERCITAZIONE 3
#  Normal Inference (RJags)
# --------------------------


library(R2jags)
library(rjags)


###################
# DATA GENERATION #
###################

mu.true <- 2
var.true <- 1
y <- rnorm(500, mu.true, var.true)


#######################
# MODEL SPECIFICATION #
#######################

model <- "
     model{
     # distribuzioni a priori
         prec ~ dgamma(0.5,0.5)
         mean ~ dnorm(0,1/100000)
         # aggiormaento
         for(i in 1:n.sim){
            y[i] ~ dnorm(mean, prec)
         }
     }

"

parameters <- c("mean", "prec")

n.iter <- 10000
n.sim <- length(y)
dati <- list(y=y, n.sim=n.sim)



#######################
# POSTERIOR INFERENCE #
#######################

post.sample <- coda.samples( model= jags.model(file = textConnection(model), 
                             data = dati), variable.names = parameters, n.iter = n.iter,
                             n.chains = 1, thin =1 )


##############
# DIAGNOSTIC #
##############

print(summary(post.sample))

# Plot densities
par(mfrow=c(1,2))
plot(density(post.sample[[1]][,1]), lwd=1.5, main=paste("Posterior on", expression(mu)))
plot(density(1/post.sample[[1]][,2]), lwd=1.5, main=paste("Posterior on", expression(sigma)))

# Plot chains
par(mfrow=c(1,2))
plot(array(post.sample[[1]][,1]), type="l", cex=0.5, ylab=expression(mu), main="Samples for mu")
abline(h=median(post.sample[[1]][,1]), col="red")
plot(array(1/post.sample[[1]][,2]), type="l", cex=0.5, ylab=expression(sigma^2), main="Samples for sigma")
abline(h=median(1/post.sample[[1]][,2]), col="red")

# Plot first iterations
par(mfrow=c(1,2))
plot(post.sample[[1]][1:20,1], type="l", cex=0.5, ylab=expression(mu), main="Samples for mu")
abline(h=median(post.sample[[1]][,1]), col="red")
plot(1/post.sample[[1]][1:20,2], type="l", cex=0.5, ylab=expression(sigma^2), main="Samples for sigma")
abline(h=median(1/post.sample[[1]][,2]), col="red")

# Bivariate plot
par(mfrow=c(1,1))
plot(array(post.sample[[1]][,1]), array(1/post.sample[[1]][,2]), pch=16, cex=0.5, main="Bivariate representation",
     xlab=expression(mu), ylab=expression(sigma^2))

# Outlier
which.max(array(1/post.sample[[1]][,2]))
1/post.sample[[1]][1,2]

# Box plots
par(mfrow=c(1,1))
boxplot(list(mu=post.sample[[1]][,1], var=post.sample[[1]][,2]),  main="Box plots")

# Checking autocorrelations
par(mfrow=c(1,2))
acf(array(post.sample[[1]][,1]), main="ACF for mean")
acf(array(1/post.sample[[1]][,2]), main="ACF for variance")



par(mfrow=c(1,2))

# Model validation
mu.est <- median(post.sample[[1]][,1])
var.est <- median(1/post.sample[[1]][,2])
y.pred <- rnorm(n.sim, mu.est, sqrt(var.est))
plot(y, col="black", pch=16, main = "Data VS Predictions")
points(y.pred, col="green", pch=16)

# QQ plots
p <- seq(0.01,0.99,0.01)
q.true <- qnorm(p, mu.true, sqrt(var.true))
q.est <- qnorm(p, mu.est, sqrt(var.est))
plot(q.est, q.true, pch=16, xlab="Estimated quantiles", ylab="True quantiles", main="QQ plot")
abline(a=0, b=1, col="red")


# Other visualization tools
plot(post.sample)





