# --------------------------------------
#            ESERCITAZIONE 5
#          Beta-Binomial Model 
#                (RJags)
# --------------------------------------

library(R2jags)
library(rjags)

setwd("/Users/marcoratta/Desktop/PhD/Teaching/Modelli Statistici")


########
# DATI #
########

y <- 2*34
N <- 2*100


#######################
# MODEL SPECIFICATION #
#######################

model <- "
     model{
         
         # Priors
         p ~ dbeta(1, 1)
        
         # Likelihood
         y ~ dbin(p,N)
     }

"
parameters <- c("p")



#######################
# POSTERIOR INFERENCE #
#######################

n.iter <- 100000
dati <- list(y=y, N=N)

post.sample <- coda.samples( model= jags.model(file = textConnection(model), data = dati),
                             variable.names = parameters, n.iter = n.iter, n.chains = 3, thin=1)

summary(post.sample)

p.post <- post.sample[[1]][,1]

# Guardo la densità a posteriori di P
plot(density(p.post), col="red", main="Posterior P", 
     xlab="P", ylab = "PDF")

# Confrontiamo con la Beta(1+y, 1+N-y)
lines(density(rbeta(100000,1+y, 1+N-y)), col="blue")



