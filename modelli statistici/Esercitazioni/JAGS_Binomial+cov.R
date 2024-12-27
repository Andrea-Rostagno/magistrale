# --------------------------------------
#            ESERCITAZIONE 5
#     Beta-Binomial Model + Covariate 
#                (RJags)
# --------------------------------------

library(R2jags)
library(rjags)

setwd("/Users/marcoratta/Desktop/PhD/Teaching/Modelli Statistici")


###################
# DATA GENERATION #
###################

# N <- 100
# a <- 0.5
# b <- 0.3
# x.norm <- rnorm(N,0,1)
# x <- 75 + 10*x.norm
# p.true <- exp(a + b*x.norm)/(1+exp(a + b*x.norm))
# y <- rbinom(N,1,p.true)
# df <- data.frame(succ=y, x=x)
# plot(df$x, df$succ)
# save(df, file="lifting.RData")


load("lifting.RData")

y <- df$succ
x <- (df$x - mean(df$x))/sqrt(var(df$x))

# Plotto i dati del dataset
plot(df$x, df$succ, pch=16, main = "Summary Data", col="red", 
     xlab = "Peso", ylab = "Successo(SI/NO)")

# Logit VS Prob
xx <- seq(0,1,0.001)
plot(xx, log(xx/(1-xx)), type="l", col="blue",
     main = "P vs Logit(P)", xlab = "P", ylab = "logit(P)")
abline(h=0, lty=2)




#######################
# MODEL SPECIFICATION #
#######################

model <- "
     model{
         
         # Priors
         a ~ dnorm(0, 0.00001)
         b ~ dnorm(0, 0.00001)
         
         # Likelihood
         for(i in 1:length(y)){
               y[i] ~ dbin(p[i],1)
               logit(p[i]) <- a + b*x[i]
         }
     }

"
parameters <- c("a", "b")



#######################
# POSTERIOR INFERENCE #
#######################

n.iter <- 10000

dati <- list(y=y, x=x)

post.sample <- coda.samples( model= jags.model(file = textConnection(model), data = dati),
                             variable.names = parameters, n.iter = n.iter, n.chains = 3, thin=1)

print(summary(post.sample))

# Rappresentazione grafica della probabilità di successo
a <- median(post.sample[[1]][,1])
b <- median(post.sample[[1]][,2])
xx <- seq(-4,4,0.01)
plot(xx, exp(a+b*xx)/(1+exp(a+b*xx)), type="l", col="blue",
     main = "Probabilità attesa di successo", xlab = "Peso Normalizzato", ylab = "P")


# Probabilità attese che persone con un certo peso X abbiano successo
p.post <- c()
X <- seq(50,100,5)
X.std <- (X - mean(df$x))/sqrt(var(df$x))

for(i in 1:length(X)){
  p.post[i] <- median(exp(post.sample[[1]][,1] + post.sample[[1]][,2]*X.std[i])/(1+exp(post.sample[[1]][,1] + post.sample[[1]][,2]*X.std[i])))
}

exp.succ <- data.frame(peso=X, P.succ=p.post)
print(exp.succ)


# Probabilità che il peso influisca sull'esito della sfida
mean(post.sample[[1]][,2]>0)




