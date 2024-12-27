# ---------------------------------
#        ESERCITAZIONE 5
#    Beta-Binomial Model (RJags)
#    Clinical Trial Application
# ---------------------------------

library(R2jags)
library(rjags)


n.trials <- 10
pt.true <- 0.7
pc.true <- 0.5
N <- 100

succ <- c()


#######################
# MODEL SPECIFICATION #
#######################

model <- "
     model{
         
         # Priors
         pc ~ dbeta(1, 1)
         pt ~ dbeta(1, 1)
         
         # Likelihood
         for(i in 1:length(yc)){
               yc[i] ~ dbin(pc,1)
         }
         for(i in 1:length(yt)){
               yt[i] ~ dbin(pt,1)
         }
         
         delta <- log((pt*(1-pc))/(pc*(1-pt)))
     }

"
parameters <- c("pc", "pt", "delta")



for(k in 1:n.trials){
  
  ###################
  # DATA GENERATION #
  ###################
  
  yc <- rbinom(N/2,1,pc.true)
  yt <- rbinom(N/2,1,pt.true)
  
  #######################
  # POSTERIOR INFERENCE #
  #######################
  
  n.iter <- 1000
  
  dati <- list(yc=yc, yt=yt)
  
  post.sample <- coda.samples( model= jags.model(file = textConnection(model), data = dati),
                               variable.names = parameters, n.iter = n.iter, n.chains = 3, thin=1)
  
  delta.post <- post.sample[[1]][,1]
  succ[k] <- mean(delta.post > 0) > 0.95

}

# Guardo la densità a posteriori del log(OR) per l'ultima simulazione 
plot(density(delta.post), col="red", main="Posterior Log(OR)", 
     xlab="Log(OR)", ylab = "PDF")
abline(v=0, lty=2)


# Se il trattamento è efficace (pt>pc) il tasso di successo si chiama Power,
# se il trattamento non lo è (pt=pc) il tasso di successo è un errore di
# primo tipo (t1E)

pow <- mean(succ)



