# ------------------------------
#        ESERCITAZIONE 3
#    Hierarchical Model (RJags)
# ------------------------------

# In order to understand how many resources to allocate for next
# year (rooms, canteens ...) a university needs to make a prediction
# on how many students will pass the admission test next year.
# In order to do so, the university relies on some data collected 
# this year, in particular the score (pass/not pass) of each student
# of each high school.
#
# Assumptions:
# - Number of students applying for the university is random between
#   10 and 30
# - Each student has a probability to pass the test p[s] (s is the index
#   of the school) which is proportional to how good is the high school
#   he comes from
# - It is supposed that p[s] of each school depends on the quality of instruction
#   in the whole country
#
# Goal:
# - Estimate how many resources the university should allocate next year
#   to be confident enough to provide good services for everyone.
# - Study how the number of schools estimated for next year impact on
#   the predictions.
# - Study how the latter changes if we have prior information regarding
#   the quality of instruction
# - Study how the number of schools used for the analysis impacts on
#   the predictions.



library(R2jags)
library(rjags)



###################
# DATA GENERATION #
###################

set.seed(1234)

n.schools <- 20
p.glob.true <- 0.4
a.true <- 4

N <- round(runif(n.schools, 10, 30))
p <- rbeta(n.schools, a.true, ((1-p.glob.true)/p.glob.true)*a.true)

print(mean(p))

p.vec <- c()
p.obs <- c()
school <- c()

for(i in 1:n.schools){
  p.vec <- c(p.vec, rep(p[i], N[i]))
  school <- c(school, rep(i, N[i]))
}

pass <- rbinom(sum(N), 1, p.vec)
df <- data.frame(pass=pass, school=school)

for(i in 1:n.schools){
  p.obs[i] <- mean(df$pass[df$school==i])
}


par(mfrow=c(1,1))
plot(seq(1,n.schools), p, pch=16, col="black", ylim=c(0,1),
     xlab = "schools", ylab = "True Probabilities", main="Data generated")
points(seq(1,n.schools), p.obs, pch=5, col="black")
abline(h=mean(p), lty=2, col="red")
legend("bottom", c("Observed P", "True P"), pch=c(5,16))

print(df)




#######################
# MODEL SPECIFICATION #
#######################

model <- "
     model{
         
         # Priors
         
         p.glob ~ dbeta(a.glob, b.glob)
         a ~ dunif(1,100)
         for(i in 1:n.schools){
           p[i] ~ dbeta(a, ((1-p.glob)/p.glob)*a)
         }
         
         
         # Likelihood
     
         for(i in 1:n.students){
               y[i] ~ dbin(p[school[i]],1)
         }
     }

"
parameters <- c("p", "p.glob", "a")


################################
# TYPES OF PRIOR DISTRIBUTIONS #
################################

# Function to estimate Beta parameters given mean and variance
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

a.glob <- c()
b.glob <- c()

mus.glob <- c(0.5, 0.9, 0.1)
vars.glob <- c(0.288^2, 0.05^2, 0.05^2)

for(i in 1:length(mus.glob)){
  BetaPar <- estBetaParams(mus.glob[i], vars.glob[i])
  a.glob[i] <- BetaPar$alpha
  b.glob[i] <- BetaPar$beta
}


xx <- seq(0,1,0.0001)
prior.vague <- dbeta(xx, a.glob[1], b.glob[1])
prior.optim <- dbeta(xx, a.glob[2], b.glob[2])
prior.pessim <- dbeta(xx, a.glob[3], b.glob[3])

par(mfrow=c(1,3))
plot(xx, prior.vague, type="l", main="Vague Prior")
plot(xx, prior.optim, type="l", main="Optimistic Prior")
plot(xx, prior.pessim, type="l", main="Pessimistic Prior")





#######################
# POSTERIOR INFERENCE #
#######################

n.iter <- 10000

dati <- list(y=df$pass, school=df$school, n.schools=length(p),
             n.students=nrow(df), a.glob=a.glob[1], b.glob=b.glob[1])

post.sample <- coda.samples( model= jags.model(file = textConnection(model), 
                                               data = dati), variable.names = parameters, n.iter = n.iter,
                             n.chains = 1, thin =1 )

print(summary(post.sample))




###############
# PREDICTIONS #
###############

ns.list <- c(5,10,20,50)
n.sim.pred <- 1000
perc.amm <- array(NA, dim=c(length(ns.list),n.sim.pred))

for(ns in 1:length(ns.list)){
  n.schools.new <- ns.list[ns]
  for(k in 1:n.sim.pred){
    
    p.glob <- post.sample[[1]][k,n.schools+2]
    a <- median(post.sample[[1]][k,1])
    
    N.new <- round(runif(n.schools.new, 10, 30))
    p.new <- rbeta(n.schools.new, a, ((1-p.glob)/p.glob)*a)
    
    p.pred <- c()
    pass.pred <- c()
    
    for(i in 1:n.schools.new){
      p.pred <- c(p.pred, rep(p.new[i], N.new[i]))
    }
    
    pass.pred <- rbinom(sum(N.new), 1, p.pred)
    perc.amm[ns,k] <- round(mean(pass.pred), 3)
  }
}



# Density Plots
par(mfrow=c(1,1))
colors <- rainbow(length(ns.list))
plot(density(perc.amm[1,]), col=colors[1],  lwd=1.5, ylim=c(0,12), main="Superamento Medio Test")
for(i in 2:length(ns.list)){
  lines(density(perc.amm[i,]), col=colors[i], lwd=1.5)
}

legend("topright", c("5 schools", "10 schools", "20 schools", "50 schools"),
       lty=1, col=colors)



# Summary Table
summary.table <- data.frame(array(NA, dim=c(length(ns.list), 4)))
for(i in 1:length(ns.list)){
  summary.table[i,] <- quantile(perc.amm[i,], c(0.5, 0.8, 0.9, 0.95), names = FALSE)
}

rownames(summary.table) <- c("5 schools", "10 schools", "20 schools", "50 schools")
colnames(summary.table) <- c("Median", "80%", "90%", "95%")

print(summary.table)



