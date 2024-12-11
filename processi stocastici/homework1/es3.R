rm(list = ls())
set.seed(295706) 
jumps_max<-500 #numero max salti
t<-0 #tempo iniziale
jumps<-0 #contatore salti
jump_times1 <- c()
lambda_func <- function(t) { #funzione lamda
  100/(t+1)
}
while (jumps<jumps_max) {#ripeto finchè non ottengo 500 salti
  lambda_max <- lambda_func(t) #calcolo il massimo valore landa per il tempo disponibile
  tau <- rexp(1, rate = lambda_max) #calcolo un incremento del tempo tramite esponenziale
  t <- t + tau
  u <- runif(1) #campione uniforme
  if (u < lambda_func(t) / lambda_max) {#condizione accetto o rifiuto
    jumps <- jumps + 1
    jump_times1 <- c(jump_times1, t)
  }
}
##########################################################################

max_time<-150
t <- 0
jump_times2 <- c()
while (t < max_time) {
  lambda_max <- lambda_func(t)
  tau <- rexp(1, rate = lambda_max) 
  t <- t + tau
  if (t > max_time) { #interrompo se supero il t max
    break
  }
  u <- runif(1)
  if (u < lambda_func(t) / lambda_max) {
    jump_times2 <- c(jump_times2, t)
  }
}

