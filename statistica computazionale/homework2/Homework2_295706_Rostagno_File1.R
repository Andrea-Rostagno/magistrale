# Esercizio 1
rm(list = ls()) #pulisco l'enviroment

#Impostiamo il seed
set.seed(295706) 

# Parametri
tau2 <- 0.5
sigma2 <- 5
phi <- 0.3
beta0 <- 2
beta1 <- 0.1
n <- 100

#Punto 1

# Generazione delle coordinate
s1 <- runif(n, 0, 10)
s2 <- runif(n, 0, 10)
D <- cbind(s1, s2) #combino s1 e s2 in una matrice 

# Creazione del grafico
plot(
  s1, s2,
  main = "Scatterplot delle coordinate (s1, s2)",
  xlab = "s1 (coordinata x)",
  ylab = "s2 (coordinata y)",
  pch = 16,       # Cerchi pieni per i punti
  col = "blue",   # Colore blu per i punti
  xlim = c(0, 10),
  ylim = c(0, 10)
)

# Aggiunta di etichette ai punti
text(s1, s2, labels = seq_along(s1), pos = 3, cex = 0.8, col = "red")

# Matrice di covarianza
distances <- as.matrix(dist(D)) #genero una matrice dove ogni punto di cordinate i j ha il valore della distanza tra i punti s_i e s_j
Sigma <- sigma2 * exp(-phi * distances)

# Valori medi
m <- beta0 + beta1 * s1

# Simulazione di W(s)
library(MASS)
W <- mvrnorm(1, mu = m, Sigma = Sigma)

# Simulazione di Y(s)
Y <- W + rnorm(n, mean = 0, sd = sqrt(tau2))#ho separato il vettore W da il rumore eps distribuito come una N(0,tao2^0.5)

#Punto 2

# Calcolo dei quartili di Y
quartili <- quantile(Y, probs = c(0.25, 0.5, 0.75))

# Assegno i colori in base ai quartili
colori <- rep("red", n)  # Primo gruppo, rendo tutte le osservazioni rosse
colori[Y > quartili[1] & Y <= quartili[2]] <- "blue"  # Secondo gruppo
colori[Y > quartili[2] & Y <= quartili[3]] <- "green" # Terzo gruppo
colori[Y > quartili[3]] <- "purple"                   # Quarto gruppo

# Scatterplot delle coordinate con colori
plot(
  s1, s2,
  main = "Scatterplot delle coordinate colorate per quartili di Y(s)",
  xlab = "s1 (coordinata x)",
  ylab = "s2 (coordinata y)",
  pch = 16,       # Cerchi pieni per i punti
  col = colori    # Colori assegnati ai punti
)

# Legenda per i colori
legend(
  "topright", 
  legend = c("< 25%", "25%-50%", "50%-75%", "> 75%"),
  col = c("red", "blue", "green", "purple"),
  pch = 16
)

#Punto 3

# Scelta casuale di n (tra 10 e 90)
n_obs <- sample(10:90, 1)  
cat("Numero di osservazioni selezionate (n):", n_obs, "\n")

# Generazione di un indice casuale per selezionare le osservazioni
indices <- sample(1:n, n_obs)

# Separazione di y_o e y_u
y_o <- Y[indices]       # Valori osservati
y_u <- Y[-indices]      # Valori non osservati

# Separazione delle coordinate
D_o <- D[indices, ]     # Coordinate osservate
D_u <- D[-indices, ]    # Coordinate non osservate

# Separazione di w_o e w_u
W_o <- W[indices]       # Valori osservati
W_u <- W[-indices]      # Valori non osservati

library(extraDistr)

# Numero di iterazioni
n_iter <- 5001  

# Calcolo dei limiti per phi
distances <- as.matrix(dist(D_o))
max_dist <- max(distances)       # Distanza massima
min_dist <- min(distances[distances > 0])  # Distanza minima non nulla
eta <- 1  #Primo valore della varianza della proposal
c <- 50   #Ogni quante iterazioni si cambia eta
recent_alpha <- numeric(c)  #Meorizza gli alpha di c in c
alpha_star <- 0.25          #Valore di riferimento (preso dalle slide)
A <- 100          #Costante

# Inizializzo i parametri
beta0 <- rnorm(1,0,1000)
beta1 <- rnorm(1,0,1000)
tau2 <- rinvgamma(1,1,1)
sigma2 <- rinvgamma(1,1,1)
phi <- runif(1, 3 / max_dist, 3 / min_dist)

# Vettori per salvare i campioni
samples_beta0 <- numeric(n_iter)
samples_beta0[1] <- beta0
samples_beta1 <- numeric(n_iter)
samples_beta1[1] <- beta1
samples_tau2 <- numeric(n_iter)
samples_tau2[1] <- tau2
samples_sigma2 <- numeric(n_iter)
samples_sigma2[1] <- sigma2
samples_phi <- numeric(n_iter)
samples_phi[1] <- phi

# Gibbs Sampler
for (t in 1:n_iter) {
  
  # Step 1: Campionamento di beta0
  Sigma <- samples_sigma2[t] * exp(-samples_phi[t] * (as.matrix(dist(D_o))))
  Sigma_inv <- solve(Sigma)
  var_beta0 <- 1/(t(rep(1,n_obs)) %*% Sigma_inv %*% rep(1,n_obs) + 1/1000)
  mean_beta0 <- -var_beta0 * (t(rep(1,n_obs)) %*% Sigma_inv %*% (W_o-beta1*D_o[,1]))
  beta0 <- rnorm(1, mean_beta0, sqrt(var_beta0))
  
  samples_beta0[t+1] <- beta0
  
  # Step 2: Campionamento di beta1
  var_beta1 <- 1/(t(D_o[,1]) %*% Sigma_inv %*% D_o[,1] + 1/1000)
  mean_beta1 <- -var_beta1 * (t(D_o[,1]) %*% Sigma_inv %*% W_o - samples_beta0[t+1] * t(rep(1,n_obs)) %*% Sigma_inv %*% D_o[,1])
  beta1 <- rnorm(1, mean_beta1, sqrt(var_beta1))
  
  samples_beta1[t+1] <- beta1
  
  # Step 3: Campionamento di tau2
  a <- (n_obs / 2) + 1
  b <- 1 + (t(y_o-W_o) %*% (y_o-W_o)) / 2
  tau2 <- rinvgamma(1, a, b)
  
  samples_tau2[t+1] <- tau2
  
  # Step 4: Campionamento di sigma2
  M <- exp(-samples_phi[t] * (as.matrix(dist(D_o))))
  M_inv <- solve(M)
  residuals <- W_o - samples_beta0[t + 1] * rep(1, n_obs) - samples_beta1[t + 1] * D_o[, 1]
  a <- (n_obs / 2) + 1
  b <- 1 + 0.5 * t(residuals) %*% M_inv %*% (residuals)
  sigma2 <- rinvgamma(1, a, b)
  
  samples_sigma2[t+1] <- sigma2
  
  # Step 5: Metropolis-Hastings per phi
  phi_proposal <- -1  
  while (phi_proposal < (3 / max_dist) | phi_proposal > (3 / min_dist)){
    phi_proposal <- rnorm(1, samples_phi[t], sqrt(eta))
  }
  
  #Calcolo della matrice C corrente e quella proposta e le loro inverse
  C <- samples_sigma2[t+1] * exp(-samples_phi[t] * (as.matrix(dist(D_o))))
  C_inv <- solve(C)
  C_proposal <- samples_sigma2[t+1] * exp(-phi_proposal * (as.matrix(dist(D_o))))
  C_proposal_inv <- solve(C_proposal) 
  
  # Calcolo del log-determinante
  log_det_current <- determinant(C, logarithm = TRUE)$modulus
  log_det_proposal <- determinant(C_proposal, logarithm = TRUE)$modulus
  log_det_term <- 0.5 * (log_det_current - log_det_proposal)
  
  # Calcolo del termine quadratico
  quad_current <- t(residuals) %*% C_inv %*% residuals
  quad_proposal <- t(residuals) %*% C_proposal_inv %*% residuals
  quad_term <- 0.5 * (quad_current - quad_proposal)
  
  # Calcolo del log(alpha)
  log_alpha <- log_det_term + quad_term
  
  # Calcolo di alpha
  alpha <- min(1, exp(log_alpha))

  # Aggiorno phi in base alla probabilità di accettazione
  u <- runif(1,0,1)
  if(u<=alpha){
    samples_phi[t+1] <- phi_proposal
  } else {
    samples_phi[t+1] <- samples_phi[t]
  }
  
  # Aggiornamento adattivo della varianza
  if (t %% c == 0) {
    recent_alpha[c] <- alpha
    mean_alpha <- mean(recent_alpha)  
    gamma_t <- A / (2 * A + t)
    eta <- exp(log(eta) + gamma_t * (mean_alpha - alpha_star))  # Aggiorna varianza
  }else{
    recent_alpha[t %% c] <- alpha  
  }
  
}

burn_in <- 1000  

# Rimuovo il burn-in
samples_beta0 <- samples_beta0[-(1:burn_in)]
samples_beta1 <- samples_beta1[-(1:burn_in)]
samples_tau2 <- samples_tau2[-(1:burn_in)]
samples_sigma2 <- samples_sigma2[-(1:burn_in)]
samples_phi <- samples_phi[-(1:burn_in)]

# Tracce dei parametri
par(mfrow = c(2, 3))
plot(samples_beta0, type = "l", main = "Traccia di beta0", xlab = "Iterazione", ylab = "beta0")
plot(samples_beta1, type = "l", main = "Traccia di beta1", xlab = "Iterazione", ylab = "beta1")
plot(samples_tau2, type = "l", main = "Traccia di tau2", xlab = "Iterazione", ylab = "tau2")
plot(samples_sigma2, type = "l", main = "Traccia di sigma2", xlab = "Iterazione", ylab = "sigma2")
plot(samples_phi, type = "l", main = "Traccia di phi", xlab = "Iterazione", ylab = "phi")

# Calcolo dell'ACF per la catena di phi
acf_result <- acf(samples_phi, main = "ACF della catena di phi", lag.max = 200, plot = TRUE)
















#Esercizio 2
rm(list = ls()) #pulisco l'enviroment

# Parametri
n <- 200
pi_k <- c(1/3, 1/3, 1/3)
lambda <- c(1, 10, 25)

#Punto 1

# Simulazione dei k
z <- sample(1:3, size = n, replace = TRUE, prob = pi_k)

# Simulazione dei dati
y <- rpois(n, lambda[z])

# Prepara i dati per il grafico
data <- data.frame(Index = 1:n, Value = y, Group = as.factor(z))

# Crea il grafico
library(ggplot2)

ggplot(data, aes(x = Index, y = Value, color = Group)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "green", "red")) +
  labs(title = "Distribuzione dei dati simulati (colorati per gruppo)",
       x = "Indice dell'osservazione",
       y = "Valore di Y",
       color = "Gruppo") +
  theme_minimal()

# Conta le occorrenze
conteggi <- table(data$Group)

# Grafico istogramma dei conteggi
ggplot(as.data.frame(conteggi), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = c("blue", "green", "red"), color = "black") +
  labs(title = "Istogramma delle occorrenze per gruppo",
       x = "Gruppo",
       y = "Conteggio",
       fill = "Gruppo") +
  theme_minimal()

#Punto 2

# Parametri iniziali
iter <- 3000 # Numero di iterazioni
K <- 3 # Numero di gruppi

# Inizializzazioni
lambda_curr <- c(rgamma(1,1,1), rgamma(1,1,1), rgamma(1,1,1)) # Stime iniziali per lambda
alpha <- c(1,1,1)
pi_curr <- rdirichlet(1,alpha)
z_curr <- sample(1:K, size = n, replace = TRUE) # Assegnazioni iniziali

# Per salvare i campioni
samples_lambda <- matrix(0, nrow = iter, ncol = K)
samples_lambda[1,] <- lambda_curr 
samples_pi <- matrix(0, nrow = iter, ncol = K)
samples_pi[1,] <- pi_curr
samples_z <- matrix(0, nrow = iter, ncol = n)
samples_z[1,] <- z_curr

# Algoritmo MCMC
for (t in 2:iter) {
  
  # Step 1: Campiona z_i
  for (i in 1:n) {
    prob <- pi_curr * dpois(y[i], lambda = lambda_curr)
    prob <- prob / sum(prob) # Normalizza le probabilità
    z_curr[i] <- sample(1:K, size = 1, prob = prob)
  }
  
  samples_z[t, ] <- z_curr
  
  # Step 2: Campiona lambda_k
  for (k in 1:K) {
    y_k <- y[samples_z[t,] == k]
    lambda_curr[k] <- rgamma(1, 1 + sum(y_k), 1 + length(y_k))
  }
  
  samples_lambda[t, ] <- lambda_curr
  
  # Step 3: Campiona pi_k
  n_k <- as.vector(table(samples_z[t,])) # Conta le osservazioni in ciascun gruppo
  alpha <- c(1,1,1) + n_k
  pi_curr <- rdirichlet(1,alpha)
  
  samples_pi[t, ] <- pi_curr
  
}

burn_in <- 200

# Rimuovo il burn-in
samples_z <- samples_z[-(1:burn_in),]
samples_lambda <- samples_lambda[-(1:burn_in),]
samples_pi <- samples_pi[-(1:burn_in),]

par(mfrow = c(2, 3))
plot(samples_lambda[,1], type = "l", main = "Traccia di lambda1", xlab = "Iterazione", ylab = "lambda1")
plot(samples_lambda[,2], type = "l", main = "Traccia di lambda2", xlab = "Iterazione", ylab = "lambda2")
plot(samples_lambda[,3], type = "l", main = "Traccia di lambda3", xlab = "Iterazione", ylab = "lambda3")
plot(samples_pi[,1], type = "l", main = "Traccia di pi1", xlab = "Iterazione", ylab = "pi1")
plot(samples_pi[,2], type = "l", main = "Traccia di pi2", xlab = "Iterazione", ylab = "pi2")
plot(samples_pi[,3], type = "l", main = "Traccia di pi3", xlab = "Iterazione", ylab = "pi3")























