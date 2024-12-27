# ---------------------------------------
#      GOODNESS OF FIT (GOF) TEST
# ---------------------------------------

#########################################################
# ADESIONE DEI DATI A NORMALE CON MEDIA E VARIANZA NOTA #
#########################################################

# Supponiamo di avere dei dati a disposizione e di voler valutare l'adesione
# dei dati a una distribuzione di riferimento (Normale standard).


# Genero i dati
N.sim <- 100
y <- rnorm(N.sim,0,1)

# Creo una partizione
N <- 10
s <- array(NA, dim=c(1,N))
s[1] <- -Inf
s[N] <- Inf
s[2:(N-1)] <- seq(-5,5,length.out = N-2)


# Calcolo le probabilità teoriche di osservare dati nei vari intervalli della
# partizione creata.
pi0 <- pnorm(s[2:N])-pnorm(s[1:N-1])

# Costruisco le probabilità empiriche
pi.hat <- c()
for(i in 1:(N-1)){
  pi.hat[i] <- mean(y<s[i+1] & y>s[i])
}

# Test costruito a mano
X2 <- sum(((pi.hat*N.sim-pi0*N.sim)^2)/(pi0*N.sim))
p.val <- pchisq(X2, N-2, lower.tail = FALSE)
print(p.val)

# Test con function "chisq.test"
chisq.test(x=pi.hat*N.sim, p=pi0)



#############################################################
# ADESIONE DEI DATI A NORMALE CON MEDIA E VARIANZA NON NOTA #
#############################################################

# Supponiamo di avere dei dati a disposizione e di voler valutare l'adesione
# dei dati a una distribuzione di riferimento i cui parametri sono stimati in
# maniera consistente a partire dai dati.


# Genero i dati
N.sim <- 100
y <- rnorm(N.sim,2,1)

# Stime dei parametri della distribuzione
mu.hat <- mean(y)
sigma.hat <- sqrt(var(y))

# Creo una partizione
N <- 10
s <- array(NA, dim=c(1,N))
s[1] <- -Inf
s[N] <- Inf
s[2:(N-1)] <- seq(-5,5,length.out = N-2)


# Calcolo le probabilità teoriche di osservare dati nei vari intervalli della
# partizione creata.
pi0 <- pnorm(s[2:N], mu.hat, sigma.hat)-pnorm(s[1:N-1], mu.hat, sigma.hat)

# Costruisco le probabilità empiriche
pi.hat <- c()
for(i in 1:(N-1)){
  pi.hat[i] <- mean(y<s[i+1] & y>s[i])
}

# Test costruito a mano
X2 <- sum(((pi.hat*N.sim-pi0*N.sim)^2)/(pi0*N.sim))
p.val <- pchisq(X2, (N-2)-2, lower.tail = FALSE)
print(p.val)
