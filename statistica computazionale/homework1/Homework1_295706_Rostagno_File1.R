# Esercizio 1
rm(list = ls()) #pulisco l'enviroment

#Impostiamo il seed
set.seed(295706) 

#Impostiamo i parametri
a <- runif(1,2.5,5.5)
b <- 1
p <- 0.5
lambda <- runif(1,2,4)
n_campioni <- 1000 

#Punto 1
Z <- rbinom(n_campioni,1,p) #mettendo la size=1 posso generare un campione di una bernoulli con probabilità p
Y <- c() #creo vettore y vuoto
for (i in 1:n_campioni) { #per ogni campione verifico se è uguale a zero o ad 1 
  if(Z[i]==0){
    Y[i] <- rpois(1,lambda)#se è uguale a 0 calcolo un campione rispetto ad una poisson
  }
  else{
    Y[i] <- rgamma(1,a,b)#se è uguale a 1 calcolo un campione rispetto ad una gamma
  }
}
yseq <- seq(0,quantile(Y, 0.95),0.05)#genero la sequenza per visualizzare al meglio la cdf sul grafico (ho fatto alcune prove e credo che questa sia un'ottima sequenza)
cdf_Y <- ecdf(Y)(yseq) #comando per calcolare la cdf
plot(yseq, cdf_Y, main = "Stima Monte Carlo della CDF di Y", xlab = "y", ylab = "F(y)", col = "red", lwd = 2, type = "s")#comando per plottare

#Punto 2
intervallo <- c(3.5, 4.5)
prob_Y1 <- (sum(Y >= intervallo[1] & Y <= intervallo[2]))/ n_campioni#calcolo quanti campioni di Y sono presenti nell'intervallo e divido per il numero totale di campioni
Y_Z0 <- Y[Z == 0]#memorizzo le posizioni dove z=0 e dopodiche salvo nel vettore y_z0 i corrispettivi valori di y
prob_Y2 <- (sum(Y_Z0 >= intervallo[1] & Y_Z0 <= intervallo[2]))/ length(Y_Z0)#calcolo quanti campioni di Y_z0 sono presenti nell'intervallo e divido per il numero totale di campioni di y_z0
var_Y1 <- prob_Y1 *(1-prob_Y1)/n_campioni#per calcolare la varianza dello stimatore uso che la varianza per una variabile bernoulliana si calcola con p(1-p)/n, dove p è la probabilità appena calcolata 
var_Y2 <- prob_Y2 *(1-prob_Y2)/length(Y_Z0)

#Punto 3
intervallo2 <- c(1.5, 3.5)
f_marginale_intervallo <- (sum(Y >= intervallo2[1] & Y <= intervallo2[2]))/ n_campioni#calcolo la probabilità che i campioni di y cadano nell'intervallo
f_marginale_z0 <- (sum(Z==0))/ n_campioni#calcolo la probabilità di avere 0 nel campione z
f_marginale_z1 <- (sum(Z==1))/ n_campioni#calcolo la probabilità di avere 1 nel campione z
f_condizionata_z0 <- (sum(Y_Z0 >= intervallo2[1] & Y_Z0 <= intervallo2[2]))/ length(Y_Z0)#calcolo la probabilità di avere dei valori di y nell'intervallo condizionato che z=0
Y_Z1 <- Y[Z == 1]#trovo i valori y generati da valori z=1
f_condizionata_z1 <- (sum(Y_Z1 >= intervallo2[1] & Y_Z1 <= intervallo2[2]))/ length(Y_Z1)#calcolo la probabilità di avere dei valori di y nell'intervallo condizionato che z=1
prob_Z0_cond <- f_condizionata_z0*f_marginale_z0/f_marginale_intervallo#applico la formula di bayes in entrambi i casi
prob_Z1_cond <- f_condizionata_z1*f_marginale_z1/f_marginale_intervallo

#Punto 4    #ho implementato l'inversa generalizzata come richiesto
quantili <- c(0.1, 0.2, 0.5, 0.75)
inversa_generalizzata <- function(campione,prob){#creo una funzione che implementi la inversa generalizzata
  campione_ordinato <- sort(campione)#ordino il campione in maniera crescente
  for (i in 1:n_campioni) {#calcolo il valore di ogni campione nella cdf e verifico se è maggiore della probabilita inserita
    if(ecdf(campione)(campione_ordinato[i])>=prob){
      return(campione_ordinato[i])#in caso affermativo ritorno il primo valore che soddisfa la disuguaglianza
    }
  }
}
quantili_Y <- sapply(quantili, function(p) inversa_generalizzata(Y, p))#calcolo la inversa generalizzata per ogni valore dato
quantili_Z <- sapply(quantili, function(p) inversa_generalizzata(Z, p))

#Punto 5  #ho modificato la densita di gamma
y_punti <- seq(0, 10, by = 0.25)#creo i punti
densita_poisson <- c()
densita_gamma <- c()
for (i in 1:length(y_punti)) {
  densita_poisson[i] <- sum(Y_Z0 == y_punti[i]) / length(Y_Z0)#calcolo la probabilità di osservare ogni punto rispetto al campione Y
}

for (i in 1:length(y_punti)) {
  densita_gamma[i] <- dgamma(y_punti[i],a,b)#calcolo la probabilità di osservare ogni punto 
}

plot(y_punti, densita_poisson,type = "h",  col = "blue", lwd = 2, xlab = "y", ylab = "Densità", main = "Stima Monte Carlo della densità di y", ylim = c(0, max(densita_poisson, densita_gamma)))#plotto facendo degli aggiustamenti sull asse y
lines(y_punti, densita_gamma, type = "l", col = "red", lwd = 2)
legend("topright", legend = c("Poisson (Z=0)", "Gamma (Z=1)"), col = c("blue", "red"), lty = c(1, 1), lwd = 2)








#Esercizio 2
rm(list = ls())

#Impostiamo il seed
set.seed(295706)

#Impostiamo i parametri
mu <- runif(1,-1.5,1.5)
sigma <- runif(1,0.5,1.5)
a <- 0
b <- 1

#Scriviamo la funzione
logit <- function(x) {
  log(x / (1 - x))
}
f_logistic_normal <- function(x, mu, sigma) {
  return((1 / (x * (1 - x) * sqrt(2 * pi * sigma))) * 
             exp(-((logit(x) - mu)^2) / (2 * sigma)))
}

#Punto 1
max_ln <- function(x) {
  -f_logistic_normal(x, mu, sigma)# Usiamo il segno negativo per trovare il massimo con optimize
}
result <- optimize(max_ln, interval = c(0.01, 0.99)) # Evitiamo i bordi 0 e 1
M_p1 <- result$minimum#Abbiamo trovato le cordinate del nostro massimo
M1 <- -result$objective
ndati <- 10
dati <- c()
i <- 1
while(i<=ndati){#ora calcolo 10 campioni come richiesta
  y <- runif(1,a,b)#simulo due variabili uniformi nello spazio [a,b]x[0,M1]
  u <- runif(1,0,M1)
  if(u < f_logistic_normal (y,mu,sigma)){#condizione degli algoritmi accept-reject
    dati[i] <- y
    i <- i+1
  }
}#al termine di questo ciclo avremo ottenuto il nostro campione di 10 dati
nsim <- 10000
y <- runif(nsim, a,b)
u <- runif(nsim, 0,M1)
X = y [u < f_logistic_normal (y,mu,sigma)]
U_X = u[u < f_logistic_normal (y,mu,sigma)]#questa parte serve solo a mostrare che su 10000 simulazioni ne vengono accettate più 1000 come richiesto nelle regole dell'homework
cat("Numero di simulazioni accettate su 10000 simulazioni:",length(X),"\n")
xseq = seq (0 ,1 , by =0.01)
plot (xseq , f_logistic_normal(xseq,mu,sigma) , ylim =c (0 , M1) ,type ="l ", lwd =2)
points (y,u , pch =20 , cex = 0.1)
points (X ,U_X , pch =20 , cex = 0.1 , col =2)
lines ( density (X , from =0 , to = 1) , col =2 , lwd =2)#infine vengono mostrate visivamente le 10000 simulazioni

#Punto 2   #ho modificato il nome alle variabili in modo da essere piu intuitive e nel pdf ho specificato a cosa corrispondono i vari elementi delle immagini
prior_mu <- function(valori_mu) {#creiamo la funzione a priori della media
  dnorm(valori_mu,0,sqrt(100))
}
g <- function(x) {#creiamo la funzione g(x) per campionare con il metodo accept-reject
  dnorm(x,1,sqrt(6))
}
verosomiglianza <- function(valori_mu,campione) {#creiamo la verosomiglianza
  verosomiglianza_valori <- sapply(campione, function(z) {
    log(f_logistic_normal(z, valori_mu, sigma))})   #utilizzo sapply in modo che ad ogni valore del campione venga calcolata la logistic normal, calcolo la log verosomiglianza per problemi relativi ai dati
  return(exp(sum(verosomiglianza_valori)))#essendo che usiamo la log-verosomiglianza sarebbe exp(logA+logB)=exp(logA)*exp(logB)=A*B quindi prodotto tra verosomiglianze
}
posteriori_non_normalizzata <- function(valori_mu,campione){#scriviamo la posteriori non normalizzata (senza il dnominatore del teorema di bayes) che ha come parametro incognito i valori di mu mentre i campioni sono precedentemente calcolati 
  return(prior_mu(valori_mu)*verosomiglianza(valori_mu,campione))
}

integrale_denominatore <- integrate(function(m) posteriori_non_normalizzata(m, dati), lower = 0, upper = 3)$value#calcolo del denominatore

posteriori <- function(valori_mu, campione) {#calcolata la posteriori secondo il teorema di bayes
  posteriori_non_normalizzata(valori_mu, campione) / integrale_denominatore
}
integrale_posteriori <- integrate(function(m) posteriori(m, dati), lower = 0, upper = 3)$value#verifico che la densita a posteriori integri a 1
xseq <- seq(0,3,0.01)
posteriori_valori <- sapply(xseq, function(m) posteriori(m, dati))#calcolo i corrispondenti valori di densità per ogni valore di mu
plot(xseq, posteriori_valori, type = "l", col = "blue", lwd = 2,
     main = "Distribuzione a Posteriori di mu", xlab = expression(mu), ylab = "Densità a posteriori") #disegno graficamente la densita a posteriori

max_post <- function(m) {#utilizzo il kernel come richiesto dall'esercizio e non la posteriori completa
  -posteriori_non_normalizzata(m,dati)# Usiamo il segno negativo per trovare il massimo con optimize
}
result <- optimize(max_post, interval = c(0.01, 2.99)) 
M_p2 <- result$minimum
M2 <- -result$objective#svolgo come nel punto precedente per trovare il massimo della funzione
M<-M2/g(M_p2)*1.15 #trovo M da usare nel metodo accept reject aumentato del 15%
y <- rnorm(nsim,1,sqrt(6))#campiono dalla g
u<- c()
for (i in 1:nsim) {
  u[i]<-runif(1,0,M*g(y[i]))#campiono dall uniforme 0,M*g(y)
}
campione_accettato_della_posteriori <- y [u < sapply(y, function(m) posteriori_non_normalizzata(m, dati))]
U_X <- u[u < sapply(y, function(m) posteriori_non_normalizzata(m, dati))]#seleziono i parametri che rispettano i vincoli dell accept reject
xseq <- seq (0 ,3 ,0.01)
plot (xseq , sapply(xseq, function(m) posteriori_non_normalizzata(m, dati)) , ylim =c (0 , M2) ,type ="l ", lwd =2)
points (y,u , pch =20 , cex = 0.1)
points (campione_accettato_della_posteriori ,U_X , pch =20 , cex = 0.1 , col =2)
lines ( density ( campione_accettato_della_posteriori, from =0 , to = 3) , col =2 , lwd =2)#disegno come il punto precedente
cat("Numero di simulazioni accettate su 10000 simulazioni:",length(campione_accettato_della_posteriori),"\n")#soddisfo i 1000 campioni accettati 

#Punto 3
n <- 100
dati2 <- c()
i <- 1
while(i<=n){#ora calcolo 100 campioni come richiesta
  y <- runif(1,a,b)
  u <- runif(1,0,M1)
  if(u < f_logistic_normal (y,mu,sigma)){#condizione degli algoritmi accept-reject
    dati2[i] <- y
    i <- i+1
  }
}#otteniamo 100 campioni della logistic normal
integrale_denominatore2 <- integrate(function(m) posteriori_non_normalizzata(m, dati2), lower = 0, upper = 2)$value#calcolo del denominatore
posteriori2 <- function(valori_mu, campione) {#calcolata la posteriori secondo il teorema di bayes
  posteriori_non_normalizzata(valori_mu, campione) / integrale_denominatore2
}
integrale_posteriori2 <- integrate(function(m) posteriori2(m, dati2), lower = 0, upper = 2)$value#verifico che la densita a posteriori integri a 1
g2 <- function(x) {#creiamo la funzione g(x) per campionare con il metodo accept-reject
  dnorm(x,1,sqrt(0.5))
}
xseq <- seq(0,2,0.01)
posteriori_valori <- sapply(xseq, function(m) posteriori2(m, dati2))#calcolo i corrispondenti valori di densità per ogni valore di mu
plot(xseq, posteriori_valori, type = "l", col = "blue", lwd = 2,
     main = "Distribuzione a Posteriori di mu (100 campioni)", xlab = expression(mu), ylab = "Densità a posteriori") #disegno graficamente la densita a  posteriori con 100 campioni

max_post2 <- function(m) {
  -posteriori_non_normalizzata(m,dati2)# Usiamo il segno negativo per trovare il massimo con optimize
}
result <- optimize(max_post2, interval = c(0.01, 1.99)) 
M_p3 <- result$minimum
M3 <- -result$objective#massimo
M<-M3/g2(M_p3)*1.15 #trovo M da usare nel metodo accept reject aumentato del 15%
y <- rnorm(nsim,1,sqrt(0.5))#campiono dalla g
u<- c()
for (i in 1:nsim) {
  u[i]<-runif(1,0,M*g2(y[i]))#campiono dall uniforme 0,M*g(y)
}
campione_accettato_della_posteriori2 <- y [u < sapply(y, function(m) posteriori_non_normalizzata(m, dati2))]
U_X <- u[u < sapply(y, function(m) posteriori_non_normalizzata(m, dati2))]#seleziono i parametri che rispettano i vincoli dell accept reject
xseq <- seq (0 ,2 ,0.01)
plot (xseq , sapply(xseq, function(m) posteriori_non_normalizzata(m, dati2)) , ylim =c (0 , M3) ,type ="l ", lwd =2)
points (y,u , pch =20 , cex = 0.1)
points (campione_accettato_della_posteriori2 ,U_X , pch =20 , cex = 0.1 , col =2)
cat("Numero di simulazioni accettate su 10000 simulazioni:",length(campione_accettato_della_posteriori2),"\n")

#densità della media a priori
mu_seq <- seq(-6,6,0.01)
priori_valori <- prior_mu(mu_seq)
plot(mu_seq,priori_valori,type = "l", col = "blue", lwd = 2,main = "Densità prior", xlab = expression(mu), ylab = "Densità")
#densità posteriori con il campione da 10 elementi
xseq <- seq(0,3,0.01)
posteriori_valori <- sapply(xseq, function(m) posteriori(m, dati))#calcolo i corrispondenti valori di densità per ogni valore di mu
plot(xseq, posteriori_valori, type = "l", col = "blue", lwd = 2,
     main = "Distribuzione a Posteriori di mu(10)", xlab = expression(mu), ylab = "Densità a Posteriori") #disegno graficamente la densita a posteriori
#densità posteriori con il campione da 100 elementi
xseq <- seq(0,2,0.01)
posteriori_valori <- sapply(xseq, function(m) posteriori2(m, dati2))#calcolo i corrispondenti valori di densità per ogni valore di mu
plot(xseq, posteriori_valori, type = "l", col = "blue", lwd = 2,
     main = "Distribuzione a Posteriori di mu(100)", xlab = expression(mu), ylab = "Densità a Posteriori") #disegno graficamente la densita a posteriori

#Punto 4
par_a <- 1.2
par_b <- 1.2
num_sim <- 1000
camp <- rbeta(num_sim,par_a,par_b)#campione beta
funzione <- camp * f_logistic_normal(camp,mu,sigma) /dbeta(camp,par_a,par_b)#calcolo per ogni punto il valore x_i*f(x_i)/g(x_i) dove f è la logistic normal e g è la beta
val_atteso <- sum(funzione)/num_sim#trovo il valore atteso
cat("Stima del valore atteso E(X) usando Importance Sampling:", val_atteso, "\n")























