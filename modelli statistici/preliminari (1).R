#   ---------------------------------------------
#       MODELLI STATISTICI - ESERCITAZIONE 1 
#               Marco Ratta, 26/9/24
#   ---------------------------------------------


#############################
# 1 - Allocare le variabili #
#############################

rm(list=ls()) 

a <- 1
type.a <- class(a)
print(paste("La variabile 'a' è di tipo ", type.a))

a.int <- 1L
type.a.int <- class(a.int)
print(paste("La variabile 'a.int' è di tipo ", type.a.int))
  
b <- 0.85
type.b <- class(b)
print(paste("La variabile 'b' è di tipo ", type.b))

c <- -.6
type.c <- class(c)
print(paste("La variabile 'c' è di tipo ", type.c))

char <- 'a'
type.char <- class(char)
print(paste("La variabile 'char' è di tipo ", type.char))

  
str <- "Gasparini"
type.str <- class(str)
print(paste("La variabile 'str' è di tipo ", type.str))

bool <- TRUE
type.bool <- class(bool)
print(paste("La variabile 'bool' è di tipo ", type.bool))

na <- NA
type.na <- class(NA)
print(paste("La variabile 'na' è di tipo ", type.na))

nan <- NaN
type.nan <- class(NA)
print(paste("La variabile 'nan' è di tipo ", type.nan))





######################
# 2 - Strutture dati #
######################
rm(list=ls())

# ------------
# 2.1) VETTORI
# ------------

# Costruzione 
x <- c(5,4,3,2,1)
x.bis <- seq(5,1,-1)
y <- c(2,2,2,2,2)
y.bis <- rep(2,5)

# Estrazione elementi 
x[1]
y[3]
x[3:5]
x[c(3,4,5)]
x[x%%2==0]
x[x%%2==1]

# Operazioni componente a componente
sum <- x+y
prod <- x*y
frac <- x/y
pow <- x^y

# Confronti booleani
x==1
x==y
x < 3
(x+y) > 10
x > 2 & y < 3
x > 2 | y < 3

# Operazioni 
max(x)
min(x)
sum(x)
prod(x)
length(x)
order(x)
x[order(x)]
sort(x)



# ------------
# 2.2) MATRICI
# ------------

# Inizializzazione da zero
mat <- matrix(runif(25,0,1), ncol = 5)
mat.bis <- matrix(seq(2,50,2), byrow = TRUE, ncol = 5)
nrow(mat)
ncol(mat)

# Inizializzazione tramite vettori
v1 <- c(1,2,3)
v2 <- c(4,5,6)
mat.tris <- rbind(v1,v2)
mat.quat <- cbind(v1, v2)
mat.quat[1,2]

# Estrazione elementi
mat[2,2]
mat[2:4, 2:4]
mat[3,]
mat[1,]

# Confronti
mat==mat.bis
mat==t(mat.bis)

# Operazioni
mat + mat.bis
mat - mat.bis
mat * mat.bis
mat / mat.bis
mat %*% mat.bis
sum(mat[1,]*mat.bis[,1])
det(mat)
mat.inv <- solve(mat)
round(mat%*%mat.inv,3)


# ------------
# 2.3) LISTE
# ------------

l1 <- list(x=10, y="casa", z=FALSE, k=c(1,3,5))
l1$x
l1$y
l1$z
l1$k


# -----------------
# 2.4) DATA FRAME
# -----------------

df1 <- data.frame(name=c("Aldo", "Bruno", "Chiara", "Daniela", "Enzo", "Federico"),
                  anni=c(25,47,63,20,78,51),
                  fa_il_fantacalcio=c("TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "FALSE"))

# Estrazione per nome
df1$name

# Estrazione per colonna
df1[[1]]

# Estrazione tipo-matrice
df1[3:5,2]

# Estrazione con condizione
df1[which(df1$anni<50),]
df1[which(df1$fa_il_fantacalcio==TRUE),]




###################
# 3) Probabilità' #
###################

rm(list=ls())

# Campionare da distribuzioni note
s.norm <- rnorm(10000, mean = 0, sd=1)
m.norm <- mean(s.norm)
v.norm <- var(s.norm)

s.exp <- rexp(10000,0.5)
m.exp <- mean(s.exp)
v.exp <- var(s.exp)

par(mfrow=c(1,2))
plot(s.norm, col="red", ylab = "X", pch=16, cex=0.5, main="Normal Sample")
abline(h=m.norm, col="black")
plot(s.exp, col="blue", ylab = "Y", pch=16, cex=0.5, main="Exponential Sample")
abline(h=m.exp, col="black")


# Densità di distribuzioni note
d.norm <- dnorm(0, 0, 1)
d.exp <- dexp(1, 0.5)

# Quantili teorici ed empirici
q.norm <- qnorm(c(0.1,0.9), 0, 1)
q.norm.bis <- quantile(s.norm, c(0.1,0.9))
q.exp <- qexp(c(0.1, 0.5, 0.9), 0.5)
q.exp.bis <- quantile(s.exp, c(0.1, 0.5, 0.9))

# Funzione Ripartizione
p.norm <- pnorm(1.5, 0, 1)
p.exp <- pexp(1, 0.5)

# Plottiamo le densità approssimate dai dati
par(mfrow=c(1,2))
plot(density(s.norm), xlim=c(-4,4), ylim=c(0,0.4), 
     xlab="X", ylab="PDF(X)", main="Normal", col="red")
plot(density(s.exp), xlim=c(0,20), ylim=c(0,0.4),
     xlab="Y", ylab="PDF(Y)", main="Exponential", col="blue")
  



###############
# 4) FUNZIONI #
###############
rm(list=ls())

somma <- function(x,y){
  return(x+y)
}

somma(3,4)


divisione <- function(x,y){
  q <- x%/%y
  r <- x-q*y
  return(list(q=q, r=r))
}

divisione(20,6)






#####################
# 5) IF-ELSE e Loop #
#####################
rm(list=ls())

# if-else statement

rnd <- rbinom(1,1,0.5)
if(rnd==0){
  print(paste(rnd, " <- Hai perso"))
}else{
  print(paste(rnd, " <- Hai vinto"))
}


# for loop

rnd <- rbinom(100,1,0.1)

for(i in 1:length(rnd)){
  if(rnd[i]==0){
    print(paste(rnd[i], " <- Hai perso"))
  }else{
    print(paste(rnd[i], " <- Hai vinto"))
  }
}


# while loop

rnd <- 0
k <- 0

while (rnd==0) {
  rnd <- rbinom(1,1,0.1)
  if(rnd==0){
    print(paste(rnd, " <- Hai perso"))
  }else{
    print(paste(rnd, " <- Hai vinto"))
  }
  k=k+1
}

print(paste("Hai avuto il primo success al tentativo ", k))






###############################
# 6) Intervalli di Confidenza #
###############################

# Creare una funzione che permetta di calcolare l'intervallo di confidenza
# per la media di una popolazione nei seguenti casi:
# 1 - Popolazione normale con varianza nota
# 2 - Popolazione normale con varianza non nota
# 3 - Popolazione Bernoulliana 


rm(list=ls())

conf.int <- function(data, alpha, type, sd=NA, sd.known=TRUE){
  CI <- array(NA,dim=c(1,2))
  if(type=="normal"){
    mean <- mean(data)
    N <- length(data)
    if(sd.known==TRUE){
      z <- qnorm(c(alpha/2,1-alpha/2))
      CI <- mean + (z*sd)/sqrt(N)
    }else{
      sd <- sqrt(var(data))
      t <- qt(c(alpha/2,1-alpha/2), N-1)
      CI <- mean + (t*sd)/sqrt(N)
    }
  }
  if(type=="binomial"){
    p <- mean(data)
    sd <- sqrt(p*(1-p))
    N <- length(data)
    t <- qt(c(alpha/2,1-alpha/2), N-1)
    CI <- p + (t*sd)/sqrt(N)
  }
  return(CI)
}


perc.cover <- c()

# ------------------------------
#   Normale con varianza nota
# ------------------------------

type <- "normal"
cover <- c()

for(k in 1:10000){
  dati <- rnorm(100)
  CI <- conf.int(data=dati, alpha=0.05, type=type, sd=1)
  cover[k] <- CI[1]<0 & CI[2]>0
}

perc.cover[1] <- mean(cover)


# ----------------------------------
#   Normale con varianza non nota
# ----------------------------------

type <- "normal"
cover <- c()

for(k in 1:10000){
  dati <- rnorm(10)
  CI <- conf.int(data=dati, alpha=0.05, type=type, sd=1, sd.known = FALSE)
  cover[k] <- CI[1]<0 & CI[2]>0
}
perc.cover[2] <- mean(cover)


# -------------
#   Binomiale
# -------------

type <- "binomial"
cover <- c()

for(k in 1:10000){
  dati <- rbinom(100,1,0.7)
  CI <- conf.int(data=dati, alpha=0.05, type=type, sd=NA)
  cover[k] <- CI[1]<0.7 & CI[2]>0.7
}

perc.cover[3] <- mean(cover)


res <- data.frame(perc.cover)
rownames(res) = c("Normale Varianza nota", "Normale varianza non nota", "Binomiale")
colnames(res) = "Copertura"
res








#####################
# 7) Test d'ipotesi #
#####################

# Lanciando una moneta 100 volte ottengo 57 teste e 43 croci.
# Si considerino le seguenti ipotesi:
# H0: Moneta non truccata
# H1: Moneta truccata
#
# 1) Posso rifiutare l'ipotesi nulla H0 in favore di H1 al livello 95%?
# 
# Si consideri l'ipotesi alternativa
# H1*: Moneta truccata in favore di testa
#
# 2) Posso rifiutare l'ipotesi nulla H0 in favore di H1* al livello 95%?

rm(list=ls())

# Svolgimento
alpha <- 0.05
p.value <- pbinom(58,100,0.5,lower.tail = FALSE)
rej.1 <- p.value < alpha/2 # 1) Non posso rifiutare H0 in favore di H1
rej.2 <- p.value < alpha # 2) Posso rifiutare H0 in favore di H1*

par(mfrow=c(1,2))

x <- seq(0,100,1)
plot(x, dbinom(x,100,0.5), main = "Test d'ipotesi bilaterale \n H0 vs H1",
     xlab = "x", ylab = "P(X=x)", pch=16, cex=0.7)
X0 <- X1 <- c(seq(0,42,1),seq(58,100,1))
segments(X0, rep(0, length(X0)), X1, dbinom(X0,100,0.5), col="red")

x <- seq(0,100,1)
plot(x, dbinom(x,100,0.5), main = "Test d'ipotesi unilaterale  \n H0 vs H1*",
     xlab = "x", ylab = "P(X=x)", pch=16, cex=0.7)
X0 <- X1 <- seq(58,100,1)
segments(X0, rep(0, length(X0)), X1, dbinom(X0,100,0.5), col="red")


