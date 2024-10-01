#### #### #### #### #### #### #### ####
####        Gianluca Mastrantonio
####        METODI MONTE CARLO - Vers. 1.0.0
#### #### #### #### #### #### #### ####
#options(repr.plot.width=10, repr.plot.height=10)
DIR = "/Users/gianlucamastrantonio/Dropbox (Politecnico di Torino Staff)/Didattica/statistica computazionale/codice R/Monte Carlo/Monte Carlo.R/"




library(datasets)
library(catdata)
library(dslabs)
library(mvtnorm)
library(patchwork)
library(paletteer)
library(tidyverse)

#### #### #### ####
#### Comandi sulle distribuzioni
#### #### #### ####

# ci sono dei comandi standard su R per ottenere il calcolo
# della densità/probabilità (d), comulata (p), quantile (q)  e ottenere dei
# valori da una distribuzione prefissata (r)
# per esempio, per una normale possiamo usare
# dnorm(), pnorm(), qnorm(), rnorm()
?dnorm
# per l'esponenziale dexp(), pexp(), qexp(), rexp()
?dexp
# per la poisson dpois(), ppois(), qpois(), rpois()
?dpois


# I comandi qui sotto e fino alla fine di questo chunk del codice servono solo a far vedere cosa
# calcolano queste funzioni nel caso di una normale standard.
# Lanciateli e vedete la figura che producono

z_scores <- seq(-4, 4, by = 0.01)
mu <- 0
sd <- 1


#Functions
##Using `dnorm` and `pnorm` to setup the "skeleton" of related plots.
normal_dists <- list(`dnorm()` = ~ dnorm(., mu, sd),
`rnorm()` = ~ dnorm(., mu, sd),
`pnorm()` = ~ pnorm(., mu, sd),
`qnorm()` = ~ pnorm(., mu, sd))

##Apply functions to data and parameter combinations
df <- tibble(z_scores, mu, sd) %>%
mutate_at(.vars = vars(z_scores), .funs = normal_dists) %>%
#"Lengthen" the data
pivot_longer(cols = -c(z_scores, mu, sd), names_to = "func",
  values_to = "prob") %>%
#Categorize based on shape of distribution -- need to split up the dataframe
# for plotting later.
mutate(distribution = ifelse(func == "pnorm()" | func == "qnorm()",
  "Cumulative probability", "Probability density"))

##Split up the data into different pieces that can then be added to a plot.
###Probabilitiy density distrubitions
df_pdf <- df %>%
filter(distribution == "Probability density") %>%
rename(`Probabilitiy density` = prob)

###Cumulative density distributions
df_cdf <- df %>%
filter(distribution == "Cumulative probability") %>%
rename(`Cumulative probability` = prob)

###dnorm segments
#Need to make lines that represent examples of how values are mapped -- there
# is probably a better way to do this, but quick and dirty is fine for now.
df_dnorm <- tibble(z_start.line_1 = c(-1.5, -0.75, 0.5),
pd_start.line_1 = 0) %>%
mutate(z_end.line_1 = z_start.line_1,
  pd_end.line_1 = dnorm(z_end.line_1, mu, sd),
  z_start.line_2 = z_end.line_1,
  pd_start.line_2 = pd_end.line_1,
  z_end.line_2 = min(z_scores),
  pd_end.line_2 = pd_start.line_2,
  id = 1:n()) %>%
pivot_longer(-id) %>%
separate(name, into = c("source", "line"), sep = "\\.") %>%
pivot_wider(id_cols = c(id, line), names_from = source) %>%
mutate(func = "dnorm()",
  size = ifelse(line == "line_1", 0, 0.03))

###rnorm segments
#Make it reproducible
set.seed(20200209)
df_rnorm <- tibble(z_start = rnorm(10, mu, sd)) %>%
mutate(pd_start = dnorm(z_start, mu, sd),
  z_end = z_start,
  pd_end = 0,
  func = "rnorm()")

###pnorm segments
df_pnorm <- tibble(z_start.line_1 = c(-1.5, -0.75, 0.5),
pd_start.line_1 = 0) %>%
mutate(z_end.line_1 = z_start.line_1,
  pd_end.line_1 = pnorm(z_end.line_1, mu, sd),
  z_start.line_2 = z_end.line_1,
  pd_start.line_2 = pd_end.line_1,
  z_end.line_2 = min(z_scores),
  pd_end.line_2 = pd_start.line_2,
  id = 1:n()) %>%
pivot_longer(-id) %>%
separate(name, into = c("source", "line"), sep = "\\.") %>%
pivot_wider(id_cols = c(id, line), names_from = source) %>%
mutate(func = "pnorm()",
  size = ifelse(line == "line_1", 0, 0.03))

###qnorm segments
df_qnorm <- tibble(z_start.line_1 = min(z_scores),
pd_start.line_1 = c(0.1, 0.45, 0.85)) %>%
mutate(z_end.line_1 = qnorm(pd_start.line_1),
  pd_end.line_1 = pd_start.line_1,
  z_start.line_2 = z_end.line_1,
  pd_start.line_2 = pd_end.line_1,
  z_end.line_2 = z_end.line_1,
  pd_end.line_2 = 0,
  id = 1:n()) %>%
pivot_longer(-id) %>%
separate(name, into = c("source", "line"), sep = "\\.") %>%
pivot_wider(id_cols = c(id, line), names_from = source) %>%
mutate(func = "qnorm()",
  size = ifelse(line == "line_1", 0, 0.03))

  cp <- paletteer_d("ggsci::default_locuszoom", 4, )
  names(cp) <- c("dnorm()", "rnorm()", "pnorm()", "qnorm()")

  ##Probabilitiy density
  p_pdf <- df_pdf %>%
    ggplot(aes(z_scores, `Probabilitiy density`)) +
    geom_segment(data = df_dnorm,
      aes(z_start, pd_start, xend = z_end, yend = pd_end),
      arrow = arrow(length = unit(df_dnorm$size, "npc"), type = "closed"),
      size = 0.8, color = cp["dnorm()"]) +
    geom_segment(data = df_rnorm,
      aes(z_start, pd_start, xend = z_end, yend = pd_end),
      arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
      size = 0.8, color = cp["rnorm()"]) +
    geom_line(size = 0.6) +
    facet_wrap(~ func, nrow = 1) +
    theme_bw() +
    theme(panel.grid = element_blank(),
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      text = element_text(family = "serif", size = 14)) +
    scale_y_continuous(expand = expand_scale(c(0, 0.05))) +
    scale_x_continuous(expand = c(0.01, 0))

  ##Cumulative probability
  p_cdf <- df_cdf %>%
    ggplot(aes(z_scores, `Cumulative probability`)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_segment(data = df_pnorm,
      aes(z_start, pd_start, xend = z_end, yend = pd_end),
      arrow = arrow(length = unit(df_dnorm$size, "npc"), type = "closed"),
      size = 0.8, color = cp["pnorm()"]) +
    geom_segment(data = df_qnorm,
      aes(z_start, pd_start, xend = z_end, yend = pd_end),
      arrow = arrow(length = unit(df_qnorm$size, "npc"), type = "closed"),
      size = 0.8, color = cp["qnorm()"]) +
    geom_line(size = 0.6) +
    facet_wrap(~ func, nrow = 1) +
    labs(x = "z-score/quantiles") +
    theme_bw() +
    theme(panel.grid = element_blank(),
      strip.background = element_blank(),
      text = element_text(family = "serif", size = 14)) +
    scale_x_continuous(expand = c(0.01, 0))

  ##Combine the plots
  p_pdf + p_cdf + plot_layout(ncol = 1)

  
## ## ## ## ##
##  Vediamo come funziona il seed
## ## ## ## ##
set.seed(100)
runif(10,0,1)

set.seed(100)
runif(10,0,1)

runif(10,0,1)


## ## ## ## ##
##  Implementiamo un algoritmo per campionare da un'uniforme
## ## ## ## ##

r_unif01 = function(n, seed, m=100, c=2, a=4)
{
  if( (c<0) | (c>m))
  {
    stop("(c<0) | (c>m)")
  }
  if( (a<=0) | (a>=m))
  {
    stop("(a<=0) | (a>=m)")
  }
  ret = rep(NA, n)
  prev_val = seed
  for(i in 1:n)
  {
    ret[i] = (a*prev_val + c) %% m
    prev_val = ret[i]
  }
  return(ret/m)
}

x = r_unif01(n=100000, seed=0, m=1000, c=3, a=5)
par(mfrow=c(1,2))
plot(x)
hist(x)
table(x)

x = r_unif01(n=100000, seed=0, m=1000000, c=3.3, a=5.3)
par(mfrow=c(1,2))
plot(x)
hist(x)
table(table(x))


x = r_unif01(n=100000, seed=0, m=1000000, c=3.7, a=30.3)
par(mfrow=c(1,2))
plot(x)
hist(x)
table(table(x))



## ## ## ## ##
##  Funzione per simulare da una
##  normale multivariata partendo
##  da normali standard
## ## ## ## ##


rmnorm=function(n = 1, mean = rep(0, d), varcov)
{
   d <- if (is.matrix(varcov))
       ncol(varcov)
   else 1
   z <- matrix(rnorm(n * d), n, d) %*% chol(varcov)
   y <- t(mean + t(z))
   return(y)
}


## ## ## ## ##
##  Funzione per simulare da una
##  normale multivariata partendo
##  da uniformi
## ## ## ## ##

n = 10000
mu = matrix(c(1,2), ncol=1)
A  = matrix(runif(4,0,1), ncol=2)
Sigma = A%*%t(A)
u1  = runif(n)
u2  = runif(n)

x = sqrt(-2*log(u1))*cos(2*pi*u2)
y = sqrt(-2*log(u1))*sin(2*pi*u2)

norm_var1 = t(A%*%rbind(x,y) + matrix(mu, ncol=n, nrow=2))
norm_var2 = rmnorm(n,c(mu), Sigma)

#norm_var1 = rbind(x,y)
par(mfrow=c(1,2))
smoothScatter(norm_var1)
smoothScatter(norm_var2)

## ## ## ## ## 
## Inverse Generalizzata
## ## ## ## ## 

x = seq(0,20, by=0.1)
par(mfrow=c(1,1))
plot(x, ppois(x,2), type="s")


x = seq(0,20, by=0.1)
par(mfrow=c(1,1))
plot(x, pgamma(x,1,1), type="s")


## ## ## ## ## 
## Marginali e congiunte
## ## ## ## ## 

# creaiamo una matrice di probabilià per due variabili
# discrete in 1, K

K = 5
prob_mat = matrix(runif(K^2,0,1), ncol=K)
prob_mat = round(prob_mat/sum(prob_mat),3)
prob_mat

# campioniamo
n = 10000
# prima x1, poi x2
x1 = sample( 1:K,n, prob= rowSums(prob_mat), replace=T)
x2 = rep(NA, n)
for(i in 1:n)
{
  x2[i] =  sample( 1:K,1, prob= prob_mat[x1[i],]/sum(prob_mat[x1[i],]), replace=T)
}
matrix_conteggi1 = matrix(0, ncol=K, nrow=K)
for(i in 1:n)
{
  matrix_conteggi1[x1[i],x2[i]] = matrix_conteggi1[x1[i],x2[i]]+1  
}

# prima x2, poi x1
x2 = sample( 1:K,n, prob= colSums(prob_mat), replace=T)
x1 = rep(NA, n)
for(i in 1:n)
{
  x1[i] =  sample( 1:K,1, prob= prob_mat[,x2[i]]/sum(prob_mat[,x2[i]]), replace=T)
}
matrix_conteggi2 = matrix(0, ncol=K, nrow=K)
for(i in 1:n)
{
  matrix_conteggi2[x1[i],x2[i]] = matrix_conteggi2[x1[i],x2[i]]+1  
}

matrix_conteggi1/n
matrix_conteggi2/n
prob_mat

## ## ## ## ##
##  Teorema Accept-Reject
## ## ## ## ##
library(plotly)
# assumiamo x da  G(9,2)
n = 100
x = seq(0,10, length.out=n)
fx = dgamma(x,shape=9,rate=2)
m = max(fx)
u = seq(0,m, length.out=n)

dens = matrix(0, nrow=n, ncol=n)
for(i in 1:n)
{
  for(j in 1:n)
  {
    if( u[j]< fx[i])
    {
      dens[i,j] = 1
    }
  }
  
}

pp = plot_ly(x = x, y = u,z = t(dens), type = "surface")
layout(pp,
    scene = list(
      xaxis = list(title = 'x'),
      yaxis = list(title = 'u'),
      zaxis = list(title = 'dens')
    )
)
## ## ## ## ##
##  stime di densità
## ## ## ## ##

# simulazione
n = 10
x = rgamma(n,2,2)

# scegliamo i parametri di un kernel gaussiano
# con diversi parametri
stand_dev_1 = 0.0001
stand_dev_2 = 0.05
stand_dev_3 = 0.5

# calcoliamo la stima di densità in xseq
xseq = seq(0,10, by=0.1)

stima_dens_1 = c()
stima_dens_2 = c()
stima_dens_3 = c()
for(i in 1:length(xseq))
{
  stima_dens_1[i] = sum( dnorm(x,xseq[i],stand_dev_1)  )/n
  stima_dens_2[i] = sum( dnorm(x,xseq[i],stand_dev_2)  )/n
  stima_dens_3[i] = sum( dnorm(x,xseq[i],stand_dev_3)  )/n
}

par(mfrow=c(1,1))
plot(xseq, dgamma(xseq,2,2), type="l", lwd=2)
lines(xseq,stima_dens_1, col=2, lwd=2)
lines(xseq,stima_dens_2, col=3, lwd=2)
lines(xseq,stima_dens_3, col=4, lwd=2)
legend("topright", c("sd 0.0001","sd 0.05","sd 0.5"), col=2:4, lwd=2, lty=1 , cex=4)
par(mfrow=c(1,1))


###
n = 20
x = rgamma(n,2,2)

# calcoliamo al densità nel punto xseq[i]
library(graphics)
i = 100
plot(xseq, dnorm(xseq,xseq[i],stand_dev_3)/n, type="l") # kernel
points(x,  dnorm(x,xseq[i],stand_dev_3)/n) #
points(x, rep(-0,n), pch=20) # osservazioni
segments(x, rep(-0,n), x, dnorm(x,xseq[i],stand_dev_3)/n)

# la somma dei segmenti è la stima
sum(dnorm(x,xseq[i],stand_dev_3)/n)


## ## ## ## ##
##  Simulazione da uniforme
##  con inversa della cumulata
## ## ## ## ##
n = 20000
x = runif(n, 0,1)

y_norm1 = qnorm(x)
y_norm2 = qnorm(x, 0.5)
y_gamma = qgamma(x, 1,1)
y_exp   = qexp(x, 5)
y_pois  = qpois(x, 2)

#
yseq1 = seq(-5,5,by=0.1)
yseq2 = seq(0,5,by=0.1)
yseq3 = 0:10

# N(0,1)
hist(y_norm1, freq=F)
lines(yseq1, dnorm(yseq1), lwd=3)
lines(density(y_norm1), col=2, lwd=3)

# N(0.5,1)
hist(y_norm2, freq=F)
lines(yseq1, dnorm(yseq1,0.5), lwd=3)
lines(density(y_norm2), col=2, lwd=3)

# G(1,1)
hist(y_gamma, freq=F)
lines(yseq2, dgamma(yseq2,1,1), lwd=3)
lines(density(y_gamma, from=0), col=2, lwd=3)

# Exp(5)
hist(y_exp, freq=F)
lines(yseq2, dexp(yseq2,5), lwd=3)
lines(density(y_exp, from=0), col=2, lwd=3)

# P(3)
TT = table(y_pois)/n
plot(as.numeric(names(TT)),TT, col=2, lwd=2, cex=2)
points(yseq3, dpois(yseq3,2), lwd=3, pch=20)



## ## ## ## ## ## ##
## Acc/Rej da una beta - Figure del pdf
## ## ## ## ## ## ##

# parametri
para = 3 # i parametri devono essere maggiori di 1
parb = 1.2
# calcoliamo il massimo
moda  =  (para-1)/(para+parb-2)
m     = dbeta(moda,para,parb)

## campioni U e Y
n = 10000
Y = runif(n,0,1)
U = runif(n,0,m)
X = Y[U < dbeta(Y,para,parb)]

U_X = U[U < dbeta(Y,para,parb)]
# plot densità
xseq = seq(0,1,by=0.01)
#pdf(paste(DIR, "BetaSim.pdf",sep=""))
plot(xseq, dbeta(xseq,para,parb), ylim=c(0,m), type="l", lwd=2 )
points(Y,U, pch=20, cex = 0.1)
points(X,U_X, pch=20, cex = 0.1, col=2)
lines(density(X, from=0, to = 1), col=2, lwd=2)

acc     = length(X)/n
acc_est = 1/((1-0)*m)
acc
acc_est

#### #### #### #### ####
#### Kernel
#### #### #### #### ####

## si vede come tutte queste figure hanno la stessa forma a campana

xseq = seq(-3,3, by = 0.1)
par(mfrow=c(2,2))
plot(xseq, 2*exp(-0.5*xseq^2), type="l")
plot(xseq, 10*exp(-0.5*xseq^2), type="l")
plot(xseq, 0.1*exp(-0.5*xseq^2), type="l")
plot(xseq, dnorm(xseq, 0,1), type="l")
par(mfrow=c(1,1))

#### #### #### #### ####
#### Simulazione da kernel  - Figure del pdf
#### #### #### #### ####

#pdf(paste(DIR, "Sin.pdf",sep=""))
xseq = seq(-pi, pi, by=0.01)
dens = exp( -xseq^2/2 )*(sin(6*xseq)^2+3*cos(xseq)^2*sin(4*xseq)^2+1)
#dens = exp( -xseq^2/2 )*(sin((6*xseq)^2)+3*cos((xseq)^2)*sin((4*xseq)^2)+1)
plot(xseq, dens, type="l", ylim=c(0,5), lwd=3, xlab="x", ylab="density")
lines(xseq, 12*dnorm(xseq), col=2, lwd=3)
#dev.off()
table((12*dnorm(xseq))>=dens)/length(xseq)

## campioniamo
dens_f = function(x)
{
   exp( -x^2/2 )*(sin(6*x)^2+3*cos(x)^2*sin(4*x)^2+1)
}


n = 10000
m = 12
Y = rnorm(n, 0,1)
U = runif(n,0,1)
X = Y[U <= dens_f(Y)/(m*dnorm(Y))]
U_X = U[U <= dens_f(Y)/(m*dnorm(Y))]

plot(xseq, dens, type="l", ylim=c(0,5), lwd=3, xlab="x", ylab="density")
lines(xseq, 12*dnorm(xseq), col=2, lwd=3)
points(Y,U*m*dnorm(Y), pch=20, cex = 0.1)
points(X,U_X*m*dnorm(X), pch=20, cex = 0.1, col=2)


### vediamo la densità "proper"

# calcoliamo la costante di normalizzazione
C = integrate(dens_f, lower =-Inf,upper=Inf)$value
plot(xseq, dens/C, type="l",  lwd=3, xlab="x", ylab="density")

# verifichiamo che integri a 1

X1 = runif(10000,min(xseq),max(xseq))
X2 = runif(10000,0,0.8)

plot(xseq, dens/C, type="l",  lwd=3, xlab="x", ylab="density", ylim=c(0,0.8))
points(X1,X2)

### l'aria totale è
Area = (max(xseq)-min(xseq))*0.8
#quindi ci aspettiamo una proporzione di  
1/Area
# punti interni alla densità (visto che deve integrare a 1)

ninside = length(X1[X2<dens_f(X1)/C])

ninside/length(X1) - 1/Area

# Possiamo anche calcolare la costante di normalizzazione usando
# dei campioni

(length(X)/length(Y))* m
C

## ## ## ## ## ## ## ## ##
# Monte Carlo - Legge dei grandi numeri 
## ## ## ## ## ## ## ## ##

# Assumiamo X  ~ G(1,1)
# cha ha E(X) = 1
# "dimsotriamo" che la legge dei grandi numero è vera
n = 10000
x = rgamma(n, 1,1)
h = cumsum(x)/c(1:n)

plot(h, type="l")
abline(h=1, col=2, lwd=2)

# verifichiamo la sua varianza
stima_varx = rep(NA, n-1)
for(i in 2:n)
{
  stima_varx[i-1] = (sum((x[1:i]-h[i])^2)/(i-1))
}
varh = stima_varx/(1:(n-1))
plot(varh)
plot(varh[-c(1:100)])
which(varh<0.02)[1]

## vediamo graficamente la "variabilità"

n = 10000
nsim = 1000
hmat = matrix(NA, ncol= n, nrow=nsim)
for(i in 1:nsim)
{
  x = rgamma(n, 1,1)
  hmat[i,] = cumsum(x)/c(1:n)  
}
plot(hmat[1,], type="n")
for(i in 1:nsim)
{
  lines(hmat[i,], lwd=0.1)
}
abline(h=1, col=2, lwd=2)
#vediamo la varianza
var_stima = apply(hmat,2,var)

plot(varh, type="l", lwd=3)
lines(var_stima[-1], col=2, lwd=3)


## ## ## ## ## ## ## ## ##
# vediamo la funzione di ripartizione empirica di una beta(1,3)
# con diversi valori di n
## ## ## ## ## ## ## ## ##

# simuliamo dalla beta
x1 = rbeta(10,1,3)
x2 = rbeta(50,1,3)
x3 = rbeta(100,1,3)


# calcoliamo le funzioni di ripartizioni empiriche su xseq
xseq= seq(0,1, by=0.001)
ec1 = ecdf(x1)(xseq)
ec2 = ecdf(x2)(xseq)
ec3 = ecdf(x3)(xseq)

#pdf(paste(DIR,"ecdf.pdf",sep=""))
plot(xseq,ec1, type="s", lwd=2, col=2, xlab="X", ylab="ecdf")
lines(xseq,ec2, type="s", col=3, lwd=2)
lines(xseq,ec3, type="s", col=4, lwd=2)
lines(xseq, pbeta(xseq,1,3), type="s", col=1, lwd=2)
legend("bottomright", c("True","n=10","n=50","n=100"), col=1:4, lwd=3, cex=2)
#dev.off()

#### #### #### #### #### #### #### ####
#### Calcoliamo la cumulata
#### F(a;lambda) = int_{-infty}^a f(x;lambda)dx con
#### x ~ Exp(lambda)
#### e il quantile p
#### #### #### #### #### #### #### ####

n      = 100 # numero osservazioni
lambda = 1   # parametro esponenziale
a      = 2   # limite destro dell'integrale
p      = 0.5

x = rexp(n,lambda)

#### Cumulata
Fa_stima = sum(x<=a)/n
Fa_vera  = pexp(a,lambda)

# la qualità della stima dipenderà da n (provare n = 100 e n = 10000)
Fa_stima
Fa_vera

#### Quantile p

x_ord = sort(x)      # ordino in maniera crescente
rank_stima = (1:n) # la prima osservazione di x_ord

quantile_stima = x_ord[min(which((rank_stima/n) >= p))]
quantile_vero  = qexp(p,lambda)

# la qualità della stima dipenderà da n (provare n=100 e n=10000)
quantile_stima
quantile_vero


#### #### #### #### #### #### #### ####
#### Calcoliamo
#### F(b;lambda)- F(b;lambda) = int_{a}^b f(x;lambda)dx con
#### x ~ Exp(lambda)
#### simulando da uniformi
#### #### #### #### #### #### #### ####

n = 10
a = 0
b = 0.1

x_unif = runif(n,a,b) # campione da uniforme

int_stima = sum(dexp(x_unif,lambda)*(b-a))/n # stimatore

int_vero  = pexp(b,lambda)-pexp(a,lambda) # valore vero

int_stima
int_vero


#### #### #### #### #### #### #### ####
#### Calcoliamo
#### f_x(a;lambda) = int f_{x|y}(a;lambda,y)f_y(y)dy con
#### y ~ Exp(lambda)
#### e x|y \sim G(exp(y),1)   - Figure del pdf
#### #### #### #### #### #### #### ####

n = 1000
a = seq(0,10,by=0.1) # valori di x su cui calcolare la densità
lambda = 2


fx_stima = c()
y = rexp(n,lambda)
for(i in 1:length(a))
{
  fzgiveny = dgamma(a[i],exp(y),1) # densità condizionata
  fx_stima[i] = sum(fzgiveny)/n    # stima di fx nel punto a[i]
}
#pdf(paste(DIR, "MC.pdf",sep=""))
plot(a,fx_stima)
lines(a,fx_stima, col=2)
#dev.off()

#### #### #### #### #### #### #### ####
#### Mostriamo vari modi
#### per ottenere campioni
#### Otteniamo campioni da una normale bivariata, usando la marginale e condizionata
#### (per trovare la distribuzione condizionata di una normale, potete guardare su
#### https://en.wikipedia.org/wiki/Multivariate_normal_distribution)
#### #### #### #### #### #### #### ####

#### Abbiamo bisogno delle funzioni
rmnorm=function(n = 1, mean = rep(0, d), varcov)
{
  d <- if (is.matrix(varcov))
    ncol(varcov)
  else 1
  z <- matrix(rnorm(n * d), n, d) %*% chol(varcov)
  y <- t(mean + t(z))
  return(y)
}
dmnorm=function (x, mean = rep(0, d), varcov, log = FALSE)
{
  d <- if (is.matrix(varcov))
    ncol(varcov)
  else 1
  if (d > 1 & is.vector(x))
    x <- matrix(x, 1, d)
  n <- if (d == 1)
    length(x)
  else nrow(x)
  X <- t(matrix(x, nrow = n, ncol = d)) - mean
  Q <- apply((solve(varcov) %*% X) * X, 2, sum)
  logDet <- sum(logb(abs(diag(qr(varcov)[[1]]))))
  logPDF <- as.vector(Q + d * logb(2 * pi) + logDet)/(-2)
  if (log)
    logPDF
  else exp(logPDF)
}
## ## ## ## ## ##
## Metodo Monte Carlo - Normale Bivariata
## ## ## ## ## ##

# Calcoliamo la media assumendo di saper campionare dalla normale
nb = 10000
mu = c(10,20)
Sigma = matrix(c(1,0.5,0.5,1), ncol=2)
z = rmnorm(nb,mu, Sigma)

# facciamone il plot per vedere la congiunta (distribuzione dei dati)
smoothScatter(z,asp=1)

# usiamo la marginale della prima componente
# e la condizionata della seconda
z2 = rnorm(nb, mu[2], Sigma[2,2]^0.5)
z1 = rnorm(nb, mu[1]+Sigma[1,2]*(z2-mu[2])/Sigma[2,2], (Sigma[1,1]-Sigma[1,2]^2/Sigma[2,2])^0.5)
zstar = cbind(z1,z2)

# vediamo che i due metodi producono cose molto simili
par(mfrow=c(1,2))
smoothScatter(z,asp=1)
smoothScatter(zstar,asp=1)
par(mfrow=c(1,1))

# vediamo che se prendo una sola colonna di z, questa è distribuita come la marginale
par(mfrow=c(1,2))
# z1
plot(density(z[,1]))
lines(seq(6,15,by=0.1), dnorm(seq(6,15,by=0.1), mu[1], Sigma[1,1]), col=2)
# z2
plot(density(z[,2]))
lines(seq(16,25,by=0.1), dnorm(seq(16,25,by=0.1), mu[2], Sigma[2,2]), col=2)
par(mfrow=c(1,1))

## ## ## ## ## ##
## Rao-Blackwel
## ## ## ## ## ##

## ipotizziamo di voler calcolare la media di una normale
## N(0,1), assumendo di non sapere come calcolarla numericamente

## simuliamo dalla normali
n = 100
x_marg = rnorm(n, 0,1)

# lo stimatore
mean(x_marg)
# e la sua varianza
sum( (x_marg-mean(x_marg))^2)/n^2

# facciamo la stessa cosa usando le normali multivariate
# e condizionando (Rao-Blackwellizando)

# settiamo i parametri della congiunta
mu = c(0,2)
Sigma = matrix(c(1,0.8,0.8,1), ncol=2)
# siamo interessati alla prima componente ( N(0,1) )

# funzione della media cond
cond_mean = function(y)
{
  mu[1]+Sigma[1,2]*(y-mu[2])/Sigma[2,2]
}
## simulo dalla marginale della seconda componente
y = rnorm(n, mu[2], Sigma[2,2]^0.5)

# calcolo lo stimatore
mean(cond_mean(y))
# e la sua varianza
sum(  (cond_mean(y)-mean(cond_mean(y)))^2 )/n^2

## confronto le varianze
sum( (x_marg-mean(x_marg))^2)/n^2
sum(  (cond_mean(y)-mean(cond_mean(y)))^2 )/n^2
sum( (x_marg-mean(x_marg))^2)/n^2>sum(  (cond_mean(y)-mean(cond_mean(y)))^2 )/n^2

## le varianze stesse sono variabili aleatorie, e quindi dovremmo valutarne la distribuzione
# simuliamo diversi dataset e vediamo cosa succede

var1 = c()
var2 = c()
for(i in 1:10000)
{
  ## simuliamo dalla normali
  n = 100
  x_marg = rnorm(n, 0,1)

  # lo stimatore
  mean(x_marg)
  # e la sua varianza
  sum( (x_marg-mean(x_marg))^2)/n^2

  # facciamo la stessa cosa usando le normali multivariate
  # e condizionando (Rao-Blackwellizando)

  # settiamo i parametri della congiunta
  mu = c(0,2)
  Sigma = matrix(c(1,0.8,0.8,1), ncol=2)
  # siamo interessati alla prima componente ( N(0,1) )

  # funzione della media cond
  cond_mean = function(y)
  {
    mu[1]+Sigma[1,2]*(y-mu[2])/Sigma[2,2]
  }
  ## simulo dalla marginale della seconda componente
  y = rnorm(n, mu[2], Sigma[2,2]^0.5)

  # calcolo lo stimatore
  mean(cond_mean(y))
  # e la sua varianza
  sum(  (cond_mean(y)-mean(cond_mean(y)))^2 )/n^2

  ## confronto le varianze
  var1[i] =  sum( (x_marg-mean(x_marg))^2)/n^2
  var2[i] =  sum(  (cond_mean(y)-mean(cond_mean(y)))^2 )/n^2
}


plot(density(var2))
lines(density(var1), col=2)

#### #### #### #### #### #### ####
#### Importance sampling
#### #### #### #### #### #### ####

# proviamo a fare la stessa cosa di sopra, utilizzando l'importance sampling
# e come funzione g utilizziamo 3 t di student
n = 10000
xseq = seq(-10,10,by=0.1)

plot(xseq, dnorm(xseq, 0,1), col=1, lwd=2, type="l")
lines(xseq, dt(xseq,1), col=2, lwd=2)
lines(xseq, dt(xseq,5), col=3, lwd=2)
lines(xseq, dt(xseq,20), col=4, lwd=2)
legend("topright", c("N(0,1)", "T(1)", "T(5)","T(20)"), col=1:4, lwd=rep(2,4), lty=rep(1,4))

#set.seed(1)
x_t1  = rt(n,1)
x_t5  = rt(n,5)
x_t20 = rt(n,20)

# medie
m1 = x_t1*dnorm(x_t1)/dt(x_t1,1) 
m2 = x_t5*dnorm(x_t5)/dt(x_t5,5) 
m3 = x_t20*dnorm(x_t20)/dt(x_t20,20) 

## 
mean(m1)
mean(m2)
mean(m3)

#varianze
var(x_t1*dnorm(x_t1)/dt(x_t1,1))
var(x_t5*dnorm(x_t5)/dt(x_t5,5))
var(x_t20*dnorm(x_t20)/dt(x_t20,20))



#### facciamo un esempio con la B(10,10)
n = 10000
xseq = seq(0,1,by=0.01)

plot(xseq,dbeta(xseq, 10,10), col=1, lwd=2, type="l")
lines(xseq, dbeta(xseq, 1,1), col=2, lwd=2)
lines(xseq, dbeta(xseq, 3,3), col=3, lwd=2)
lines(xseq, dbeta(xseq, 8,8), col=4, lwd=2)
legend("topright", c("B(10,10)", "B(1,1)", "B(3,3)","B(8,8)"), col=1:4, lwd=rep(2,4), lty=rep(1,4))


x_b1  = rbeta(n,1,1)
x_b3  = rbeta(n,3,3)
x_b8 = rbeta(n,8,8)

# medie
m1 = x_b1*dbeta(x_b1,10,10)/dbeta(x_b1,1,1) 
m2 = x_b3*dbeta(x_b3,10,10)/dbeta(x_b3,3,3) 
m3 = x_b8*dbeta(x_b8,10,10)/dbeta(x_b8,8,8) 
mean(m1)
mean(m2)
mean(m3)

#varianze
var(m1)
var(m2)
var(m3)

#### #### #### #### 
#### Vediamo un caso in cui IS funziona molto bene
#### #### #### #### 

# voglaimo simulare da una half t(3) (t distribution definita solo sui positivi)
# usigna come g uno half normal(0,1), e half cauchy(0,1) (una half t(1))

xseq = seq(0, 5, by = 0.01)

plot(xseq,dt(xseq, 3), col=1, lwd=2, type="l",ylim= c(0,0.43))
lines(xseq, dnorm(xseq), col=2, lwd=2)
lines(xseq, dt(xseq,1), col=3, lwd=2)
legend("topright", c("HT(3)", "HN(0,1)", "HC(0,1)"), col=1:4, lwd=rep(2,4), lty=rep(1,4))


nsim = 10000
n = 100
results = data.frame(list(normal=rep(0,nsim), Cauchy=rep(0,nsim), t3=rep(0,nsim)))

for (i in 1:nsim) 
{
   x_norm = rnorm(n)
   x_cauchy = rt(n, 1)
   results[i,1] = mean(abs(x_norm)*dt(x_norm, df=3)/dnorm(x_norm))
   results[i,2] = mean(abs(x_cauchy)*dt(x_cauchy, df=3)/dt(x_cauchy, df=1))
   results[i,3] = mean(abs(rt(100, df=3)))
}
apply(results,2,var)

