### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### 07-12-2021 Glm Poisson                              ### ###
### Si vuole studiare come il numero delle rotture di   ### ###
### fili di lana dipendano dal tipo di                  ### ###
### lana e dalla tensione                               ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## Voglio modellare il numero di rotture di un filo dipendentemente dal tipo di lana e dalla tensione, entrambe le variabili
## sono categoriche, pertanto il log dei parametri stimati mi dirà di quanto mediamente il passaggio da un gruppo all'altro
## tenendo invariate le altre condizioni mi influenza la risposta.

## 
rm(list =ls())
## 

## librerie
library(datasets)
library(contrast)
library(MASS)


# carichiamo i dati
Data = warpbreaks
summary(Data)

# I dati si possono rappresentare come tabelle a doppia entrata
table(Data[,-1])



### ### ### ### ### ### ### ###
### Statistiche descrittive ###
### ### ### ### ### ### ### ###

tapply(Data$breaks,Data$tension,mean) # media delle rotture divise per tensione
tapply(Data$breaks,Data$wool,mean)    # media delle rotture divise per lana 

tapply(Data$breaks,Data$tension,var)  # varianza delle rotture divise per tensione
tapply(Data$breaks,Data$wool,var)     # varianza delle rotture divise per lana

tapply(Data$breaks,paste(Data$tension,Data$wool),mean) # media delle rotture divise sia per lana che per tensione
tapply(Data$breaks,paste(Data$tension,Data$wool),var)  # varianza delle rotture divise sia per lana che per tensione




## ## ## ## ## ## ## ## ##
## Modello di poisson   ##
## ## ## ## ## ## ## ## ##                         


Mod1 = glm(breaks ~ tension+wool, data=Data, family="poisson")
summary(Mod1)
X = model.matrix(Mod1) # --> Ho una serie di 0 e di 1, ma in realtà nell'intercetta sono inglobati i valori della lana di riferimento
                       #     e della tensione di riferimento quindi non è possibile a questo livello capire quanto pesa sull'intercetta
                       #     la lana di riferimento  e quanto la tensione di riferimento.

## Calcoliamo gli intervalli di confidenza
confint.default(Mod1)


# Possiamo fare dei CONTRASTI:
# sto confrontando delle coppie di covariate per vedere se c'è della differenza sostanziale. 

Cont1 = contrast(Mod1, 
                 list(tension="M", wool = "A"),
                 list(tension="L", wool = "A"), type="individual" )
print(Cont1, X=T)


Cont2 = contrast(Mod1 , 
                 list(tension="H", wool = "A"),
                 list(tension="M", wool = "A"), type="individual" )
print(Cont2, X=T)


Cont3 = contrast(Mod1 , 
                 list(tension="H", wool = "B"),
                 list(tension="L", wool = "B"), type="individual" )
print(Cont3, X=T)

Cont4 = contrast(Mod1 , 
                 list(tension="H", wool = "A"),
                 list(tension="L", wool = "A"), type="individual" )
print(Cont4, X=T)



# CALCOLIAMOLO MANUALMENTE QUESTO CONTRASTO:
Cont.hand = contrast(Mod1 , 
                 list(tension="M", wool = "A"),
                 list(tension="H", wool = "A"), type="individual" )
print(Cont.hand, X=T)

# salvo le stime dei coefficienti
coefs=matrix(coef(Mod1),ncol=1)
coefs

#calcolo la matrice di covarianza dei coefficienti J=x'Wx
covmatJ=solve(t(X)%*%diag(Mod1$weights)%*%X)
Mod1$weights
covmatJ

# scelgo il vettore v della configurazione
v=array(0,dim=length(coefs))
v[c(2:3)]=c(1,-1)
v

##  valore del parametro sotto analisi
Diff=v%*%coefs
Diff
var=v%*%covmatJ%*%v

# Tvalue
tvalue=Diff/(var^0.5)
df=length(Data$breaks)-4

# t-test
pt(tvalue,df,lower.tail = F)+pt(-tvalue,df,lower.tail = T)




## ## ## ## 
## TESTS ##
## ## ## ## 

# Test tutto/niente sulla delta-devianza: testo se il modello in questione è
# migliore del modello a sola intercetta.
str(summary(Mod1))
D0 = summary(Mod1)$null.deviance
gdl0 = summary(Mod1)$df.null #N-1
D1 = summary(Mod1)$deviance
gdl1 = summary(Mod1)$df.residual #N-4

deltaD = D0-D1
pchisq(deltaD, gdl0-gdl1, lower.tail=F)


# Test della devianza, vedo se il modello è buono come il saturo.
pchisq(D1, gdl1, lower.tail=F)

# rappresentazione grafica
xseq = seq(0,220, length.out=100)
plot(xseq,dchisq(xseq,gdl1), type="l", main="Test della Devianza",
     xlab="D", ylab="Devianza sotto H0")
abline(v=D1,col=2)



## ## ## ## ## ## ## ## ## 
## Misure di influenza  ##
## ## ## ## ## ## ## ## ##

InfMeas1 = influence.measures(Mod1)
InfMeas1Mat = InfMeas1$infmat
ObsInf = InfMeas1$is.inf


# i risultati dipendono dalla parametrizzazione
summary(InfMeas1)
s=1:54 #indici delle osservazioni
plot(InfMeas1Mat[,1])

plot(InfMeas1Mat[,2])
points(s[Data$tension=='H'],InfMeas1Mat[Data$tension=='H',2],col='red')

plot(InfMeas1Mat[,3])
points(s[Data$tension=='M'],InfMeas1Mat[Data$tension=='M',3],col='red')






## ## ## ## ## ## ## ## ## ## ##
## introduciamo l'interazione ##
## ## ## ## ## ## ## ## ## ## ##

Mod2 = glm(breaks ~ tension+wool+tension:wool, data=Data, family=poisson)
Mod3 = glm(breaks ~ (tension+wool)^2, data=Data, family=poisson)

summary(Mod2)
summary(Mod3)

# AIC
AIC(Mod1,Mod2,Mod3)

# TEST SULLE DEVIANZE
# Testo i modelli annidati

# con funzione
anova(Mod1,Mod2,test="Chisq")

# a mano
D2=Mod2$deviance
gdl2=Mod2$df.residual
pchisq(D1-D2,gdl1-gdl2,lower.tail = F)




