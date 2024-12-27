###############################################################
### 30-11-2020 Negative Binomial and Zero-inflated Poisson ####
###############################################################


rm(list=ls())

## librerie
library(pscl)
library(sandwich)
library(lmtest)
library(MASS)


#####
# 1       ofp     Number of physician office visits (integer outcome)
# 2      ofnp     Number of non physician office visits (integer outcome)
# 3       opp
# 4      opnp
# 5      emer
# 6      hosp     Number of hospital stays (integer)
# 7    health     Self-perceived health status (poor, average, excellent)
# 8  numchron     Number of chronic condition (integer)
# 9   adldiff
# 10   region
# 11      age
# 12    black
# 13   gender     Gender (female, male)
# 14  married
# 15   school     Number of years of education (integer)
# 16   faminc
# 17 employed
# 18  privins     Private insurance indicator (no, yes)
# 19 medicaid

## Load
load("/Users/marcoratta/Desktop/Università/Metodi Statistici/Negative Binomial + ZIP/211209 DebTrivedi.RData")
Data = DebTrivedi
summary(Data)
head(Data)


## facciamo un po' di plot descrittivi
plot(sort(Data$ofp))
hist(Data$ofp, breaks = 0:90-0.5)
plot(table(Data$ofp))

## analisi bivariata
plot(ofp ~ numchron, data = Data)
# e creaiamo una funzione che trasformi una numerica in un fattore
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], 
                     ifelse(diff(breaks) > 1,
                            c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                     sep = "")
  return(x)
}
plot((ofp) ~ cfac(numchron), data = Data)




## abbiamo molti valori estremi, e per fare plot utiliziamo il log della variabile
clog <- function(x) log(x + 0.5) # -> il + 0.5 serve a far vedere i valori nulli


par(mfrow=c(3,2))
plot(clog(ofp) ~ health, data = Data, varwidth = TRUE)
plot(clog(ofp) ~ cfac(numchron), data = Data)
plot(clog(ofp) ~ privins, data = Data, varwidth = TRUE)
plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = Data)
plot(clog(ofp) ~ gender, data = Data, varwidth = TRUE)
par(mfrow=c(1,1))




## ## ## ## ## ## ## ## ##
## Scegliamo la formula ##
## ## ## ## ## ## ## ## ##

formula <- ofp ~ hosp + health + numchron + gender + school + privins
## e stimiamo il modello
modelPoisson <- glm(formula = formula,
                    family  = poisson(link = "log"),
                    data    = Data)
summary(modelPoisson)

## I coefficienti nella scala della media
exp(coef(modelPoisson))


## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##  Possiamo anche fittare un modello NB  ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
modelNB <- glm.nb(formula = formula,
                  data    = Data)
summary(modelNB)

## coefficienti
exp(coef(modelNB))

# e vedere i coefficienti dei due modelli insieme
rbind(exp(coef(modelPoisson)),exp(coef(modelNB)))

# e il loro intervalli di confidenda
cbind(confint.default((modelPoisson)),confint.default((modelNB)))

cbind(confint.default((modelPoisson))[,2]-confint.default((modelPoisson))[,1],
      confint.default((modelNB))[,2]-confint.default((modelNB))[,1])



## ## ## ## ## ## ## ## ## ## ##
## Fit zero-inflated Poisson  ##
## ## ## ## ## ## ## ## ## ## ##

# Negative Binomial + latent Binomial
modelZeroInfl <- zeroinfl(formula = formula,
                          dist    = "negbin",
                          data    = Data)
summary(modelZeroInfl)

# Poisson + latent Binomial
modelZeroInfl <- zeroinfl(formula = formula,
                          data    = Data)
summary(modelZeroInfl)


## E i corrispettivi coefficienti
expCoef <- exp(coef((modelZeroInfl)))
expCoef <- matrix(expCoef, ncol = 2)


## Possiamo facilmente settare differenti modelli per le due parti
modelZeroInflSimpler <- zeroinfl(formula = ofp ~ hosp + health + numchron + gender + school + privins | hosp + numchron + gender + school + privins,
                                 dist    = "negbin",
                                 data    = DebTrivedi)
summary(modelZeroInflSimpler)

## E fare un test delle verosimiglianza (Devianza)
lmtest::lrtest(modelZeroInfl, modelZeroInflSimpler)


## possiamo fare diversi predict  
?predict.zeroinfl
plot(predict(modelZeroInfl,type="zero"))

# decidiamo con l'AIC quale sia il modello migliore
AIC(modelPoisson,modelNB,modelZeroInfl,modelZeroInflSimpler)
