# -----------------------------------------------------
#    Si vuole studiare come gli studenti decidano                                            
#    il tipo di programma da seguire                                                         
#    tra generale, accademico e tecnico (vocation)                                           
# -----------------------------------------------------
#    COVARIATE:
#    ses: stato economico                                                                    
#    schtyp: tipo di scuola (variabile categorica --> deve essere trattata come fattoriale)  
#    read, write, math e science: valutazione nelle rispettive materie (numero intero)       

 
rm(list =ls()) #cancello le variabili 
 

# Carico le librerie
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


# Setto la directory
setwd("/Users/marcoratta/Desktop/Università/Metodi Statistici/Binomial GLM")

# Carico i dati
load('211202 DataBinomial.RData')
summary(Data)
head(Data)


#########################
#  Analisi Descrittive  #
#########################

?with
with(Data, table(ses, prog))  #Il comando 'with' serve a valutare un'espressione in un ambiente locale.
table(Data$ses, Data$prog)    #In questo caso fa una tabella di conteggio con la doppia entrata ses-prog

?do.call
?tapply
with(Data, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x))))) # prende i dati in Data e fa la media e varianza 
with(Data, do.call(rbind, tapply(write, ses, function(x) c(M = mean(x), SD = sd(x)))))  # di write per tutti i vari gruppi di prog e ses.  

mean(Data$write[Data$prog=='general']) # E' equivalente alla prima riga della tabella; prende della colonna write di data, solo i dati 
sd(Data$write[Data$prog=='general'])   # che hanno prog=general e ne calcola media e varianza.


# Qualche boxplot (il primo argomento mi dice su che variabile devo fare il boxplot, il secondo mi dice quanti farne, in questo caso
# uno per ogni fattore di prog)
boxplot(Data$write ~ Data$prog, main="Write VS Program", xlab="Program", ylab="Write")
boxplot(Data$science ~ Data$prog, main="Science VS Program", xlab="Program", ylab="Science")
boxplot(Data$math~ Data$prog, main="Math VS Program", xlab="Program", ylab="Math")

# Printo i quantili
with(Data, do.call(rbind, tapply(write, prog, quantile)))


# possiamo testare delle ipotesi con il t-test o equivalentemente con un modello lineare o anova.
#L'obiettivo è dire se la scelta di prog è influenzata dalla valutazione 'write'. 
t.test(Data$write[Data$prog=="general"],Data$write[Data$prog=="academic"],var.equal=T)
summary(lm(write~ prog, data =Data[Data$prog!="vocation",] ))                           
summary(aov(write ~ prog, data=Data[Data$prog!="vocation",]))

# OSS: C'è correlazione fra le due variabili in quanto il p-value è molto basso,
#      (inferiore a 1%).
#
# OSS: I tre comandi fanno la stessa cosa in questo caso, ma ricordare che "t.test"
#      può essere usato per confrontare solo 2 gruppi, "aov" può essere usato per 
#      confrontare più gruppi, mentre "lm" è comprensivo di tutti i modelli lineari
#      di cui ANOVA fa parte.



###############################
# Testiamo i modelli nominali #
###############################

par(mfrow=c(2,3))
scatter.smooth(Data$math,Data$science, xlab="Math", ylab="Science", col="green", pch=16, cex=0.7)  
scatter.smooth(Data$math,Data$write, xlab="Math", ylab="Write", col="green", pch=16, cex=0.7)    
scatter.smooth(Data$science,Data$write, xlab="Science", ylab="Write", col="green", pch=16, cex=0.7) 
scatter.smooth(Data$read,Data$write, xlab="Read", ylab="Write", col="green", pch=16, cex=0.7)
scatter.smooth(Data$read,Data$science, xlab="Read", ylab="Science", col="green", pch=16, cex=0.7)
scatter.smooth(Data$read,Data$math, xlab="Read", ylab="Math", col="green", pch=16, cex=0.7)


# MODELLO BERNOULLIANO PER MODELLARE SE UNO STUDENTE SCEGLIERA' IL 'VOCATIONAL' O IL 'GENERAL'
# OSS: Non prendo i dati 'academic' in questa fase, quindi di fatto utilizzo solo
#      una parte del dataset.

Data$V1V3 = ifelse(Data$prog=="vocation",1,0) #aggiungo una colonna con 1 se il programma scelto è il vocational e 0 altrimenti.
Mod13 = glm(V1V3 ~ ses + schtyp + read + write + math, data = Data[Data$prog!="academic",], family="binomial" )
summary(Mod13) 

# OSS: Ricordiamo che sto modellando il logit(pi), quindi per le covariate quantitative il valore di beta rappresenterà l'incremento
#      del logit della probabilità; per quelle categoriche beta si guarda l'odds ratio e beta rappresenta la differenza fra i logit
#      corrispondenti ai 2 gruppi, tenendo le altre covariate uguali.
#      MORALE: I coefficienti non sono significativi a parte l'intercetta, quindi questo modello non rileva correlazione fra 
#              le covariate e la scelta fra i due programmi vocational e general.


# MODELLO BERNOULLIANO PER MODELLARE SE UNO STUDENTE SCEGLIERA' IL 'GENERAL' O 'ACADEMIC'
Data$V1V2 = ifelse(Data$prog=="academic",1,0)
Mod12 = glm(V1V2 ~ ses + schtyp + read + write + math, data =Data[Data$prog!="vocation",], family="binomial" )
summary(Mod12) 

# MORALE: In questo caso c'è il coefficiente relativo alla covariata 'math' che è significativo e positivo, quindi
#         deduco che essere bravo in matematica aumenta la probabilità di scegliere un curriculum 'academic'.



#Vediamo i parametri dei due modelli a confronto
cbind(summary(Mod13)$coefficients[,c(1,4)],summary(Mod12)$coefficients[,c(1,4)])

#Scegliamo i subset dei parametri con step: l'obiettivo è testare dei GLM con tutte le possibili combinazioni di covariate e 
#scegliere il modello migliore secondo il criterio dell'AIC.
#
# OSS: Con step vado a testare anche gli effetti interattivi!!

StepMod13 = step(Mod13) #dentro la variabile sarà conservato il sottomodello migliore di Mod13
summary(StepMod13)

StepMod12 = step(Mod12) #dentro la variabile sarà conservato il sottomodello migliore di Mod12
summary(StepMod12)

## Possiamo calcolare le previsioni: faccio l'esponenziale e di fatto quello che sto modellando è il rapporto fra le probabilità 
## prima di vocational e general e poi di general e academic.

Exp13 = exp(predict(StepMod13, type="link", newdata=Data))
Exp12 = exp(predict(StepMod12, type="link", newdata=Data))


pigeneral = 1/(1+Exp13+Exp12) # GENERAL
pivocation = Exp13/(1+Exp13+Exp12) # VOCATION
piacademic = Exp12/(1+Exp13+Exp12) # ACADEMIC

Prev = cbind(pigeneral,pivocation,piacademic)
rowSums(Prev) #ovviamente la somma delle 3 probabilità deve fare 1.
Prev

# Alcuni esempi
Data[10,]
Prev[10,]


############################
# INTERPRETAZIONI GRAFICHE #
############################

par(mfrow=c(1,1))
## write
plot(0,0, xlim=c(min(Data$write), max(Data$write)), ylim=c(0,1.2),
     xlab="Write", ylab="Prob(PROG=i)", main="Program Prediction")
points(Data$write,Prev[,1], col=2, pch=20)
points(Data$write,Prev[,2], col=3, pch=20)
points(Data$write,Prev[,3], col=4, pch=20)
legend(x=32, y=1.2, c("General", "Vocation", "Academic"), col=c(2,3,4), pch=16)

par(mfrow=c(1,1))
## math
plot(0,0, xlim=c(min(Data$write), max(Data$write)), ylim=c(0,1.2),
     xlab="Math", ylab="Prob(PROG=i)", main="Program Prediction")
points(Data$math,Prev[,1], col=2, pch=20)
points(Data$math,Prev[,2], col=3, pch=20)
points(Data$math,Prev[,3], col=4, pch=20)
legend(x=32, y=1.2, c("General", "Vocation", "Academic"), col=c(2,3,4), pch=16)


## per ogni condizione economica (bassa, media, alta) faccio un boxplot con le probabilità di scegliere uno dei 3 programmi.
## Si può notare che mediamente la probabilità di fare un academic cresce con lo status economico.
par(mfrow=c(1,3))
boxplot(Prev[,1] ~ Data$ses, col=2:4, ylim=c(0,1), xlab="SES", ylab="P(General)", main="General VS SES")
boxplot(Prev[,2] ~ Data$ses, col=2:4, ylim=c(0,1), xlab="SES", ylab="P(Vocational)", main="Vocational VS SES")
boxplot(Prev[,3] ~ Data$ses, col=2:4, ylim=c(0,1), xlab="SES", ylab="P(Academic)", main="Academic VS SES")

par(mfrow=c(1,1))



###########################################
# Possiamo testare altri tipi di modelli  #
###########################################
#
# Per esempio posso voler costruire un modello binomiale avente come
# output la scelta del programma accademico VS altro.

Data$V1 = ifelse(Data$prog=="academic",1,0)

ModV1 = glm(V1 ~ ses + schtyp + read + write + math, data =Data, family="binomial" )
summary(ModV1) 

Prev=ModV1$fitted.values
probsV1 <- predict(ModV1,newdata=Data,type='response') # qui mi da la response, quindi estrae già le probabilità dal logit.

# Qual è l'accuratezza del modello sul "training set"?
mean(round(probsV1)==(Data$prog=="academic"))
mean(round(probsV1[Data$prog=="academic"]))
mean((Data$prog=="academic"))


# ----------------------------------------------------------

Data$V2 = ifelse(Data$prog=="general",1,0)

ModV2 = glm(V2 ~ ses + schtyp + read + write + math, data =Data, family="binomial" )
summary(ModV2) 

Prev=ModV2$fitted.values
probsV2 <- predict(ModV2,newdata=Data,type='response') # qui mi da la response, quindi estrae già le probabilità dal logit.

# Qual è l'accuratezza del modello sul "training set"?
mean(round(probsV2)==(Data$prog=="general"))
mean(round(probsV2[Data$prog=="general"]))
mean((Data$prog=="general"))

# ------------------------------------------------------------

Data$V3 = ifelse(Data$prog=="vocation",1,0)

ModV3 = glm(V3 ~ ses + schtyp + read + write + math, data =Data, family="binomial" )
summary(ModV3) 

Prev=ModV3$fitted.values
probsV3 <- predict(ModV3,newdata=Data,type='response') # qui mi da la response, quindi estrae già le probabilità dal logit.

# Qual è l'accuratezza del modello sul "training set"?
mean(round(probsV3)==(Data$prog=="vocation"))
mean(round(probsV3[Data$prog=="vocation"]))
mean((Data$prog=="vocation"))

# ------------------------------------------------------------
#
# Faccio alcuni test sul modello V1: ACADEMIC/NON ACADEMIC

testdata=data.frame(ses=c('low','middle','high'), schtyp=c(rep("public",3), rep("private",3)), write=mean(Data$write),math=mean(Data$math),read=mean(Data$read))
testdata$prob=predict(ModV1,newdata=testdata,type='response')
testdata


testdata=data.frame(ses=c('low'), schtyp=c(rep("public",3), rep("private",3)), write=c(30,40,50),math=mean(Data$math),read=mean(Data$read))
testdata$prob=predict(ModV1,newdata=testdata,type='response')
testdata

