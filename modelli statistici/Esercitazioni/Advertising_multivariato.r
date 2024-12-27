# -------------------------------------
#    MODELLI STATISTICI AA 2024/2025 
# -------------------------------------

# Regressione lineare - Marco Ratta
#
# Dataset: Adverstising
#
# Il dataset contiene le vendite, di migliaia di unita', di un prodotto in 200 mercati,
# insieme al budget speso per la pubblicita', in migliaia di dollari, su 3 media: Tv,
# radio e giornali.
# Lo scopo e' determinare quale strategia pubblicitaria e' migliore e prevedere
# le vendita in base al budget speso per ogni media


rm(list=ls())  # Cancello le variabili salvate


# Leggiamo il file csv
library(readr)

# Settiamo la directory di lavoro  ---------
setwd("/Users/marcoratta/Desktop/PhD/Teaching/Modelli Statistici/Materiale R/Advertising")

# Carichiamo i dati  -----------------------
advertising <- read_csv("Advertising.csv")
attach(advertising)


############
# RICHIAMO #
############

advertising$TV2 = TV^0.5  # Avevamo trovato che la regressione migliore a una sola covariata era quella trovata
                          # facendo la trasformazione da TV alla sua radice
attach(advertising) # Richiamo "attach" perchè ho aggiunto la colonna con la radice di TV 
SimpleReg4 = lm(Sales~TV2)
summary(SimpleReg4)

res = SimpleReg4$residuals


# Analizziamo i residui:
#
# 1) QQ-plot (vedo se quantili empirici dei residui standardizzati coincidono
#             con i quantili teorici della Normale(0,1))
#
# 2) Istogramma dei residui (vedo se c'è - almeno ad occhio - normalità)
#
# 3) Plot fitted/residual (vedo se c'è simmetria rispetto allo zero, se ci sono
#                          eventuali componenti curvilinee e se l'ipotesi di 
#                          omoschedasticità è rispettata)
#
# 4) Shapiro-test 


# Istogramma
par(mfrow=c(1,1))
hist(SimpleReg4$residuals/sqrt(var(SimpleReg4$residuals)),main="Istogramma residui", xlab="Residui",
     ylab="Frequenze")

# Altri plot (vengono prodotti passando il modello a plot())
par(mfrow=c(2,2))
plot(SimpleReg4)

# Shapiro-test
shapiro.test(res) 

# CONSLUSIONI: Sia da una diagnostica "ad occhio", sia con un test formale
#              tipo Shapiro non possiamo rifiutare l'ipotesi nulla di normalità
#              dei residui. Tuttavia, notiamo ancora eteroschedasticità.
#              La trasformazione TV2 ha migliorato il problema della 
#              componente curvilinea. 




#############################################
# REGRESSIONE MULTIVARIATA (NO INTERAZIONI) #
#############################################

# --------------------
# SCELTA DEL MODELLO
# --------------------

# Regressione Multipla 1
MultiReg1 = lm(Sales~TV+Radio+Newspaper)  # Se voglio inserire la dipendenza non solo da una covariata ma da tante
                                          # covariate basta agiiungerle con il segno +.
                                          # Modello effetti di più covariate, ma NON le loro interazioni.
summary.lm(MultiReg1)

# Regressione Multipla 2
MultiReg2 = lm(Sales~TV2+Radio+Newspaper)
summary.lm(MultiReg2)  # In tutti e due i modelli la variabile Newspaper non è significativa
                       # e andrebbe eliminata dal modello in quanto il suo p value è molto alto
                       # e conseguentemente la probabilità di avere valori più estremali di
                       # quelli stimati è altissima, indice del fatto che l'ipotesi H_0 che
                       # quel coefficiente sia nullo è da accettare.

# Regressione Multipla 3 (senza NEWS)
MultiReg3 = lm(Sales~TV+Radio)
summary.lm(MultiReg3)

# Regressione Multipla 3.bis (confronto modelli annidati per mostrarvi test F)
MultiReg3.bis = lm(Sales~Radio)
anova(MultiReg3, MultiReg3.bis)

# Regressione Multipla 4 (senza NEWS ma con TV2)
MultiReg4 = lm(Sales~TV2+Radio)
summary.lm(MultiReg4)

# Scelta del modello
AIC(MultiReg1,MultiReg2, MultiReg3,MultiReg4) # il quarto è il migliore (potrei guardare anche gli indici R^2 e R^2 aggiustato,
                                              # ma queste due misure sono fortemente dipendenti dal numero di campioni quindi posso
                                              # usarle solo quando ho un numero di gdl uguale fra i modelli da confrontare)

# CONCLUSIONE --> SCELGO IL MODELLO 4 


# --------------------------
# INTERVALLI DI CONFIDENZA
# --------------------------

paste(round(coef(MultiReg4),3), " (95%CI: [",round(confint(MultiReg4, level=0.95),3)[,1], " ; ",round(confint(MultiReg4, level=0.95),3)[,2], "])", sep="")

# ----------------
# ERRORI STANDARD
# ----------------
paste(round(coef(MultiReg4),3), " (SE: ",round(summary.lm(MultiReg4)$coefficients[,2],3), ")",sep="")


# --------------------
# ANALISI DEI RESIDUI
# --------------------
res2=MultiReg4$residuals

# oppure
res = rstandard(MultiReg4)

# plot dei residui rispetto alle variabili usate nel modello
par(mfrow=c(1,3))
plot(fitted(MultiReg4),res, main="Residui VS Fitted")
abline(h=0, col="green", lwd=2)
plot(TV2, res, main="Residui VS TV2")
abline(h=0, col="green", lwd=2)
plot(Radio, res, main="Residui VS Radio")      
abline(h=0, col="green", lwd=2)

# OSSERVAZIONE:
# Vediamo che la media dei residui è abbastanza centrata sullo 0, ma esiste chiaramente
# una struttura per cui i residui tendono a essere negativi per le fitted centrali e 
# e positivi per osservazioni alte e crescenti.
# La struttura si vede anche plottando i residui contro i predittori.



# ----------------------
# ANALISI DI INFLUENZA 
# ----------------------

meas.inf = influence.measures(MultiReg4)$infmat
colnames(meas.inf)

# SIGNIFICATO:
#"dfb.1_"  		variazioni nel valore dell'intercetta
#"dfb.TV2" 		variazioni nel valore del coefficiente regressivo di TV2
#"dfb.Radi" 	variazioni nel valore del coefficiente regressivo di Radio
#"dffit"		  variazioni nella previsione i-esima
#"cov.r"			variazioni nella matrice di covarianza
#"cook.d"			variazione nella previsione generale
#"hat"				matrice di influenza

par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dfb.Radi"])
plot(meas.inf[,"dffit"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])  

# Noto dal grafico che ci sono delle misure di influenza che sono
# molto più alte di altre e ciò significa che è possibile che ci sia stato
# un errore di misurazioe (non sta a noi stabilirlo)


which(meas.inf[,"dfb.1_"]< -0.4) # Cerco quale osservazione è anomala andando a pescarla con una treshold
which(meas.inf[,"dfb.TV2"]>0.6)  # Scopriamo quindi che l'osservzione incriminata è la n° 131.
which(meas.inf[,"cook.d"]>0.2)

# OSSERVAZIONE:
# l'osservazione 131 sembra essere anomala !!

summary.lm(MultiReg4)
summary(advertising)
advertising[131,]
res[131]
summary(res)  # l'osservazione ha residuo alto in modulo
              # ha valori alti per radio e bassi per TV2
              # e valore di vendita medio basso


# Vediamo il valore predetto
predict(MultiReg4)[131]


# ------------------------------------------------
# ANALISI PREDITTIVA MODELLO Sales ~ TV2 + Radio
# ------------------------------------------------
# Intervallo di Confidenza CI: Mi indica un'intervallo con copertura al 95% per E[Y|X=x]
# Intervallo di Previsione PI: Mi indica un'intervallo con copertura al 95% per Y|X=x

predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=0, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=30, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=100), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=30, Newspaper=100), interval="confidence")

predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=0), interval="prediction")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=0, Newspaper=0), interval="prediction")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=30, Newspaper=0), interval="prediction")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=100), interval="prediction")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=30, Newspaper=100), interval="prediction")




############################################
# REGRESSIONE MULTIVARIATA CON INTERAZIONE #
############################################

# --------------------
# SCELTA DEL MODELLO
# --------------------

# con tre variabili abbiamo molti modelli da testare; infatti oltre agli effetti 
# singoli si possono tenere in considerazione gli effetti multipli (interazioni).
# E' possibile infatti che due covariate non esprimano il proprio effetto indipendentemente
# ma si rafforzino o inibiscano a vicenda. (es se una pubblicità viene fatta solo su TV male,
# solo su Radio male, ma se fatta su entrambi l'effetto diventa positivo)

# ATTENZIONE:
# Non si dovrebbe (a priori) mettere un'interazione se non sono presenti anche gli effetti singoli,
# il modello diventa difficile da interpretare

# ATTENZIONE:
# ci sono differenti uguali modi per specificare l'interazione (A:B, A*B, (A+B)^2)

Reg1 = lm(Sales~TV2:Radio)  # Modella solo le interazioni (pericoloso, non si dovrebbe fare se non ci sono gli
                            # effetti singoli)
Reg2 = lm(Sales~TV2*Radio)  # Interazini singole + multiple
Reg3 = lm(Sales~(TV2+Radio)^2) # Interazioni singole + multiple
summary(Reg1)
summary(Reg2)
summary(Reg3)

RegAll = lm(Sales~(TV2+Radio+Newspaper)^2) #Modello completo con tutte le interazioni
summary.lm(RegAll)

# OSSERVAZIONE:
# notate come newspaper ha un effetto interattivo significativo, quindi prima di toglierlo
# di botto dopo aver fatto la regressione senza interazione conviene fare la regressione
# con interazione e vedere se la covariata è influente se accoppiata


# Utilizziamo la funzione step
?step
RegStep = step(RegAll)
summary(RegStep)

# nel modello migliore, newspaper ha un effetto singolo significativo,
# anche se nel modello senza interazione non era significativa
# In generale (non in questo caso) step potrebbe scegliere un modello
# con alcune variabili non significative, che vanno poi eliminate


# --------------------------
# INTERVALLI DI CONFIDENZA
# --------------------------
paste(round(coef(RegStep),3), " (95%CI: [",round(confint(RegStep, level=0.95),3)[,1], " ; ",round(confint(RegStep, level=0.95),3)[,2], "])", sep="")

# ----------------
# ERRORI STANDARD
# ----------------
paste(round(coef(RegStep),3), " (SE: ",round(summary.lm(RegStep)$coefficients[,2],3), ")",sep="")


# --------------------
# ANALISI DEI RESIDUI 
# --------------------

res=RegStep$residuals
# plot dei residui
plot(fitted(RegStep),res)
abline(h=0)
# ci sono delle strutture nei residui (inverse U-shape) - ma è meglio di prima
# Delle strutture rimangono
par(mfrow=c(3,2))
plot(predict(RegStep), res)
plot(TV2, res)
plot(Radio, res)
plot(Newspaper, res)
plot(TV2*Newspaper, res)
plot(TV2*Radio, res)


# ----------------------
# ANALISI DI INFLUENZA 
# ----------------------

meas.inf = influence.measures(RegStep)$infmat
colnames(meas.inf)
par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dfb.Radi"])
plot(meas.inf[,"dfb.Nwsp"])
plot(meas.inf[,"dfb.TV2:R"])
plot(meas.inf[,"dfb.TV2:N"])

par(mfrow=c(2,2))
plot(meas.inf[,"dffit"])
plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])


which(meas.inf[,"dfb.TV2"]>0.4)
which(meas.inf[,"dfb.Radi"]< -1)
which(meas.inf[,"dfb.Nwsp"]>0.4)
which(meas.inf[,"dfb.TV2:R"]> 1)
which(meas.inf[,"dffit"]< -0.7)
which(meas.inf[,"cook.d"]>0.2)

# OSSERVAZIONE: Le osservazioni 131 e 156 sembrano essere influenti,
#               ripetiamo queste ultime analisi senza queste osservazioni

# Scelta automatica del miglior modello
RegAll2 = lm(Sales~(TV2+Radio+Newspaper)^2, data = advertising[-c(131,156),])
summary.lm(RegAll2)

# Utilizziamo la funzione step
RegStep2 = step(RegAll2)
summary(RegStep2)


# residui
res=RegStep2$residuals
# plot dei residui
plot(fitted(RegStep2),res)
abline(h=0)
# ci sono delle strutture nei residui (inverse U-shape) - ma è meglio di prima


# Delle strutture rimangono
par(mfrow=c(3,2))
plot(predict(RegStep2), res)
plot(advertising[-c(131,156),]$TV2, res)
plot(advertising[-c(131,156),]$Radio, res)
plot(advertising[-c(131,156),]$Newspaper, res)
plot(advertising[-c(131,156),]$TV2*advertising[-c(131,156),]$Newspaper, res)
plot(advertising[-c(131,156),]$TV2*advertising[-c(131,156),]$Radio, res)
# I risultati sono migliori
