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


# 20 Novembre 2024

# LIBRARIES  -------------------------------------------------------------

# Functions
# le due funzioni riportate di sequito si trovano nella documentazione di pairs
?pairs

# mettere sulla diagonale le correlazioni
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}


# mettere sulla diagonale gli istogrammi
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


# Settiamo la directori di lavoro  ------------------------------------------------------
setwd("/Users/marcoratta/Desktop/PhD/Teaching/Modelli Statistici/Materiale R/Advertising")


# Carichiamo i dati  --------------------------------------------------------------

library(readr)
advertising <- read_csv("/Users/marcoratta/Desktop/Università/Metodi Statistici/Advertising/Advertising.csv")

# quando si caricano i dataset fare attenzione a separatori, decimali e header

# leggiamo le prime righe del dataset per avere un'idea di come è fatto il dataset, che nel 
# nostro caso è formato da una colonna risposta "vendite" e 4 covariate che indicano i soldi
# spesi in pubblicità di vario tipo (TV, radio e giornali)

head(advertising)

# facciamo un summary
summary(advertising)

#TV # Perche' questo comando non funziona? 
   # Bisogna fare attach del dataset che serve a connettere il dataset a delle 
   # variabili in leggibili in R per poi richiamarle facilmente.

attach(advertising)  # Con il comando detach(advertising) si elimina l'effetto di attach
                     # e il dataset torna a essere staccato da R



### ### ### ### ### ### ### ###
###  Anailisi descrittive   ###
### ### ### ### ### ### ### ###

# Servono a fare una prima esplorazione dei dati e consistono essenzialmene nello 
# studio delle dimensioni del dataset, dei quantili, di media-mediana-moda e
# nell'osservazione grafica della distribuzione dei dati e delle correlazioni fra variabili.

# Dimensioni del dataset
length(advertising) # Numero di variabili: 5 (1 è un numero incrementale che indica le osservazioni, 
                    # 3 indicano le covariate e 1 è la variabile risposta)
										# questo comando funziona solo con dataframe, ma non con matrici.

dim(advertising)		# Righe e colonne

names(advertising)  # nome variabili, potete usare anche colnames(advertising)

length(TV)					# numero di osservazioni (lunghezza della prima colonna)

# altre alternative
nrow(advertising)
ncol(advertising)

# plots
plot(advertising)  # fa un plot di tutte le variabili contro tutte le variabili
?pairs
pairs(advertising[,2:5]) # fa un plot i tutte le variabili contro tutte le variabili
                         # escludendo la prima che non è quantitativa ma conta solo 
                         # il numero di osservazioni

pairs(advertising[,2:5], upper.panel=panel.cor) # Dà l stesso grafico di prima ma 
                                                # indica il valore numerico delle correlazioni
                                                # nella parte alta dei grafici

pairs(advertising[,2:5], upper.panel=panel.cor, diag.panel=panel.hist) # Dà lo stesso grafico di prima ma  con
                                                                      # gli istogrammi delle singole variabili sulla
                                                                      # diagonale

# Come distribuire i grafici in un'altra maniera

par(mfrow=c(nrows=1,ncols=3)) # Questo comando dice ad R di creare un pannello 
                              # con 3 box messi in riga; ogni plot che verrà fatto in futuro
                              # verà inserito in questi 3 box.

plot(x=TV, y=Sales, pch=16, col="red",
     main="TV vs SALES", xlab="TV", ylab="Sales")  # Questo modo di chiamare plot vuole la X, la Y e 
                                                   # il valore di pch che è il marker con cui voglio colorare i pallini
                                                   # e il colore.
plot(x=Radio, y=Sales, pch=16, col="red",
     main="Radio vs SALES", xlab="Radio", ylab="Sales")

plot(x=Newspaper, y=Sales, pch=16, col="red",
     main="Newspaper vs SALES", xlab="Newspaper", ylab="Sales")



# stima smooth della relazione

?scatter.smooth  # Parte dallo scatter plot e disegna una curva liscia che mi dà
                 # un'idea di quale sia la legge che fitta i dati

par(mfrow=c(nrows=1,ncols=3)) 
scatter.smooth(x=TV, y=Sales, pch=16, col="red",
               main="TV vs SALES", xlab="TV", ylab="Sales")
scatter.smooth(x=Radio, y=Sales, pch=16, col="red",
               main="Radio vs SALES", xlab="Radio", ylab="Sales")
scatter.smooth(x=Newspaper, y=Sales, pch=16, col="red",
               main="Newspaper vs SALES", xlab="Newspaper", ylab="Sales")


# aumentare lo smooth - Il comando "span" serve a aumentare la precisione della linea
par(mfrow=c(nrows=1,ncols=3)) 
scatter.smooth(x=TV, y=Sales, pch=16, col="red",
               main="TV vs SALES", xlab="TV", ylab="Sales", span=0.1)
scatter.smooth(x=Radio, y=Sales, pch=16, col="red",
               main="Radio vs SALES", xlab="Radio", ylab="Sales", span=0.1)
scatter.smooth(x=Newspaper, y=Sales, pch=16, col="red",
               main="Newspaper vs SALES", xlab="Newspaper", ylab="Sales", span=0.1)






##############################
# MODELLO 1: SALES CONTRO TV #
##############################

### Regressione contro TV

SimpleReg1 = lm(Sales~TV)  # la sintassi di 'lm' vuole che si indichi prima la
                           # variabile dipendente, poi l'ondina e poi le covariate del modello
                           # separate da un +
modello1_summary=summary(SimpleReg1)  #Il summary del modello indica tutto ciò che c'è da
                                      # sapere sul modello, in particolare vengono esplicitati 
                                      # massimo minimo e quantili dei residui; valori stimati dei 
                                      # coefficienti del modello, T value e probabilità di ottenere
                                      # valori più estremali. Inoltre vengono dati i valori di R^2 e 
                                      # R^2 aggiustato che danno un'idea della bontà del modello.


# Leggiamo l'output del modello
modello1_summary
modelCoeffs <- modello1_summary$coefficients 
modelCoeffs

# test intercetta
# analizziamo i risultati relativi all'intercetta
beta1.estimate <- modelCoeffs["(Intercept)", "Estimate"]
beta1.estimate

#analizziamo i risultati relativi al coefficiente angolare
beta2.estimate <- modelCoeffs["TV", "Estimate"]
beta2.estimate



#################################
# MODELLO 2: SALES CONTRO RADIO #
#################################

SimpleReg2 = lm(Sales~Radio)
summary(SimpleReg2)


#################################
# MODELLO 3: SALES CONTRO NEWS  #
#################################

SimpleReg3 = lm(Sales~Newspaper)
summary(SimpleReg3)


######################
# BONTA' DEL MODELLO #
######################

# Per scegliere quale modello spiega meglio i dati confronto gli R^2 e R^2 
# aggiustato, metto gli indici in 2 vettori e li passo al comando AIC che ne 
# calcola un indice sintetico per ogni modello; minore è l'AIC migliore è il
# modello.

# R^2: Mi dice che frazione di variabilità dei dati è spiegata dal modello lineare
#      (0<R^2<1) e può essere utilizzato solo per confrontare modelli con lo stesso
#      numero di parametri
#      R^2 = 1 - (RSS/TSS)
# adj_R^2: E' simile al precedente ma ha un termine di correzione per tenere in conto
#          la complessità del modello (il numero dei suoi parametri).
#          Può essere utilizzato per confrontare modelli con numero di parametri diverso.
#          adj_R^2 = 1 - (RSS/TSS)*((n-1)/(n-k-1))
# AIC: E' un criterio basato sul concetto di entropia e mi indica quanta informazione
#      perdo utilizzando un determinato modello.
#      Tiene conto della sua complessità e può essere usato anche per modelli non annidati.
#      AIC=2*k - 2*log(L) --> k numero di parametri, L massimo della verosimiglianza

RsquaredAdj = c(
	summary(SimpleReg1)$adj.r.squared,
	summary(SimpleReg2)$adj.r.squared,
	summary(SimpleReg3)$adj.r.squared
	)
Rsquared = c(
	summary(SimpleReg1)$r.squared,
	summary(SimpleReg2)$r.squared,
	summary(SimpleReg3)$r.squared
	)


Aic = AIC(SimpleReg1,SimpleReg2,SimpleReg3)[,2] 
Aic
Rsquared
RsquaredAdj


# tutti e 3 gli indici preferiscono il primo modello
ModelChoice = data.frame(
	Mod=c("SimpleReg1","SimpleReg2","SimpleReg3"),
	RsquaredAdj=RsquaredAdj,
	Rsquared=Rsquared,
	Aic = Aic
	)

ModelChoice # Ho creato una matrice con i tre indici e noto che per tutti e 3
            # gli indicatori sintetici il primo modello è il migliore




##################################
#  ANALISI DEL MODELLO MIGLIORE  #
##################################

# Vediamo i risultati del primo modello
summary(SimpleReg1)
names(SimpleReg1)

# Intervalli di confidenza al 95%
# per farlo utilizzo il comando confint(modello, livello) arrotondato alla terza
# cifra decimale

paste(round(coef(SimpleReg1),3), " (95%CI: [",round(confint(SimpleReg1, level=0.95),3)[,1], " ; ",round(confint(SimpleReg1, level=0.95),3)[,2], "])", sep="")

# Stime e standard errors
paste(round(coef(SimpleReg1),3), " (SE: ",round(summary.lm(SimpleReg1)$coefficients[,2],3), ")",sep="")


# Plottiamo la linea di regressione
par(mfrow=c(nrows=1,ncols=1))
plot(x=TV, y=Sales, pch=16, col="red")
abline(SimpleReg1, col="blue", lwd=3)

# disegnamo i residui
segments(x0=TV,y0=Sales,x1=TV,y1=coef(SimpleReg1)[1] + coef(SimpleReg1)[2]*TV) # nel pratico sto disegnando i segmenti che
                                                                               # che collegano le osservazioni alla 
                                                                               # retta di regressione scritta con i
                                                                               # coefficienti


##############################
# VERIFICA DELLE IPOTESI DEL #
#     MODELLO LINEARE        #
##############################

res = SimpleReg1$residuals   # La regressione lineare ha come ipotesi alla base il fatto che la distribuzione della
                             # variabile risposta (osservazioni) sia normale e che i redisui siano anch'essi normali
                             # e omoschedastici ovvero con la stessa varianza


# Analisi dei residui

plot(TV,res, pch=16, main = "Residui")  # In questo caso notiamo che plottando i residui in funzione di TV che ci sono 2 particolarità:
                                        # 1) Per valori di TV bassi i resisui sembrano essere molto grandi
                                        # 2) per valori di TV crescenti i valori dei residui, pur avendo media nulla hanno varianza crescente
                                        # ovvero si potrebbe pensare che la varianza vari con TV
abline(h=0, col="green", lwd=2, lty=2)

# OSSERVAZIONI:
# 1) C'e' eteroschedasticita'. La varianza dei residui sembra crescere con TV.
# 2) C'e' una componente "curvilinea" residua: i residui corrispondenti a TV bassa 
#    sono tutti negativi
#
# CONSEGUENZA:
# Le ipotesi del modello lineare non sembrano essere soddisfatte



# Verifico la normalità dei residui

# Genera diversi plot per la diagnostica dei residui:
# - Residui VS Fittati
# - QQ Plot
# - |sqrt(residui)| VS fittati 
# - Residui VS Leverage

par(mfrow=c(2,2))
plot(SimpleReg1)

# oppure
shapiro.test(res) #  H0: i dati sono distribuiti normalmente


# Intervalli di confidenza dei parametri
confint(SimpleReg1)
Lower_bounds=confint(SimpleReg1)[,1]
Upper_bounds=confint(SimpleReg1)[,2]

# banda di previsione
par(mfrow=c(1,1))
plot(x=TV, y=Sales, pch=16, col="red", main="Banda di previsione")
abline(SimpleReg1, col="blue", lwd=3)
abline(Lower_bounds, col="blue", lwd=3, lty=2)
abline(Upper_bounds, col="blue", lwd=3, lty=2)



# 28 Novembre 2024

#######################
# MODELLO ALTERNATIVO #
#######################

# Proviamo a risolvere i problemi nei residui con una trasformazione
# attenzione: e' impossibile risolvere il problema dell'eteroschedasticita' con una trasformazione,
# ma possiamo provare ad eliminare, o alleviare, la componente curvilinea residua. L'intuizione nasce dal
# fatto che la forma delle osservazioni contro TV assomiglia a quella di una radice quadrata. 

# trasformazione
advertising$TV2 = TV^0.5  # sto aggiungendo la colonna TV2,  radice di TV, al dataset e vediamo se ottengo una 
                          # regressione lineare in questa nuova variabile
attach(advertising) # devo rifare l'attach perchè ho cambiato una variabile


pairs(advertising[,2:6], upper.panel=panel.cor,diag.panel=panel.hist)
scatter.smooth(x=TV2, y=Sales, pch=16, col="red")

SimpleReg4 = lm(Sales~TV2)

# Scelta del modello
RsquaredAdj = c(
	summary(SimpleReg1)$adj.r.squared,
	summary(SimpleReg2)$adj.r.squared,
	summary(SimpleReg3)$adj.r.squared,
	summary(SimpleReg4)$adj.r.squared
	)
Rsquared = c(
	summary(SimpleReg1)$r.squared,
	summary(SimpleReg2)$r.squared,
	summary(SimpleReg3)$r.squared,
	summary(SimpleReg4)$r.squared
	)
Aic = AIC(SimpleReg1,SimpleReg2,SimpleReg3,SimpleReg4)[,2]
Aic

# OSS: Il quarto modello (con TV2 anzichè TV sembra essere il migliore perchè ha
#                         l'AIC più basso.)

# Vediamo i risultati
summary(SimpleReg4)


# Plottiamo la linea di regressione
par(mfrow=c(nrows=1,ncols=1))
plot(x=TV2, y=Sales, pch=16, col="red", main="Sales VS TV2") # fate attenzione: usare TV2
abline(SimpleReg4, col="blue", lwd=3)

# plot the least squares lines
segments(x0=TV2,y0=Sales,x1=TV2,y1=coef(SimpleReg4)[1] + coef(SimpleReg4)[2]*TV2)

## Verifica delle ipotesi alla base del modello
res = SimpleReg4$residuals

# plottiamo i residui
plot(TV2,res, pch=16, main="Residui")
abline(h=0, col="green", lty=2, lwd=2)
# C'e' ancora eteroschedasticita'. Proviamo a risolverla utilizzando
# piu' variabili con una regressione multivariata (prossimo script)


# Verifichiamo la normalita' dei residui
par(mfrow=c(2,2))
plot(SimpleReg4)

#oppure
shapiro.test(res)


# Intervalli di confidenza dei parametri
confint(SimpleReg4)
Lower_bounds=confint(SimpleReg4)[,1]
Upper_bounds=confint(SimpleReg4)[,2]

# banda di previsione
par(mfrow=c(1,1))
plot(x=TV2, y=Sales, pch=16, col="red", main="Banda di previsione")
abline(SimpleReg4, col="blue", lwd=3)
abline(Lower_bounds, col="blue", lwd=3, lty=2)
abline(Upper_bounds, col="blue", lwd=3, lty=2)

# Intervallo di Confidenza CI: Mi indica un'intervallo con copertura al 95% per E[Y|X=x]
# Intervallo di Previsione PI: Mi indica un'intervallo con copertura al 95% per Y|X=x
predict(SimpleReg4,data.frame(TV2=(c(0,150,300)^0.5)), interval="confidence")
predict(SimpleReg4,data.frame(TV2=(c(0,150,300)^0.5)), interval="prediction")



# -----------------------------------------
#   Misure di influenza -  Quarto Modello
# -----------------------------------------

# L'idea e' quella di eliminare un'osservazione e vedere come cambia il modello per trovare, se ci sono
# dei dati che influenzano troppo il modello e che magari sono da togliere  se sono derivanti da errori di
# misura (o altro tipo)

meas.inf = influence.measures(SimpleReg4)$infmat

# colnames(meas.inf)
#"dfb.1_"  		variazioni nel valore dell'intercetta
#"dfb.TV2" 		variazioni nel valore del coefficiente regressivo
#"dffit"		  variazioni nella previsione i-esima
#"cov.r"			variazioni nel rapporto Cov(X,Y)/Var(X)
#"cook.d"			variazione nella previsione generale
#"hat"				matrice dll'i-esimo elemento della matrice H = X((X'X)^(-1))X' 
#             

par(mfrow=c(2,3))
plot(meas.inf[,"dfb.1_"], pch=16, col="red", cex=0.8, ylab="Variazione intercetta")
plot(meas.inf[,"dfb.TV2"], pch=16, col="red", cex=0.8, ylab="Variazione TV2")
plot(meas.inf[,"dffit"], pch=16, col="red", cex=0.8, ylab="Variazione Fit i-esimo")
plot(meas.inf[,"cov.r"], pch=16, col="red", cex=0.8, ylab="Variazione Cov Ratio")
plot(meas.inf[,"cook.d"], pch=16, col="red", cex=0.8, ylab="Variazione Fit generale")
plot(meas.inf[,"hat"], pch=16, col="red", cex=0.8, ylab="Variazione Leverage")

# OSSERVAZIONE: non sembrano esserci osservazioni particolarmente influenti

## possiamo trovare le osservazioni con valori "estremi" tramite
summary(influence.measures(SimpleReg4))

# OSSERVAZIONE: ... anche se 131 e 156 vengono segnalate con *, il che 
#                   significa che su alcune misure di influenza sono
#                   forse problematiche.



