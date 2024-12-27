# ---------------------------------------
#      GOODNESS OF FIT (GOF) TEST
# ---------------------------------------

# Esempio tratto da:
# Agresti, A. (2007). An introduction to categorical data analysis (2nd ed).
# Hoboken, NJ: Wiley-Interscience, p. 39

# Supponiamo di campionare N unità statistiche da una popolazione e di ossrvare
# per ciascuna di esse le seguenti variabili categoriche:
#
# SESSO: M, F
# PARTITO: DEM, IND, REP
#
# Eseguire un test statistico per valutare l'omogeneità del sesso rispetto all'
# adesione ai vari partiti politici.
#
#
# H0: P(sesso|partito=i) costante per ogni i
# HA: P(sesso|partito=i) != P(sesso|partito=j) per almeno una coppia (i,j)



# costruiamo la tabella
M <- as.table(rbind(c(484, 762), c(239, 327), c(477, 468)))
dimnames(M) <- list(Partito = c("Democr.", "Ind.", "Rep."),
                    Genere = c("Uomini", "Donne"))

print(M)


#################
# TEST "A MANO" #
#################

tot <- sum(M)
tot.part <- c()

tot.part[1] <- sum(M[1,])
tot.part[2] <- sum(M[2,])
tot.part[3] <- sum(M[3,])


pi0 <- as.table(matrix(c(rep(sum(M[,1])/tot,3), rep(sum(M[,2])/tot, 3)), nrow=3))
dimnames(pi0) <- list(Partito = c("Democr.", "Ind.", "Rep."),
                    Genere = c("Uomini", "Donne"))
print(pi0)

# Statistica chi-quadro di Pearson
X2 <- sum(((M-pi0*tot.part)^2)/(pi0*tot.part))

# P-value
p.val <- pchisq(X2, 2, lower.tail = FALSE)



#######################
# TEST CON FUNZIONE R #
#######################
res <- chisq.test(M)



