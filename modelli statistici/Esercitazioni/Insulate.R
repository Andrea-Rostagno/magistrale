# -----------------------------------------------------
#     Linear Regression with categorical variables
#     Date: 05/12/2024
#     Course:  Modelli Statistici
# -----------------------------------------------------


### Description of the data
# We want to evaluate the effect of thermic isolation with respect to 
# gas consumption. The file "191001insulate-data.txt" is a data frame containing
# measurements, taken before and after the thermic isolation of a building, of
#
# 1) Mean external temperature (in °C - The thermostat has been set to 20°C),
# 2) Gas consumption (feet^3).
#
# Measurements were taken for 26 weeks before the installation of the thermic
# isolation system and for 30 weeks after it.
# How can we study the connection between external temperature and gas consumption,
# by taking into account the effect of thermic isolation?

### Clean the environment
rm(list = ls())

### Set the directory
setwd("/Users/marcoratta/Desktop/PhD/Teaching/Modelli Statistici/Materiale R/Insulate")

### Import the data
# We use the function read.table for ".txt" files.
# The argument "col.names" assigns names to the columns of the dataset.
insulate <- read.table("211102insulate-data.txt", col.names = c("when", "temp", "cons"))

# Look to your data
str(insulate)
head(insulate)

# Use the variables
insulate[,2]
insulate$temp

# To write just "temp" instead of "insulate$temp" we have to attach the dataset
attach(insulate)

temp
cons

##################################
# Relation between temp and cons #
##################################
# First of all we plot cons vs temp, without considering the variable "when"
plot(temp, cons, pch = 16, col="red", main = "Tempereatura VS Consumo",
     xlab="Temperatura (°C)", ylab="Consumo (feet^3)")

# It is evident a decreasing tendency of gas consumption with respect to temp:
# this is natural, if the external temperature rises up, you may not want to 
# use the heating system... This tendency is also described by the correlation
# between the two variables
cor(temp, cons) 

### Highlight the effect of thermic isolation
plot(temp, cons, type = "n", xlab = "Temperatura (°C)",
     ylab="Consumo (feet^3)") #type = "n" means empty plot
# We select the rows with "when" is equal to "before"
points(temp[when=="before"], cons[when=="before"], pch=16, col="blue") #function "points" plots points over an existing plot
# Same for the submatrix "after"
points(temp[when=="after"], cons[when=="after"], pch=17, col="green")
# Add a legend and a title to the graph
legend(x=6,y=7,c("before", "after"), col = c("blue","green"), pch=c(16,17), y.intersp = 0.8)
# This plots a legend in a rectangle with left side corresponding to x = 9 and top side
# corresponding to y = 7, bty = "n" does not plot the box and y.intersp regulates the space
# between the entries
title("Scatter plot - insulate data - before-after isolation")

# REMARK: The relation between temp and cons is decreasing, but the consumption levels
#         are lower, if we fix a temperature value: thermic isolation has an effect.




############################ 
# Simple Linear Regression #
############################

# lm(y ~ x), where y is the dependent variable, x is the explicative variable

# We perform two linear regressions at first, only for a didactic purpose,
# one by considering only data before thermic isolation, one by considering
# only data after it.

temp_before <- temp[when=="before"]
cons_before <- cons[when=="before"]

# Model before: 
regr_before <- lm(cons_before ~ temp_before)
summary(regr_before)


temp_after <- temp[when=="after"]
cons_after <- cons[when=="after"]

# Model after:
regr_after <- lm(cons_after ~ temp_after)
summary(regr_after)


# Plot of the two regression lines over the scatter plot we did before.
# If you type str(regr_before) you see that it is a list, so we can
# consider the vector of the regression coefficients with "regr_before$coef"
abline(regr_before$coef, col="blue", lty=2, lwd=1.5) 
abline(regr_after$coef, col = "green", lty=2, lwd=1.5)

# Fitted values, i.e. values on the regression line
regr_before$fitted
regr_after$fitted


# Residuals
# To have a good fitting, residuals should be centered around zero, otherwise
# there could be some systematic errors and the regression line could over/underestimate
# the data. Moreover, residuals should not show any particular pattern: a pattern
# would indicate that a linear approximation is not adequate and that a polynomial regression
# should be preferred.

res_before <- regr_before$resid
res_after <- regr_after$resid

# Plot two graphs in one window
par(mfrow = c(1,2)) #1 row, 2 columns
#
plot(temp_before, res_before, pch=16, col="red",
     xlab="Temperatura (°C)", ylab="Residui", main="Before")
abline(h=0, lty=2, lwd=1.5)
#
plot(temp_after, res_after, pch=16, col="red",
     xlab="Temperatura (°C)", ylab="Residui", main="After")
abline(h=0, lty=2, lwd=1.5)

# Plot an histogram of the residuals
hist(res_before, xlab="Residui", ylab="Frequenze", main="Before")
hist(res_after, xlab="Residui", ylab="Frequenze", main = "After")



par(mfrow=c(1,1)) #back to one graph per row

##############################
# Multiple linear regression #
##############################

# Until now we performed two simple linear regressions, by decomposing the data
# into two subsets. However, it is better to perform a multiple linear regression
# with two variables, one of them being a categorical one.


# Case 1 - No interaction when-temp 
regr <- lm(cons~when+temp)
summary(regr)

par(mfrow=c(1,1))
plot(temp, cons, type='n', xlab="Temperatura (°C)", ylab="Consumo (feet^3)")
points(temp[when=="before"],cons[when=="before"],pch=16, col=1)
points(temp[when=="after"], cons[when=="after"], pch=17, col=2)
abline(a=regr$coef[1]+regr$coef[2], b=regr$coef[3], lwd=3)
abline(a=regr$coef[1], b=regr$coef[3], col=2, lwd=3)
legend(x=7,y=7, c("before", "after"), col = c(1,2), pch=c(16,17), y.intersp = 0.8)
title("Scatter plot - insulate data - no interactions")

# Residuals
par(mfrow=c(1,3))
plot(temp, regr$resid, col="red", pch=16, main="Scatter Residui")
abline(h=0, lty=2)
hist(regr$resid, xlab="Residui", ylab="Frequenze", 
     main="Istogramma Residui")
plot(density(regr$resid), xlab="Residui", ylab="PDF",
     main="Densità Residui")


# Case 2 - With interactions between when and temp: does the slope of the regression
# line (i.e. the speed of the decrease of gas consumption) depend on the presence
# of the isolation? To do this, we introduce a new variable (i.e. the interaction term)
# and see if it contributes significantly to the model.

regr2 <- lm(cons~when*temp) 
summary(regr2)


par(mfrow=c(1,1))
plot(temp,cons,type="n")
points(temp[when=="before"],cons[when=="before"],pch=16, col=1)
points(temp[when=="after"], cons[when=="after"], pch=17, col=2)
title("Scatter plot - insulate data - with interactions")
abline(a=regr2$coef[1]+regr2$coef[2], b=regr2$coef[3]+regr2$coef[4], lwd=3)
abline(a=regr2$coef[1], b=regr2$coef[3], col=2, lwd=3)
legend(x=7,y=7, c("before", "after"), col = c(1,2), pch=c(16,17), y.intersp = 0.8)

# Residuals
par(mfrow=c(1,3))
plot(temp, regr2$resid, col="red", pch=16, main="Scatter Residui")
abline(h=0, lty=2)
hist(regr2$resid, xlab="Residui", ylab="Frequenze", 
     main="Istogramma Residui")
plot(density(regr2$resid), xlab="Residui", ylab="PDF",
     main="Densità Residui")



### Compare our models
anova(lm(cons~1), regr2)
anova(regr, regr2)

# If you do not have to use your dataset anymore, detach it
detach(insulate)





## ESERICIZI - foglio REGIONI  DI CONFIDENZA E PREVISIONE  ##
## consideriamo la regressione additiva lm(cons~when+temp) ##
summary(regr)

# 1 - Calcolare un intervallo di confidenza di livello 95% (il default) per il coefficiente di whenbefore


# 2 - calcolare due intervalli di confidenza di livello 99% per il valore atteso del consumo in corrispondenza di
#     when='before'  e   temp=3.2
#     when='after'   e   temp=3.2


# 3 - calcolare due intervalli di predizione di livello 99% per i valore at-tesi del consumo in corrispondenza di
#     when='before'  e   temp=3.2
#     when='after'   e   temp=3.2











