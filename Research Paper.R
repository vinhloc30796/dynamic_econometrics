#----------------------#
# Dynamic Econometrics #
#    Research Paper    #
#----------------------#

library('readxl') # To read Excel files
library('urca') # For the Dickey Fuller test
library('fpp2') # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm') # To estimate ARDL models
library('GetoptLong') # For pretty printing
library('ggplot2') # For plotting
library('ggfortify') 
library(S4Vectors)


# Set seed to ensure reproducibility
set.seed(1)
# Kris: setting working directory - uncomment as needed
# setwd("C:/Users/Kris Rama/Desktop/Econometrics/Dynamic Econometrics/Research Paper//")
# Loc: setting working directory - uncomment as needed
# setwd("C:/Users/vinhl/OneDrive/[Work] Translate and Class/[RUG] Dynamic Econometrics/dynamic_econometrics")
data <- read_excel("FREDMD_march2019.xlsx") #import excel

#save columns as time series 
indpro <- ts(data[,'INDPRO'], start = c(1959), end = c(2006,12), frequency = 12)
t10yffm <- ts(data[,'T10YFFM'], start = c(1959), end = c(2006,12), frequency = 12)

numlags <- floor(12*((length(indpro)/100)^0.25)) #max lag lenght


#--------#
# indpro #
#--------#

autoplot(indpro)

#d. transformation
d.indpro <- diff(indpro)
autoplot(d.indpro)


layout(1:2); acf(d.indpro); pacf(d.indpro)#acf and pacf of d.indpro
##d.log transformation seems preferable


#d.log transformation
d.ln.indpro <- 100*diff(log(indpro))
autoplot(d.ln.indpro)

layout(1:2); acf(d.ln.indpro); pacf(d.ln.indpro) #acf and pacf of d.ln.indpro
##suggests AR(p) with p = 1,2,3


#stationarity, Augmented Dickey Fuller test
adf_d.ln.indpro <- ur.df(d.ln.indpro, lags = numlags, selectlags = "Fixed")
summary(adf_d.ln.indpro)@teststat #test stat
summary(adf_d.ln.indpro)@cval #critical values
##stationary


#modeling d.ln.indpro
aic_d.ln.indpro <- matrix(NA,5,5)
colnames(aic_d.ln.indpro) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(aic_d.ln.indpro) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

bic_d.ln.indpro <- matrix(NA,5,5)
colnames(bic_d.ln.indpro) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(bic_d.ln.indpro) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

t_est <- matrix(NA,9,9)

for (i in 0:4){
  for (j in 0:4){
    fit <- Arima(d.ln.indpro, order = c(i,0,j))
    t_est[i+1,j+1] <- length(fit$residuals)
    aic_d.ln.indpro[i+1,j+1] <- fit$aic
    bic_d.ln.indpro[i+1,j+1] <- fit$bic
  }
}

t_est

#scores
aic_d.ln.indpro
bic_d.ln.indpro
##both seems to suggest AR(3) for diff(INDPRO) 
##note: ARMA(i, j) scores will be stored in position [i+1, j+1]


d.ln.indpro.arma1 <- Arima(d.ln.indpro, order = c(1,0,0))
checkresiduals(d.ln.indpro.arma1)

d.ln.indpro.arma2 <- Arima(d.ln.indpro, order = c(2,0,0))
checkresiduals(d.ln.indpro.arma2)

d.ln.indpro.arma3 <- Arima(d.ln.indpro, order = c(3,0,0))
checkresiduals(d.ln.indpro.arma3)

#AR(3) coefficients
d.ln.indpro.arma3

#predictions


#--------#
# t10ffm #
#--------#

autoplot(t10yffm)

#stationarity, Augmented Dickey Fuller test
adf_t10yffm <- ur.df(t10yffm, lags = numlags, selectlags = "Fixed")
summary(adf_t10yffm)@teststat #test stat
summary(adf_t10yffm)@cval #critical values
##stationary

layout(1:2); acf(t10yffm); pacf(t10yffm)#acf and pacf of t10yffm
##MA part to be checked, otherwise AR(3)


#modeling t10yffm
aic_t10yffm <- matrix(NA,5,5)
colnames(aic_t10yffm) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(aic_t10yffm) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

bic_t10yffm <- matrix(NA,5,5)
colnames(bic_t10yffm) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(bic_t10yffm) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

t_est <- matrix(NA,9,9)


for (i in 0:4){
  for (j in 0:4){
    fit <- Arima(t10yffm, order = c(i,0,j))
    t_est[i+1,j+1] <- length(fit$residuals)
    aic_t10yffm[i+1,j+1] <- fit$aic
    bic_t10yffm[i+1,j+1] <- fit$bic
  }
}

t_est

#scores
aic_t10yffm
bic_t10yffm
##ARMA(1,1) and AR(3) to be checked
##note: ARMA(i, j) scores will be stored in position [i+1, j+1]


t10yffm.arma1.1 <- Arima(t10yffm, order = c(1,0,1))
checkresiduals(t10yffm.arma1.1)

t10yffm.arma3 <- Arima(t10yffm, order = c(3,0,0))
checkresiduals(t10yffm.arma3)

#AR(3)coefficients
t10yffm.arma3'fpp2') # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm') # To estimate ARDL models
library('GetoptLong') # For pretty printing
library('ggplot2') # For plotting
library('ggfortify') 
library(S4Vectors)


# Set seed to ensure reproducibility
set.seed(1)
# Kris: setting working directory - uncomment as needed
# setwd("C:/Users/Kris Rama/Desktop/Econometrics/Dynamic Econometrics/Research Paper//")
# Loc: setting working directory - uncomment as needed
# setwd("C:/Users/vinhl/OneDrive/[Work] Translate and Class/[RUG] Dynamic Econometrics/dynamic_econometrics")
data <- read_excel("FREDMD_march2019.xlsx") #import excel

#save columns as time series 
indpro <- ts(data[,'INDPRO'], start = c(1959), end = c(2006,12), frequency = 12)
t10yffm <- ts(data[,'T10YFFM'], start = c(1959), end = c(2006,12), frequency = 12)

numlags <- floor(12*((length(indpro)/100)^0.25)) #max lag lenght


#--------#
# indpro #
#--------#

autoplot(indpro)

#d. transformation
d.indpro <- diff(indpro)
autoplot(d.indpro)


layout(1:2); acf(d.indpro); pacf(d.indpro)#acf and pacf of d.indpro
##d.log transformation seems preferable


#d.log transformation
d.ln.indpro <- 100*diff(log(indpro))
autoplot(d.ln.indpro)

layout(1:2); acf(d.ln.indpro); pacf(d.ln.indpro) #acf and pacf of d.ln.indpro
##suggests AR(p) with p = 1,2,3


#stationarity, Augmented Dickey Fuller test
adf_d.ln.indpro <- ur.df(d.ln.indpro, lags = numlags, selectlags = "Fixed")
summary(adf_d.ln.indpro)@teststat #test stat
summary(adf_d.ln.indpro)@cval #critical values
##stationary


#modeling d.ln.indpro
aic_d.ln.indpro <- matrix(NA,5,5)
colnames(aic_d.ln.indpro) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(aic_d.ln.indpro) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

bic_d.ln.indpro <- matrix(NA,5,5)
colnames(bic_d.ln.indpro) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(bic_d.ln.indpro) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

t_est <- matrix(NA,9,9)

for (i in 0:4){
  for (j in 0:4){
    fit <- Arima(d.ln.indpro, order = c(i,0,j))
    t_est[i+1,j+1] <- length(fit$residuals)
    aic_d.ln.indpro[i+1,j+1] <- fit$aic
    bic_d.ln.indpro[i+1,j+1] <- fit$bic
  }
}

t_est

#scores
aic_d.ln.indpro
bic_d.ln.indpro
##both seems to suggest AR(3) for diff(INDPRO) 
##note: ARMA(i, j) scores will be stored in position [i+1, j+1]


d.ln.indpro.arma1 <- Arima(d.ln.indpro, order = c(1,0,0))
checkresiduals(d.ln.indpro.arma1)

d.ln.indpro.arma2 <- Arima(d.ln.indpro, order = c(2,0,0))
checkresiduals(d.ln.indpro.arma2)

d.ln.indpro.arma3 <- Arima(d.ln.indpro, order = c(3,0,0))
checkresiduals(d.ln.indpro.arma3)

#AR(3) coefficients
d.ln.indpro.arma3

#predictions


#--------#
# t10ffm #
#--------#

autoplot(t10yffm)

#stationarity, Augmented Dickey Fuller test
adf_t10yffm <- ur.df(t10yffm, lags = numlags, selectlags = "Fixed")
summary(adf_t10yffm)@teststat #test stat
summary(adf_t10yffm)@cval #critical values
##stationary

layout(1:2); acf(t10yffm); pacf(t10yffm)#acf and pacf of t10yffm
##MA part to be checked, otherwise AR(3)


#modeling t10yffm
aic_t10yffm <- matrix(NA,5,5)
colnames(aic_t10yffm) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(aic_t10yffm) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

bic_t10yffm <- matrix(NA,5,5)
colnames(bic_t10yffm) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)")
rownames(bic_t10yffm) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)")

t_est <- matrix(NA,9,9)


for (i in 0:4){
  for (j in 0:4){
    fit <- Arima(t10yffm, order = c(i,0,j))
    t_est[i+1,j+1] <- length(fit$residuals)
    aic_t10yffm[i+1,j+1] <- fit$aic
    bic_t10yffm[i+1,j+1] <- fit$bic
  }
}

t_est

#scores
aic_t10yffm
bic_t10yffm
##ARMA(1,1) and AR(3) to be checked
##note: ARMA(i, j) scores will be stored in position [i+1, j+1]


t10yffm.arma1.1 <- Arima(t10yffm, order = c(1,0,1))
checkresiduals(t10yffm.arma1.1)

t10yffm.arma3 <- Arima(t10yffm, order = c(3,0,0))
checkresiduals(t10yffm.arma3)

#AR(3)coefficients
t10yffm.arma3