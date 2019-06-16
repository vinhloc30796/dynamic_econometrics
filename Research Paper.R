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
library('vars')
#library(S4Vectors)
library(reshape2) #ranking
library(xtable) #latex output


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

numlags <- floor(12*((length(indpro)/100)^0.25)) #max lag length

plot.ts(cbind(indpro,t10yffm), ylab=c("INDPRO", "T10YFFM"))


#---------#
# INDPRO  #
#---------#

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

#check for sample size
t_est <- matrix(NA,5,5)

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
##both seems to suggest AR(3) for diff(log INDPRO) 
##note: ARMA(i, j) scores will be stored in position [i+1, j+1]

#ranking aic
aic_d.ln.indpro.ranked <- data.frame(rows=rownames(aic_d.ln.indpro), cols=colnames(aic_d.ln.indpro), stack(as.data.frame(aic_d.ln.indpro)))[, c("rows", "ind", "values")]
colnames(aic_d.ln.indpro.ranked) <-  c("AR", "MA", "AIC")
aic_d.ln.indpro.ranked <- na.omit(aic_d.ln.indpro.ranked[order(aic_d.ln.indpro.ranked$AIC),][complete.cases(aic_d.ln.indpro.ranked),])

#ranking bic
bic_d.ln.indpro.ranked <- data.frame(rows=rownames(bic_d.ln.indpro), cols=colnames(bic_d.ln.indpro), stack(as.data.frame(bic_d.ln.indpro)))[, c("rows", "ind", "values")]
colnames(bic_d.ln.indpro.ranked) <-  c("AR", "MA", "BIC")
bic_d.ln.indpro.ranked <- na.omit(bic_d.ln.indpro.ranked[order(bic_d.ln.indpro.ranked$BIC),][complete.cases(bic_d.ln.indpro.ranked),])

#latex tables
print(
  xtable(aic_d.ln.indpro.ranked[1:9,], caption = 'AIC scores for different ARMA models when fitting on diff(ln indpro)', digits = 1), 
  file="aic_d.ln.indpro.txt", 
  include.rownames = FALSE, include.colnames = TRUE, 
  hline.after = c(0), add.to.row = list(pos = list(-1,0), command = c("\\multicolumn{2}{c}{AIC} \\\\\n",""))
)
print(
  xtable(bic_d.ln.indpro.ranked[1:9,], caption = 'BIC scores for different ARMA models when fitting on diff(ln indpro)', digits = 1), 
  file="bic_d.ln.indpro.txt", 
  include.rownames = FALSE, include.colnames = TRUE, 
  hline.after = c(0), add.to.row = list(pos = list(-1,0), command = c("\\multicolumn{2}{c}{BIC} \\\\\n",""))
)

#residuals
d.ln.indpro.arma1 <- Arima(d.ln.indpro, order = c(1,0,0))
checkresiduals(d.ln.indpro.arma1)

d.ln.indpro.arma2 <- Arima(d.ln.indpro, order = c(2,0,0))
checkresiduals(d.ln.indpro.arma2)

d.ln.indpro.arma3 <- Arima(d.ln.indpro, order = c(3,0,0))
checkresiduals(d.ln.indpro.arma3)

#AR(3) coefficients
d.ln.indpro.arma3

#predictions
train_test_split <- floor(length(d.ln.indpro) *0.66)
train <- d.ln.indpro[0:train_test_split]
test <- d.ln.indpro[train_test_split:length(d.ln.indpro)]
model <- Arima(train, order=c(3, 0, 0))
predictions <- predict(model, 1)$pred
history <- append(train, test[1])
for (t in c(2:length(test))) {
  model <- Arima(history, order=c(3, 0, 0))
  predictions <- append(predictions, predict(model, 1)$pred)
  history <- append(history, test[t])
}

length(predictions)
length(test)
mean((predictions - test)^2)
layout(1:2); ts.plot(predictions); ts.plot(test)

#---------#
# T10YFFM #
#---------#

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

#check for sample size
t_est <- matrix(NA,5,5)


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

#ranking aic
aic_t10yffm.ranked <- data.frame(rows=rownames(aic_t10yffm), cols=colnames(aic_t10yffm), stack(as.data.frame(aic_t10yffm)))[, c("rows", "ind", "values")]
colnames(aic_t10yffm.ranked) <-  c("AR", "MA", "AIC")
aic_t10yffm.ranked <- na.omit(aic_t10yffm.ranked[order(aic_t10yffm.ranked$AIC),][complete.cases(aic_t10yffm.ranked),])

#ranking bic
bic_t10yffm.ranked <- data.frame(rows=rownames(bic_t10yffm), cols=colnames(bic_t10yffm), stack(as.data.frame(bic_t10yffm)))[, c("rows", "ind", "values")]
colnames(bic_t10yffm.ranked) <-  c("AR", "MA", "BIC")
bic_t10yffm.ranked <- na.omit(bic_t10yffm.ranked[order(bic_t10yffm.ranked$BIC),][complete.cases(bic_t10yffm.ranked),])

#latex tables
print(
  xtable(aic_t10yffm.ranked[1:9,], caption = 'AIC scores for different ARMA models when fitting on T10YFFM', digits = 1), 
  file="aic_t10yffm.txt", 
  include.rownames = FALSE, include.colnames = TRUE, 
  hline.after = c(0), add.to.row = list(pos = list(-1,0), command = c("\\multicolumn{2}{c}{AIC} \\\\\n",""))
)
print(
  xtable(bic_t10yffm.ranked[1:9,], caption = 'BIC scores for different ARMA models when fitting on T10YFFM', digits = 1), 
  file="bic_t10yffm.txt", 
  include.rownames = FALSE, include.colnames = TRUE, 
  hline.after = c(0), add.to.row = list(pos = list(-1,0), command = c("\\multicolumn{2}{c}{BIC} \\\\\n",""))
)

#residuals
t10yffm.arma1.1 <- Arima(t10yffm, order = c(1,0,1))
checkresiduals(t10yffm.arma1.1)

t10yffm.arma3 <- Arima(t10yffm, order = c(3,0,0))
checkresiduals(t10yffm.arma3)

#AR(3)coefficients
t10yffm.arma3

get_mse_from_prediction <- function(dataset, model, dataset_split=0.66){
  train_test_split <- floor(length(dataset) *0.66)
  train <- dataset[0:train_test_split]
  test <- dataset[train_test_split:length(t10yffm)]
  model <- Arima(train, order=c(3, 0, 0))
  predictions <- predict(model, 1)$pred
  history <- append(train, test[1])
  for (t in c(2:length(test))) {
    model <- Arima(history, order=c(3, 0, 0))
    predictions <- append(predictions, predict(model, 1)$pred)
    history <- append(history, test[t])
  }
  
  length(predictions)
  length(test)
  mean((predictions - test)^2)
}

#predictions
train_test_split <- floor(length(t10yffm) *0.66)
train <- t10yffm[0:train_test_split]
test <- t10yffm[train_test_split:length(t10yffm)]
model <- Arima(train, order=c(3, 0, 0))
predictions <- predict(model, 1)$pred
history <- append(train, test[1])
for (t in c(2:length(test))) {
  model <- Arima(history, order=c(3, 0, 0))
  predictions <- append(predictions, predict(model, 1)$pred)
  history <- append(history, test[t])
}

length(predictions)
length(test)
mean((predictions - test)^2)

#--------#
#  ARDL  #
#--------#

aic_ardl <- matrix(NA, nrow=4, ncol=4)
colnames(aic_ardl) <- c("T10YFFM(1)", "T10YFFM(2)", "T10YFFM(3)", "T10YFFM(4)")
rownames(aic_ardl) <- c("INDPRO(1)", "INDPRO(2)", "INDPRO(3)", "INDPRO(4)")

bic_ardl <- matrix(NA, nrow=4, ncol=4)
colnames(bic_ardl) <- c("T10YFFM(1)", "T10YFFM(2)", "T10YFFM(3)", "T10YFFM(4)")
rownames(bic_ardl) <- c("INDPRO(1)", "INDPRO(2)", "INDPRO(3)", "INDPRO(4)")

for (i in seq(1,4)) {
  for (j in seq(1,4)) {
    print(paste('Estimating AR(', i,',', j,')'))
    #Include all lags 
    #from 1 to i for diff(log INDPRO) 
    #from 1 to j for T10YFFM
    ardl <- dynlm(d.ln.indpro ~ L(d.ln.indpro, (1:i)) + L(t10yffm, (1:j)))
    aic_ardl[i,j] <- AIC(ardl)
    bic_ardl[i,j] <- BIC(ardl)
  }
}

aic_ardl
#AIC suggests ARDL(1,4) for INDPRO ~ T10YFFM
bic_ardl
#BIC suggests ARDL(1,4) for INDPRO ~ T10YFFM, too


#ranking aic
data.frame(rows=rownames(aic_ardl), cols=colnames(aic_ardl), stack(as.data.frame(aic_ardl)))
aic_ardl <- data.frame(as.table(aic_ardl))
aic_ardl <- aic_ardl[order(aic_ardl$Freq),]


#ranking bic
data.frame(rows=rownames(bic_ardl), cols=colnames(bic_ardl), stack(as.data.frame(bic_ardl)))
bic_ardl <- data.frame(as.table(bic_ardl))
bic_ardl <- bic_ardl[order(bic_ardl$Freq),]


#latex tables
aic_ardl$x <- paste(aic_ardl$Var1, aic_ardl$Var2)
aic_ardl <- xtable(aic_ardl[1:9, c(4,3)],  caption = 'AIC scores for ARDL', digits = 1)
print(aic_ardl, file="aic_ardl.txt", include.rownames = FALSE, include.colnames = FALSE, hline.after = c(0), add.to.row = list(pos = list(-1,0), command = c("\\multicolumn{2}{c}{AIC} \\\\\n","")))
#omit file="aic_d.ln.indpro.txt" to print in console

bic_ardl$x <- paste(bic_ardl$Var1, bic_ardl$Var2)
bic_ardl <- xtable(bic_ardl[1:9, c(4,3)],  caption = 'BIC scores for ARDL', digits = 1)
print(bic_ardl, file="bic_ardl.txt", include.rownames = FALSE, include.colnames = FALSE, hline.after = c(0), add.to.row = list(pos = list(-1,0), command = c("\\multicolumn{2}{c}{BIC} \\\\\n","")))



##ARDL(1,4) and ARDL(3,1) to be checked
d.ln.indpro.ardl1.4 <- dynlm(d.ln.indpro ~ L(d.ln.indpro, (1:1)) + L(t10yffm, (1:4)))
checkresiduals(d.ln.indpro.ardl1.4)

d.ln.indpro.ardl3.1 <- dynlm(d.ln.indpro ~ L(d.ln.indpro, (1:3)) + L(t10yffm, (1:1)))
checkresiduals(d.ln.indpro.ardl3.1)
#ARDL(3,1) is slightly less significant than ARDL(1,4)

#--------#
# varma  #
#--------#

ardl4 <- dynlm(d.ln.indpro ~ L(d.ln.indpro, (1:4)) + L(t10yffm, (1:4)))
summ <- summary(ardl4)
print(summ$coefficients, digits = 1)

y <- cbind(d.ln.indpro, t10yffm)
y <- y[-c(1), ]
var <- VAR(y, p = 4, type = c("const"))
corder1 <- order(names(var$varresult$d.ln.indpro$coefficients))
corder2 <- order(names(summ$coefficients[,1]))
coefvar <- cbind(var$varresult$d.ln.indpro$coefficients[corder1], summ$coefficients[corder2])
colnames(coefvar) <- c("VAR(4)", "ARDL(4,4)")
print(coefvar, digits = 1)

#impulse response 
irf <- irf(var, impulse = c("t10yffm"), response = c("d.ln.indpro"), ortho = FALSE)
plot(irf, plot.type = c("single"))

irf <- irf(var, impulse = c("t10yffm"), response = c("d.ln.indpro"), ortho = TRUE)
plot(irf, plot.type = c("single"))
## orthogonalizing not influencial 

#lag order selection
var_ic <- VARselect(y, type = c("const"), lag.max = 8)
ic <- as.data.frame(t(var_ic$criteria))
ggplot(data = ic, aes(x = seq(1,8), y = ic$`SC(n)`)) + geom_line() + ylab("BIC")
