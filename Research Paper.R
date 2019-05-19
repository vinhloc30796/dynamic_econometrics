library('readxl') # To read Excel files
library('urca') # For the Dickey Fuller test
library('fpp2') # For forecasting
library('tseries') # To estimate ARMA models
library('dynlm') # To estimate ARDL models
library('GetoptLong') # For pretty printing

# Set seed to ensure reproducibility
set.seed(1)

# Import Excel data
## Replace with your directory
fred <- read_excel("C:/Users/vinhl/OneDrive/[Work] Translate and Class/[RUG] Dynamic Econometrics/FREDMD_march2019.xlsx")

# Save two time series as variables for convenience
indpro <- ts(fred[,'INDPRO'], start = c(1959), end = c(2007), frequency=12)
t10yield <- ts(fred[,'T10YFFM'], start = c(1959), end = c(2007), frequency=12)

### Section 1: Exploratory data analysis i.e. plotting, ACF, PACF

# Plot two time series of interest
# plot.ts(ts(fred[c("INDPRO", "T10YFFM")], start=c(1959), end=c(2008), frequency=12)) # Can also use this to plot
plot.ts(cbind(indpro, t10yield), main = "Treasury Yield and Industrial Production: 1959-2007")

# Plot the ACF and PACF plots for INDPRO
layout(1:2); acf(indpro, lag.max=60)$acf; pacf(indpro, lag.max=60)$acf
## From plot, looks like AR(1) to me

# Plot the ACF and PACF plots for T10YFFM
layout(1:2); acf(t10yield, lag.max=60)$acf; pacf(t10yield, lag.max=60)$acf
layout(1:1); pacf(t10yield, lag.max=60)$acf
## From plot, looks like AR(2) to me

# Calculate the max number of lags to try in the Dickey-Fullers' test
numlags <- floor(12*((length(indpro)/100)^0.25))
# Check if numbers of lags are the same across the two time series
# numlags.indpro <- floor(12*((length(indpro)/100)^0.25)) 
# numlags.t10yield <- floor(12*((length(t10yield)/100)^0.25))

### Section 2: Check for stationarity, and transform data if not stationary
### 2.1: Dickey Fuller tests
# Get test stats for DF test for INDPRO
# Param: 
## Type: Trend (by default); 
## Lags: 18 (see above); 
## Method of selecting lags: Try on all lags from 1 to 18
# a) DF test for INDPRO
indpro.dftest <- ur.df(indpro, lags = numlags, selectlags = "Fixed")
# Print test stats
summary(indpro.dftest)@teststat
# And critical values
summary(indpro.dftest)@cval
## Conclusion for INDPRO: can't reject null -> not stationary - will take first-difference below
# b) DF test for T10YFFM
t10yield.dftest <- ur.df(t10yield, lags = numlags, selectlags = "Fixed")
# Print test stats
summary(t10yield.dftest)@teststat
# And critical value
summary(t10yield.dftest)@cval
## Conclusion for T10YFFM: reject null -> stationary - will fit ARMA model as is

### 2.2 Transformations of INDPRO: first differences
# Take first differences of INDPRO
indpro.diff <- diff(indpro)
# Plotting
layout(1:1); plot.ts(indpro.diff) # Seems okay now, will confirm with DF test and ACF/PACF below
# Get test stats for DF test for INDPRO after differencing 
indpro.diff.dftest <- ur.df(indpro.diff, lags = numlags, selectlags = "Fixed")
summary(indpro.diff.dftest)@teststat
summary(indpro.diff.dftest)@cval
## Conclusion: Reject null hypothesis, diff(INDPRO) is stationary
# Plot the ACF and PACF plots for diff(INDPRO)
layout(1:2); acf(indpro.diff, lag.max=60)$acf; pacf(indpro.diff, lag.max=60)$acf
## From plot, looks like diff(INDPRO) has a AR(2)?

### 2.3 Transformations of INDPRO: first differences of log
# Take first differences of INDPRO
indpro.difflog <- diff(log(indpro))
# Plotting
layout(1:1); plot.ts(indpro.difflog) # Seems okay now, will confirm with DF test and ACF/PACF below
# Get test stats for DF test for INDPRO after differencing 
indpro.difflog.dftest <- ur.df(indpro.difflog, lags = numlags, selectlags = "Fixed")
summary(indpro.difflog.dftest)@teststat
summary(indpro.difflog.dftest)@cval
## Conclusion: Reject null hypothesis, diff(log(INDPRO)) is stationary
# Plot the ACF and PACF plots for diff(log(INDPRO))
layout(1:2); acf(indpro.difflog, lag.max=60)$acf; pacf(indpro.difflog, lag.max=60)$acf
## From plot, looks like diff(log(INDPRO)) has a AR(1)?

### Section 3: Fitting models
### 3.1 Onto diff(INDPRO)
# Build matrix to store AIC and BIC (also rename columns and rows)
AIC.INDPRO <- matrix(NA,9,9)
rownames(AIC.INDPRO) <- paste("AR(", c(0:8), ")", sep="")
colnames(AIC.INDPRO) <- paste("MA(", c(0:8), ")", sep="")
BIC.INDPRO <- matrix(NA,9,9)
rownames(BIC.INDPRO) <- paste("AR(", c(0:8), ")", sep="")
colnames(BIC.INDPRO) <- paste("MA(", c(0:8), ")", sep="")


for (i in 0:8){
  for (j in 0:8){
    print(qq("Fitting ARMA(@{i}, @{j})"))
    # Fit ARMA(i, o, j) on the diff(INDPRO) data
    Test <- Arima(indpro.diff, order = c(i,0,j))
    # Store the AIC in the AIC.INDPRO matrix created above
    AIC.INDPRO[i+1,j+1] <- Test$aic
    # Store the AIC in the BIC.INDPRO matrix created above
    BIC.INDPRO[i+1,j+1] <- Test$bic
  }
}

# Print AIC for diff(INDPRO)
# Note here AIC for ARMA(i, j) will be stored in position [i+1, j+1]
AIC.INDPRO
# Print BIC for diff(INDPRO)
# Note here BIC for ARMA(i, j) will be stored in position [i+1, j+1]
BIC.INDPRO
## Both seems to suggest AR(3) for diff(INDPRO)

# Fit AR(3) on diff(INDPRO)
INDPRO.diff.AR3 = Arima(indpro.diff, order = c(3,0,0))
# Plot residuals, then ACF and PACF of residuals
checkresiduals(INDPRO.diff.AR3)
## Seems pretty white-noise-y, although strong auto-correlation at lag 24 of residuals (coincidence?)

### 3.2 Onto diff(log(INDPRO))
# Build matrix to store AIC and BIC (also rename columns and rows)
AIC.INDPROdifflog <- matrix(NA,9,9)
rownames(AIC.INDPROdifflog) <- paste("AR(", c(0:8), ")", sep="")
colnames(AIC.INDPROdifflog) <- paste("MA(", c(0:8), ")", sep="")
BIC.INDPROdifflog <- matrix(NA,9,9)
rownames(BIC.INDPROdifflog) <- paste("AR(", c(0:8), ")", sep="")
colnames(BIC.INDPROdifflog) <- paste("MA(", c(0:8), ")", sep="")

for (i in 0:8){
  for (j in 0:8){
    print(qq("Fitting ARMA(@{i}, @{j})"))
    # Fit ARMA(i, o, j) on the diff(INDPRO) data
    Test <- Arima(indpro.difflog, order = c(i,0,j))
    # Store the AIC in the AIC.INDPRO matrix created above
    AIC.INDPROdifflog[i+1,j+1] <- Test$aic
    # Store the AIC in the BIC.INDPRO matrix created above
    BIC.INDPROdifflog[i+1,j+1] <- Test$bic
  }
}

# Print AIC for INDPRO
# Note here AIC for ARMA(i, j) will be stored in position [i+1, j+1]
AIC.INDPROdifflog
# Print BIC for INDPRO
# Note here BIC for ARMA(i, j) will be stored in position [i+1, j+1]
BIC.INDPROdifflog
## Both seems to suggest ARMA(0,0) and MA(1) (wtf?) for diff(log(INDPRO))

# Fit AR(1) on diff(log(INDPRO))
INDPRO.difflog.AR1 = Arima(indpro.diff, order = c(1,0,0))
# Plot residuals, then ACF and PACF of residuals
checkresiduals(INDPRO.difflog.AR1)
## Doesn't seem very white-noise-y, 
## Also strong auto-correlation at lag 24 of residuals (coincidence?)
## Strong auto-correlation at the first few lags, let's check MA(1)

# Fit MA(1) on diff(log(INDPRO))
INDPRO.difflog.MA1 = Arima(indpro.diff, order = c(0,0,1))
# Plot residuals, then ACF and PACF of residuals
checkresiduals(INDPRO.difflog.MA1)
## ACF is similar to AR(1)
## Strong auto-correlation at the first few lags, let's check AR(2) next

# Fit AR(2) on diff(log(INDPRO))
INDPRO.difflog.AR2 = Arima(indpro.diff, order = c(2,0,0))
# Plot residuals, then ACF and PACF of residuals
checkresiduals(INDPRO.difflog.AR2)
## Eh, still not good enough, let's check AR(3)

# Fit AR(3) on diff(log(INDPRO))
INDPRO.difflog.AR3 = Arima(indpro.diff, order = c(3,0,0))
# Plot residuals, then ACF and PACF of residuals
checkresiduals(INDPRO.difflog.AR3)
## Looks better, similar to AR(3) when fitting on diff(INDPRO)

### 3.3 Onto T10YFFM
# Build matrix to store AIC and BIC (also rename columns and rows)
AIC.T10YFFM <- matrix(NA,9,9)
rownames(AIC.T10YFFM) <- paste("AR(", c(0:8), ")", sep="")
colnames(AIC.T10YFFM) <- paste("MA(", c(0:8), ")", sep="")
BIC.T10YFFM <- matrix(NA,9,9)
rownames(BIC.T10YFFM) <- paste("AR(", c(0:8), ")", sep="")
colnames(BIC.T10YFFM) <- paste("MA(", c(0:8), ")", sep="")

for (i in 0:8){
  for (j in 0:8){
    print(qq("Fitting ARMA(@{i}, @{j})"))
    # Fit ARMA(i, o, j) on the T10YFFM data
    Test <- Arima(t10yield, order = c(i,0,j))
    # Store the AIC in the AIC.T10YFFM matrix created above
    AIC.T10YFFM[i+1,j+1] <- Test$aic
    # Store the AIC in the BIC.T10YFFM matrix created above
    BIC.T10YFFM[i+1,j+1] <- Test$bic
  }
}

# Print AIC for T10YFFM
# Note here AIC for ARMA(i, j) will be stored in position [i+1, j+1]
AIC.T10YFFM

# Print BIC for T10YFFM
# Note here BIC for ARMA(i, j) will be stored in position [i+1, j+1]
BIC.T10YFFM

## Both seems to suggest ARMA(1, 1) for T10YFFM

# Fit ARMA(1,1) on diff(log(INDPRO))
T10YFFM.AR1.1 = Arima(t10yield, order = c(1,0,1))
# Plot residuals, then ACF and PACF of residuals
checkresiduals(T10YFFM.AR1.1)
## Ehhhh, doesn't look too good but okay fine...