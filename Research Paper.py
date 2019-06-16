# -*- coding: utf-8 -*-
"""
Created on Sat May 11 19:29:56 2019

@author: Rutger
"""

import math
import pandas as pd
from pandas import Series
from matplotlib import pyplot
import numpy as np
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.vector_ar.var_model import VAR
from statsmodels.graphics.tsaplots import plot_pacf
from sklearn.metrics import mean_squared_error
from scipy.stats import skew
import os

# Make sure that the Excel file is in your working
# directory. Also, make sure that you delete all
# data prior to 2007 in the Excel file.
cwd = os.getcwd()
# Loc: Saving this line to run code locally for me
# os.chdir('C:\\Users\\vinhl\\OneDrive\\[Work] Translate and Class\\[RUG] Dynamic Econometrics\\dynamic_econometrics')
print(cwd)


# INDPRO
data = pd.read_excel('FREDMD_march2019.xlsx')
indpro = pd.DataFrame(data, columns= ['INDPRO'])[0:577] # Select only data upto 2007
date = pd.DataFrame(data, columns= ['sasdate'])[0:577] # Select only data upto 2007

fig = pyplot.figure()
ax = pyplot.axes()
ax.plot(date, indpro)
ax.set(xlabel='Time', ylabel='INDPRO')
pyplot.show()

plot_acf(indpro, lags=60)
plot_pacf(indpro, lags=10)


# INDPRO differenced
indpro_temp = indpro.iloc[:,0].values
d_test_indpro = indpro_temp[1:-1] - indpro_temp[0:-2]

def difference(dataset, interval):
    diff = list()
    for i in range(interval, len(dataset)):
        value = dataset[i] - dataset[i - interval]
        diff.append(value)
    return Series(diff)
d_indpro_temp = difference(indpro_temp, 1)

date_temp = date.iloc[:,0].values
date_temp = np.delete(date_temp,0)

pyplot.plot(date_temp, d_indpro_temp)
pyplot.show()
print('Skewness: %f' % skew(d_indpro_temp))

plot_acf(d_indpro_temp, lags=60)
plot_pacf(d_indpro_temp, lags=20)


# INDPRO differenced log
ln_indpro = list()
for i in range(0, len(indpro_temp)):
    value = math.log(indpro_temp[i])
    ln_indpro.append(value)

ln_indpro = pd.DataFrame(ln_indpro)
ln_indpro_temp = ln_indpro.iloc[:,0].values
d_ln_indpro_temp = difference(ln_indpro_temp, 1)

pyplot.plot(date[0:-1], d_ln_indpro_temp)
pyplot.show()
print('Skewness: %f' % skew(ln_d_indpro))

plot_acf(d_ln_indpro_temp, lags=60)
plot_pacf(d_ln_indpro_temp, lags=20)


# Adfuller tests
## INDPRO w/o first differencing
result = adfuller(ln_indpro_temp)
print('ADF Statistic: %f' % result[0])
print('p-value: %f' % result[1])
print('Critical Values:')
for key, value in result[4].items():
    print('\t%s: %.3f' % (key, value))

    
## INDPRO first differenced
result2 = adfuller(d_indpro_temp)
print('ADF Statistic: %f' % result2[0])
print('p-value: %f' % result2[1])
print('Critical Values:')
for key, value in result2[4].items():
    print('\t%s: %.3f' % (key, value))

## INDPRO first differenced log
result3 = adfuller(d_ln_indpro_temp)
print('ADF Statistic: %f' % result3[0])
print('p-value: %f' % result3[1])
print('Critical Values:')
for key, value in result3[4].items():
    print('\t%s: %.3f' % (key, value))

    
# ARIMA INDPRO
## fit model ARIMA(4,1,0), differencing done
## by ARIMA
model = ARIMA(indpro, order=(3, 1, 0))
model_fit = model.fit(disp=0)
print(model_fit.summary())

## fit model ARIMA(4,0,0), differencing done by me
## beforehand. So this is essentially an ARMA(4, 0)
## model on the already differenced data
d_indpro = pd.DataFrame(d_indpro_temp)
model2 = ARIMA(d_indpro, order=(3, 0, 0))
model_fit2 = model2.fit(disp=0)
print(model_fit2.summary())

### model2 is equivalent to model
### hence, my differenced series is differenced
### in the same way as the ARIMA function differences

residuals = pd.DataFrame(model_fit.resid)
plot_acf(residuals, lags=100)
residuals.plot()
pyplot.show()
residuals.plot(kind='kde')
pyplot.show()
print(residuals.describe())

## fit model ARIMA(1,0,0) on differenced log
d_ln_indpro = pd.DataFrame(d_ln_indpro_temp)
model3 = ARIMA(d_ln_indpro, order=(1, 0, 0))
model_fit3 = model3.fit(disp=0)
print(model_fit3.summary())

residuals = pd.DataFrame(model_fit3.resid)
plot_acf(residuals, lags=100)
residuals.plot()
pyplot.show()
residuals.plot(kind='kde')
pyplot.show()
print(residuals.describe())

### notice that the acf plot of the residuals shows
### no serial correlation, which implies that
### there is no need to include an MA (q) coefficient
### in the ARIMA model
### also notice that the distribution of the
### residuals has mean zero, which is good


# Forecasting

## forecasting last 34% of d_indpro using ARMA(4, 0)
X = d_indpro.values
len(X) *0.66
int(len(X) *0.66)
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(4, 0, 0))
    model_fit = model.fit(disp=0)
    output = model_fit.forecast()
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)
    # maybe put a # in front of the line below
    #print('predicted=%f, expected=%f' % (yhat, obs))
error = mean_squared_error(test, predictions)
print('Test MSE: %.3f' % error)

pyplot.plot(test)
pyplot.plot(predictions, color='red')
pyplot.show()


## forecasting last 34% of d_ln_indpro using ARMA(1, 0)
X = d_ln_indpro.values
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(1, 0, 0))
    model_fit = model.fit(disp=0)
    output = model_fit.forecast()
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)
    # maybe put a # in front of the line below
    #print('predicted=%f, expected=%f' % (yhat, obs))
error = mean_squared_error(test, predictions)
print('Test MSE: %.3f' % error)

pyplot.plot(test)
pyplot.plot(predictions, color='red')
pyplot.show()


## forecasting last 34% of d_ln_indpro using ARMA(3, 0)
X = d_ln_indpro.values
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(3, 0, 0))
    model_fit = model.fit(disp=0)
    output = model_fit.forecast()
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)
    # maybe put a # in front of the line below
    #print('predicted=%f, expected=%f' % (yhat, obs))
error = mean_squared_error(test, predictions)
print('Test MSE: %.9f' % ((test-predictions)**2).mean())
print('Test MSE: %.9f' % error)

pyplot.plot(test)
pyplot.plot(predictions, color='red')
pyplot.show()


# T10YFFM
t10yffm = pd.DataFrame(data, columns= ['T10YFFM'])[0:577]
pyplot.plot(date, t10yffm)

## T10YFFM acf and pacf
plot_acf(t10yffm, lags=40)
plot_pacf(t10yffm, lags=30)

## adfuller T10YFFM
t10yffm_temp = t10yffm.iloc[:,0].values
result4 = adfuller(t10yffm_temp)
print('ADF Statistic: %f' % result4[0])
print('p-value: %f' % result4[1])
print('Critical Values:')
for key, value in result4[4].items():
    print('\t%s: %.3f' % (key, value))

# ARIMA T10YFFM
model4 = ARIMA(t10yffm, order=(1, 0, 1))
model_fit4 = model4.fit(disp=0)
print(model_fit4.summary())

residuals = pd.DataFrame(model_fit4.resid)
plot_acf(residuals, lags=100)
residuals.plot()
pyplot.show()
residuals.plot(kind='kde')
pyplot.show()
print(residuals.describe())

## forecasting last 34% of d_indpro using ARMA(1, 1)
X = t10yffm.values
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(1, 0, 1))
    model_fit = model.fit(disp=0)
    output = model_fit.forecast()
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)
    # maybe put a # in front of the line below
    #print('predicted=%f, expected=%f' % (yhat, obs))
error = mean_squared_error(test, predictions)
print('Test MSE: %.3f' % error)

pyplot.plot(test)
pyplot.plot(predictions, color='red')
pyplot.show()

## forecasting last 34% of d_indpro using ARMA(3, 0)
X = t10yffm.values
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(3, 0, 0))
    model_fit = model.fit(disp=0)
    output = model_fit.forecast()
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)
    # maybe put a # in front of the line below
    #print('predicted=%f, expected=%f' % (yhat, obs))
error = mean_squared_error(test, predictions)
print('Test MSE: %.9f' % error)

pyplot.plot(test)
pyplot.plot(predictions, color='red')
pyplot.show()


# VAR model indpro t10yffm
d_ln_indpro_var = list()
d_ln_indpro_var_temp = d_ln_indpro.iloc[:,0].values
for i in range(0, len(d_ln_indpro_var_temp)):
    value = d_ln_indpro_var_temp[i]
    d_ln_indpro_var.append(value)

t10yffm_var = list()
t10yffm_var_temp = t10yffm.iloc[:,0].values
for i in range(0, len(t10yffm_var_temp)):
    value = t10yffm_var_temp[i]
    t10yffm_var.append(value)

vardata = list()  
for i in range(0, len(d_ln_indpro_var)):
    v1 = d_ln_indpro_var[i]
    v2 = t10yffm_var[i]
    row = [v1, v2]
    vardata.append(row)

varmodel = VAR(vardata)
varmodel_fit = varmodel.fit(3) ##VAR(1) has lower criterion, but correlation in ACF of T10YFFM residuals, VAR(3) better
print(varmodel_fit.summary())

## Residuals
residuals = pd.DataFrame(varmodel_fit.resid)

plot_acf(residuals[0], lags=100)
pyplot.show()
residuals[0].plot()
pyplot.show()
residuals[0].plot(kind='kde')
pyplot.show()

plot_acf(residuals[1], lags=100)
pyplot.show()
residuals[1].plot()
pyplot.show()
residuals[1].plot(kind='kde')
pyplot.show()

print(residuals.describe())

## Forecasting using VAR
X = d_ln_indpro.values
Y = t10yffm.values
size = int(len(X) *0.66)
trainX, testX = X[0:size], X[size:len(X)]
trainY, testY = Y[0:size], Y[size:len(Y)]
historyX = [x for x in trainX]
historyY = [y for y in trainY]

history = list()
for i in range(0, len(historyX)):
    row = [historyX[i], historyY[i]]
    history.append(row)
    
test = list()
for i in range(0, len(testX)):
    row = [testX[i], testY[i]]
    test.append(row)

predictions = list()
for t in range(len(test)):
    model = VAR(history)
    model_fit = model.fit()
    output = model_fit.forecast(model_fit.y, steps=1)
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)

testindpro = list()
testt10yffm = list()
for i in range(0, len(test)):
    value = test[i]
    testindpro.append(value[0])
    testt10yffm.append(value[1])

predindpro = list()
predt10yffm = list()
for i in range(0, len(predictions)):
    value = predictions[i]
    predindpro.append(value[0])
    predt10yffm.append(value[1])

errorindpro = mean_squared_error(testindpro, predindpro)
print('Test MSE: %.3f' % errorindpro)   
pyplot.plot(testindpro)
pyplot.plot(predindpro, color='red')
pyplot.show()

errort10yffm= mean_squared_error(testt10yffm, predt10yffm)
print('Test MSE: %.3f' % errort10yffm) 
pyplot.plot(testt10yffm)
pyplot.plot(predt10yffm, color='red')
pyplot.show()


## Impulse Response Analysis
irf = varmodel_fit.irf(30)
irf.plot(orth=False)
