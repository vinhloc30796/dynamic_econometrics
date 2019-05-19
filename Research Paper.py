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
from statsmodels.graphics.tsaplots import plot_pacf
from sklearn.metrics import mean_squared_error
from scipy.stats import skew
import os

# Make sure that the Excel file is in your working
# directory. Also, make sure that you delete all
# data prior to 2007 in the Excel file.
cwd = os.getcwd()
print(cwd)

# INDPRO
data = pd.read_excel('FREDMD_march2019_upto2007.xlsx')
df_ip = pd.DataFrame(data, columns= ['INDPRO'])
df_date = pd.DataFrame(data, columns= ['sasdate'])

pyplot.plot(df_date, df_ip)

plot_acf(df_ip, lags=60)
plot_pacf(df_ip, lags=10)

# INDPRO differenced
df_ip2 = df_ip.iloc[:,0].values
def difference(dataset, interval):
    diff = list()
    for i in range(interval, len(dataset)):
        value = dataset[i] - dataset[i - interval]
        diff.append(value)
    return Series(diff)
df_ip_diff = difference(df_ip2, 1)

df_date2 = df_date.iloc[:,0].values
df_date2 = np.delete(df_date2,0)

pyplot.plot(df_date2, df_ip_diff)
pyplot.show()
print('Skewness: %f' % skew(df_ip_diff))

plot_acf(df_ip_diff, lags=60)
plot_pacf(df_ip_diff, lags=20)

# INDPRO differenced log
logdf_ip = list()
for i in range(0, len(df_ip2)):
    value = math.log(df_ip2[i])
    logdf_ip.append(value)
    
logdf_ip = pd.DataFrame(logdf_ip)
logdf_ip2 = logdf_ip.iloc[:,0].values
logdf_ip_diff = difference(logdf_ip2, 1)

pyplot.plot(df_date2, logdf_ip_diff)
pyplot.show()
print('Skewness: %f' % skew(logdf_ip_diff))

plot_acf(logdf_ip_diff, lags=60)
plot_pacf(logdf_ip_diff, lags=20)    

# Adfuller tests
## INDPRO w/o first differencing
result = adfuller(df_ip2)
print('ADF Statistic: %f' % result[0])
print('p-value: %f' % result[1])
print('Critical Values:')
for key, value in result[4].items():
    print('\t%s: %.3f' % (key, value))

## INDPRO first differenced  
result2 = adfuller(df_ip_diff)
print('ADF Statistic: %f' % result2[0])
print('p-value: %f' % result2[1])
print('Critical Values:')
for key, value in result2[4].items():
    print('\t%s: %.3f' % (key, value))
    
## INDPRO first differenced log    
result3 = adfuller(logdf_ip_diff)
print('ADF Statistic: %f' % result3[0])
print('p-value: %f' % result3[1])
print('Critical Values:')
for key, value in result3[4].items():
    print('\t%s: %.3f' % (key, value))
    
# ARIMA INDPRO 
## fit model ARIMA(4,1,0), differencing done 
## by ARIMA
model = ARIMA(df_ip, order=(3, 1, 0))
model_fit = model.fit(disp=0)
print(model_fit.summary())
    
## fit model ARIMA(4,0,0), differencing done by me
## beforehand. So this is essentially an ARMA(4, 0) 
## model on the already differenced data
df_ip_diff = pd.DataFrame(df_ip_diff)
model2 = ARIMA(df_ip_diff, order=(3, 0, 0))
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
logdf_ip_diff = pd.DataFrame(logdf_ip_diff)
model3 = ARIMA(logdf_ip_diff, order=(1, 0, 0))
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
## forecasting of last 34% differences
X = df_ip_diff.values
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

## forecasting of last 34% actual index
X = df_ip.values
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(4, 1, 0))
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

## forecasting of last 34% logged differences
X = logdf_ip_diff.values
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

## forecasting of last 34% logged index
X = logdf_ip.values
size = int(len(X) *0.66)
train, test = X[0:size], X[size:len(X)]
history = [x for x in train]
predictions = list()
for t in range(len(test)):
    model = ARIMA(history, order=(1, 1, 0))
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

# T10YFFM
df_t10 = pd.DataFrame(data, columns= ['T10YFFM'])
pyplot.plot(df_date, df_t10)

## T10YFFM acf and pacf
plot_acf(df_t10, lags=40)
plot_pacf(df_t10, lags=30)

## adfuller T10YFFM
df_t102 = df_t10.iloc[:,0].values
result4 = adfuller(df_t102)
print('ADF Statistic: %f' % result4[0])
print('p-value: %f' % result4[1])
print('Critical Values:')
for key, value in result4[4].items():
    print('\t%s: %.3f' % (key, value))
    
# ARIMA T10YFFM
model4 = ARIMA(df_t10, order=(1, 0, 1))
model_fit4 = model4.fit(disp=0)
print(model_fit4.summary())

residuals = pd.DataFrame(model_fit4.resid)
plot_acf(residuals, lags=100)
residuals.plot()
pyplot.show()
residuals.plot(kind='kde')
pyplot.show()
print(residuals.describe())

## forecasting of last 34% T10YFFM
X = df_t10.values
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
