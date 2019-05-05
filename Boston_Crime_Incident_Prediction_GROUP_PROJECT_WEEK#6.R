#### install.packages('ggplot2')# for the purpose of plotting 
#### install.packages('forecast')#for the purpose of making use of predict and ts 
#### install.packages('tseries')# for the purpose of making use of Time series analysis 
#### install.packages('TTR')#for the purpose of Functions and data to construct technical trading rules with R
#### Load packages from ggplot2, tseries, TTR and forcast packages
library('ggplot2')
library('forecast')
library('tseries')
library("TTR")
#### Read external data from csv file using read.csv() function, file.choose lets you choose the file of your choice 
### you must choose file named Cleaned_Data_updated.csv
CrimeData_Boston <- read.csv(file.choose())
#### developing time series out of the data set by making use of ts method 
CrimeCount_ts <- ts(CrimeData_Boston, frequency=12, start=c(2015,7))
#### Time Series Plot
plot(CrimeCount_ts)
#### get all components using decompose()
CrimeCountcomponents <- decompose(CrimeCount_ts)
  
#### Time Series components plot after decomposing 
plot(CrimeCountcomponents)
###it is  observed that seasonal component is very prominenet in determining the output

#### Dividing data into training and test dataset filtering using window() to start with forecasting
  train_set <- window(CrimeCount_ts, end=c(2017, 12)) #80 20 ration 2017 12
  test_set <- window(CrimeCount_ts, start=c(2018, 1))

#### plotting the train dataset 
  plot(train_set)
  
  
####[1]linear trend
#### Developing time series linear regression model
Linear_Trend <- tslm(train_set ~ trend)

#### forcasting for next 9 months
forecast_trend <- forecast(Linear_Trend,h=9)
plot(forecast_trend)

#### Checking the accuracy of linear trend
#### RMSE value can be calculated using accuracy function
accuracy(forecast_trend,test_set)
#RMSE(root means square error )
#694.7080



####[2]  Linear Trend + Seasonal techinque used for prediction
Linear_Trend_Seasonal <- tslm(train_set ~ trend + season)
#### forcasting for next 9 months
forecast_Trend_Seasonal <- forecast(Linear_Trend_Seasonal,h=9)
plot(forecast_Trend_Seasonal)

#### Checking the accuracy of linear trend + seasonal
#### RMSE value can be calculated using accuracy function
accuracy(forecast_Trend_Seasonal,test_set)
#RMSE
#495.2110 

####[3] Exponential Smoothing
#### Computes Holt-Winters Method of a given time series. Unknown parameters
#### are determined by minimizing the squared prediction error.
exp_smoothing_ts <- hw(train_set,n.ahead=9)
plot(exp_smoothing_ts)

#### Checking the accuracy of Exponential Smoothing
#### RMSE value can be calculated using accuracy function
accuracy(exp_smoothing_ts, test_set)
#RMSE
#940.3791

###[4]ARIMA model with manual determinitation of (p,d,q)

#### plotting the time_series models and checking if they are stationery  or not?
#plot a auto correlation fucntion plot to check now we see that the spkies gradually decrease into the 
#confidence proving the ts is no stationery 
acf(CrimeCount_ts)
### The resulting time series of Model_a_ts (above) does not appear to be stationary in
### mean. Therefore, we can
### difference the time series once, to see if that gives us a stationary time series:
CrimeCount_ts_diff <- diff(CrimeCount_ts, differences=1)
acf(CrimeCount_ts_diff)
#auto corelaion fucntion of the difference it  has not gradually decreasing lags 

acf(CrimeCount_ts_diff, lag.max = 50)
#auto correlaion fucntion of the difference it  has not gradually decreasing lags 

pacf(CrimeCount_ts_diff, lag.max=50)
#partial auto correlaion fucntion of the difference has all the lags within the confidence level

### In this way we can find the parameter d of arima model
### If you need to difference your original time series data d times in order to obtain
### a stationary time series, this
### means that you can use an ARIMA(p,d,q) model for your time series, where d is the 
###order of differencing used.
### For example, for the time series of the diameter of TV viewers, we had to difference
### the time series once, and
### so the order of differencing (d) is 0. This means that you can use an ARIMA(p,0,q) 
### model for our time series.
### The next step is to figure out the values of p and q for the ARIMA model.
### If your time series is stationary, or if you have transformed it to a stationary 
### time series by differencing d times,
### the next step is to select the appropriate ARIMA model, which means finding the 
### values of most appropriate values of p and q for an ARIMA(p,d,q) model.
###  To do this, you usually need to examine the auto correlation function  and partial auto correlation function 
###of the stationary time series.
### To plot an auto correlation function and partial auto correlation function we can use the "acf()" and "pacf()" 
### functions in R, respectively.

#******************************************************************************************

### The partial auto correlation function  shows that the partial autocorrelations does not exceed the
### significance bounds, Since the lag  is zero after lag 1 and the partial 
### lag tails off to zero after lag 1, this means that
### the following ARIMA (autoregressive integrated moving average) models are possible for the time
### series of first differences:
### 1. an ARIMA(1,0) model, that is, an autoregressive model of order p=1, since the 
###  partial auto correlation function is zero after lag 1, and the auto correlation function  tails off to zero
### 2. an ARIMA(2,0) model, that is, a moving average model of order q=1, since the acf
###is zero after lag 1 and the partial auto correlation function tails off to zero
### 3. an ARIMA(p,q) model, that is, a mixed model with p and q greater than 0, since 
### the auto correlation function and partial autocorrelation function tail off to zero
### We use the principle of parsimony to decide which model is best: that is, we assume
### that the model with the fewest
### parameters is best. The ARIMA(2,0) model has 2 parameters, the ARIMA(1,0) model has
### 1 parameter
### Therefore, the ARIMA(0,1) model is taken as the best model.
### This model has seasonal component So final model would be ARIMA(1,0,0,0,1,0)
#### executing ARIMA model using seasonal parameter = 1 for 12 months

Arima_ts_Model <- arima(train_set, order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12))

# fit an ARIMA(1,0,0) model
### Forcasting arima model for next 9 months
Arima_ts_Model_Forcast <- forecast(Arima_ts_Model, h=9)
plot(Arima_ts_Model_Forcast)

#### Checking the accuracy of Arima Model
#### RMSE value can be calculated using accuracy function
accuracy(Arima_ts_Model_Forcast, test_set)
#RMSE
#343.9849

#### [5] Running Auto Arima model to make a forecast for next 9 months 
Auto_Arima_ts_Model <- auto.arima(train_set)
Auto_Arima_ts_Model_Forcast <- forecast(Auto_Arima_ts_Model, h = 9)
plot(Auto_Arima_ts_Model_Forcast)

#### Checking the accuracy of Auto Arima Model
#### RMSE value can be calculated using accuracy function
accuracy(Auto_Arima_ts_Model_Forcast, test_set)
#RMSE
#343.9849

#### Conclusion:-
#### we have implemented 5 methods and compared the results to choose the best Model
#### To compare the models we have choosen RMSE values to compare the models.
#### Even though arima and AUTO arima gave us same RMSE we stick with auto arima as the value of d is determined in a stringent manner as comapared to ARIMA
#### After checking all the RMSE value we choose the auto Arima model for our analysis, as it had best accuracy and p,d,q values 
###  that helped provide the best fit with out train dataset 
#### which provide the best result
###  seasonal component plays a big part in  determining future trends 
#### Reference of data:- https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system/resource/12cb3883-56f5-47de-afa5-3b1cf61b257b











