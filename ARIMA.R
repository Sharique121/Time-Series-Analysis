# Basic ARIMA Analysis #

skirts <- scan("C:/Users/user/Desktop/Econometrics/skirts.txt")
skirtsseries <- ts(skirts, start = c(1866))
plot.ts(skirtsseries)
# differencing the time series 
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)
# the time series after first differencing does not appear to be stationary as its mean is varying over time.
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
# the time series afetr second difference seems to be a stationary one.
skirtsseriesdiff3 <- diff(skirtsseries, differences=3)
plot.ts(skirtsseriesdiff3)
# Augmented Dickey Fuller Test to check stationarity of a time series

library(tseries)
adf.test(skirtsseries)
# Null hypothesis that assumes "no stationarity" has a support. Hence the time series is not stationary.
adf.test(skirtsseriesdiff1)
# Agian the null hypothesis has a support. Hence the time series is not stationary.
adf.test(skirtsseriesdiff2)
# test fails
adf.test(skirtsseriesdiff3)
# test succeeds. The time series is stationary after third differencing.

# The age at death of kings time series
kings <- scan("C:/Users/user/Desktop/Econometrics/kings.txt")
kingstimeseries <- ts(kings)
plot(kingstimeseries)
kingstimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot(kingstimeseriesdiff1)
kingstimeseriesdiff2 <- diff(kingstimeseries, differences=2)
plot(kingstimeseriesdiff2)
adf.test(kingstimeseries)
# the null hypothesis has a support.Hence not stationary.
adf.test(kingstimeseriesdiff1)
# the null hypothesis has a no support; hence stationary


# Selecting a candidate ARIMA model
# Plot the ACF and PACF 

acf(kingstimeseriesdiff1, lag.max=20)
acf(kingstimeseriesdiff1, lag.max=20, plot=FALSE)
# It is seen that the autocorrelation at lag 1 (-0.360) exceeds the significance bounds, but all other autocorrelations between lags 1 -20 did not exceed the significance bound.

pacf(kingstimeseriesdiff1, lag.max=20)
pacf(kingstimeseriesdiff1, lag.max=20, plot=FALSE)
# The partial autocorrelations at lags 1,2 and 3 exceeds the significant bounds and are negative. The partial autocorrelation slowly tails off to zero after lag 3. 

# The time series can be modeled as an ARMA(3,0) model, that is an autoregressive model of order 3, since the partial auto correlation tailed off to zero after lag 3 and the auto correlation tails off to zero. It can also be modled as ARMA(0,1) model, that is, a MOVING AVERAGE model of order q = 1, since the auto correlation is zero after lag 1 and the partial auto correlation tails off to zero. It can also be modeled as ARMA(p,q) model, that is a mixed model with p and q greater than 0, since the autocorrelation and partial auto correlation tail off to zero.
# We use the principle of parsimony to decide which model is the best, that is, we assume that the model with fewest parameters is the best. ARMA(3,0) has three parameters, ARMA(0,1) has one parameter, and ARMA(p,q) has at least 2 parameters. Hence the ARMA(0,1) model is taken as the best model.

# An ARMA(0,1) model is a moving average model of order 1 or MA(1) model. The model can be written as: X_t - mu = Z_t - (theta*Z_t-1), where X_t is the stationary time series (the first differenced serie of ages at death of English kings), mu is the mean of the time series X_t, Z_t is the white noise with mean zero and constant variance, and theta is a paramter that can be estimated. An MA model is usually used to model a time series that shows short-term dependencies between successive observations. It is expected that the age at death of of a particular english king to have some effect on the ages at death of the next king or two, but not much effect on the ages at the death of kings that reign much longer after that.

# use of auto.arima() function
library(forecast)
auto.arima(kings)
# we find ARIMA(0,1,1) model is the most fit. Hence, p = 0, d = 1 and q = 1.

# Example of Volcanic Dust Veil in the Northern Hemisphere
volcanodust <- scan("C:/Users/user/Desktop/Econometrics/volcano.txt")
volcanodustseries <- ts(volcanodust, start=c(1500))
plot.ts(volcanodustseries) 
# additive model is suitable and the time series is stationary - no differencing is required.

# acf and pacf plots
acf(volcanodustseries, lag.max = 20)
acf(volcanodustseries, lag.max = 20, plot=FALSE)
# The auto correlations at lags 1, 2 and 3 exceed the significance bounds and auto correlation tails off to zero after lag 3. The auto correlations at lag 19 and 20 may be due to chance. 

pacf(volcanodustseries, lag.max = 20)
pacf(volcanodustseries, lag.max = 20, plot = FALSE)
# The partial auto correlation at lag 1 is positive and exceeds the significance bound (0.666), while the partial autocorrelation at lag 2 is negative and also exceeds the significance bounds (-0.126). The partial autocorrelations tail off to zero after lag 2. 
# Hence the suitable models are: ARMA(2,0), ARMA(0,3) and ARMA(p,q) with non-zero p and q values.

auto.arima(volcanodust)
# ARIMA(1,0,2) is the best fit.
auto.arima(volcanodust, ic = "bic")
# ARIMA (2,0,0) is the best fit since BIC criteria penalizes paramter inclusion. 
# ARMA (2,0 model is an autoregressive model of order 2 or AR(2) model. This model can be written as: X_t - mu = (Beta1*(X_t-1 - mu)) + (Beta2*(X_t-2 - mu)) + Z_t, where X_t is the stationary time series (Volcanic dust weil index), mu is the mean of the time series X_t, Beta1 and Beta2 are parameters to be estimated, and Z_t is a white noise with mean zero and constant variance. 
# AR is susually used to model a time series which shows longer term dependencies between successive observations. Intutively, it makes sense that an AR model could be used to describe the time series of volcanic dust veil index , as we would expect volcanic dust level in one year to affect those in much later years, since dust are unlikely to disappear quickly.

# Forecasting using ARIMA model
auto.arima(kings)
kinstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1))
kinstimeseriesarima
kingstimeseriesforecasts <- forecast:::forecast.Arima(kinstimeseriesarima, h=5)
kingstimeseriesforecasts
forecast:::plot.forecast(kingstimeseriesforecasts)

acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")
# None of the auto correlation values for lags 1-20 exceed the significance bounds and the p-value for the Ljung-Box test is 0.9, we conclude that there is a very little evidence of non-zero autocorrelations in the forecast errors at lags 1-20. 

# Example of the Volcanic Dust Veil in the Northern Hemisphere
auto.arima(volcanodust, ic="bic")
volcanodustarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesforecasts <- forecast:::forecast.Arima(volcanodustarima, h=31)
volcanodustseriesforecasts
forecast:::plot.forecast(volcanodustseriesforecasts)

acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
# No evidence of nono-zero autocorrelations among the residuals
