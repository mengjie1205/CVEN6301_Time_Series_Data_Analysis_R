library(zoo)
library(xts)
library(imputeTS)

# set work directory
setwd('D:\\MHE\\Dataset')
# import dataset
data1<-read.csv('J-17.csv')

# extract begin and end dates
bgn<-as.Date(data1$DailyHighDate[length(data1$DailyHighDate)], '%Y-%m-%d')
print(bgn)
end<-as.Date(data1$DailyHighDate[1], '%Y-%m-%d')
print(end)

datex<-seq.Date(bgn,end,'day')
pdatex<-as.Date(data1$DailyHighDate, format = '%Y-%m-%d')

# check missing data
data1.zoo<-zoo(data1$WaterLevelElevation, pdatex)
dum.zoo<-zoo(,datex)
data1.zoom<-merge(dum.zoo,data1.zoo)
plot(data1.zoom, xlab='Year', ylab='Water Level Elevation')
ggplot_na_distribution(data1.zoom)
ggplot_na_distribution2(data1.zoom)

# filling missing value
# try 1.interpolate(linear)
data1_linear_interp<-na_interpolation(data1.zoom)
plot(data1_linear_interp, xlab='Year', ylab='Water Level Elevation')
ggplot_na_imputations(data1.zoom, data1_linear_interp)

# try 2.Kalman filter
data1_kalman_interp<-na_kalman(data1.zoom)
plot(data1_kalman_interp, xlab='Year', ylab='Water Level Elevation')
ggplot_na_imputations(data1.zoom, data1_kalman_interp)

# Autocorrelation
acf(data1_linear_interp, lag.max = 30)
acf(data1_kalman_interp, lag.max = 30)
# Partial autocorrelation
pacf(data1_linear_interp, lag.max = 30)
pacf(data1_kalman_interp, lag.max = 30)

# decompose
#data1_decompose<-decompose(data1_kalman_interp) # Error:time series has no or less than 2 periods
# use TTR package to decompose non-seasonal timeseries data
library(TTR)
# use SMA to smooth data by simple moving average
data1_SMA10<-SMA(data1_kalman_interp, n=10)
plot(data1_SMA10, xlab='Year', ylab='Water Level Elevation')
data1_SMA100<-SMA(data1_kalman_interp, n=100)
plot(data1_SMA100, xlab='Year', ylab='Water Level Elevation')
data1_SMA1000<-SMA(data1_kalman_interp, n=1000)
plot(data1_SMA1000, xlab='Year', ylab='Water Level Elevation')

# xts package function: resample to monthly
data(data1_kalman_interp)
data1_sample<-as.xts(data1_kalman_interp)
data1_weekly<-to.period(data1_sample, period='weeks', OHLC=FALSE)
data1_monthly<-to.period(data1_sample, period='months', OHLC=FALSE)
plot(data1_weekly, xlab='Year', ylab='Water Level Elevation')
plot(data1_monthly, xlab='Year', ylab='Water Level Elevation')

# decompose
data1_weekly_decom<-decompose(data1_weekly)
data1_monthly_decom<-decompose(data1_monthly)