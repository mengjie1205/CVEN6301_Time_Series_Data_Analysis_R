library(zoo)
library(xts)
library(imputeTS)
# set work directory
setwd('D:\\MHE\\Dataset')
# import dataset
# Read TAB delimited files
comal<-read.delim('ComalSpring.txt', header = TRUE, sep = "\t", dec = ".")
head(comal)

# extract begin and end dates
bgn<-as.Date(comal$datetime[1], '%Y-%m-%d')
print(bgn)
end<-as.Date(comal$datetime[length(comal$datetime)], '%Y-%m-%d')
print(end)

datex<-seq.Date(bgn,end,'day')
pdatex<-as.Date(comal$datetime, format = '%Y-%m-%d')

# check missing data
data2.zoo<-zoo(comal$Discharge_cfs, pdatex)
dum.zoo<-zoo(,datex)
data2.zoom<-merge(dum.zoo,data2.zoo)
plot(data2.zoom, xlab='Year', ylab='Comal Spring Discharge (cfs)')
ggplot_na_distribution(data2.zoom)
ggplot_na_distribution2(data2.zoom)

# fill missing data by Kalman filter
data2_kalman_interp<-na_kalman(data2.zoom)
plot(data2_kalman_interp, xlab='Year', ylab='Comal Spring Discharge (cfs)')
ggplot_na_imputations(data2.zoom, data2_kalman_interp)

library(ggplot2)
# plot with J-17 in dual y-axis figure
# combine two dataset
data12 <- data.frame(
  day = datex,
  a = data1_kalman_interp,
  b = data2_kalman_interp
)

# transfer the y axis range
a.diff <- max(data12$a) - min(data12$a)
b.diff <- max(data12$b) - min(data12$b)
a.min <- min(data12$a)
b.min <- min(data12$b)

ggplot(data12, aes(x=day)) +
  geom_line(aes(y=a), color = "blue") +
  geom_line(aes(y = (b - b.min) / b.diff * a.diff + a.min), color = "red") +
  scale_x_continuous(name = "Day") +
  scale_y_continuous(name = "J-17",
                     sec.axis = sec_axis(trans = ~((. -a.min) * b.diff / a.diff) + b.min,
                                         name = "Comal")) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"))

# Autocorrelation
acf(data2_kalman_interp, lag.max = 30)
# Partial autocorrelation
pacf(data2_kalman_interp, lag.max = 30)
# cross relationship between J-17 and comal spring datasets
ccf(data1_kalman_interp,data2_kalman_interp)


# decompose
#data2_decompose<-decompose(data2_kalman_interp) # Error:time series has no or less than 2 periods
# use TTR package to decompose non-seasonal timeseries data
library(TTR)
# use SMA to smooth data by simple moving average
data2_SMA10<-SMA(data2_kalman_interp, n=10)
plot(data2_SMA10, xlab='Year', ylab='Comal Spring Discharge (cfs)')
data2_SMA100<-SMA(data2_kalman_interp, n=100)
plot(data2_SMA100, xlab='Year', ylab='Comal Spring Discharge (cfs)')
data2_SMA1000<-SMA(data2_kalman_interp, n=1000)
plot(data2_SMA1000, xlab='Year', ylab='Comal Spring Discharge (cfs)')

# xts package function: resample to monthly
data(data2_kalman_interp)
data2_sample<-as.xts(data2_kalman_interp)
data2_weekly<-to.period(data2_sample, period='weeks', OHLC=FALSE)
data2_monthly<-to.period(data2_sample, period='months', OHLC=FALSE)
plot(data2_weekly, xlab='Year', ylab='Comal Spring Discharge (cfs)')
plot(data2_monthly, xlab='Year', ylab='Comal Spring Discharge (cfs)')

# decompose
data_weekly_decom<-decompose(data_weekly)
data_monthly_decom<-decompose(data_monthly)
