library(zoo)
library(xts)
library(imputeTS)
# set work directory
setwd('D:\\MHE\\Dataset')
# import dataset
# Read TAB delimited files
sanmarcos<-read.delim('SanMarcos.txt', header = TRUE, sep = "\t", dec = ".")
head(sanmarcos)

# extract begin and end dates
bgn2<-as.Date(sanmarcos$datetime[1], '%Y-%m-%d')
print(bgn2)
end2<-as.Date(sanmarcos$datetime[length(sanmarcos$datetime)], '%Y-%m-%d')
print(end2)

datex2<-seq.Date(bgn2,end2,'day')
pdatex2<-as.Date(sanmarcos$datetime, format = '%Y-%m-%d')

# check missing data
data3.zoo<-zoo(sanmarcos$Discharge_cfs, pdatex2)
dum2.zoo<-zoo(,datex2)
data3.zoom<-merge(dum2.zoo,data3.zoo)
plot(data3.zoom, xlab='Year', ylab='San Marcos Spring Discharge (cfs)')
ggplot_na_distribution(data3.zoom)
ggplot_na_distribution2(data3.zoom)
# no NaN

# Autocorrelation
acf(data3.zoom, lag.max = 30)
# Partial autocorrelation
pacf(data3.zoom, lag.max = 30)
# cross relationship between J-17 and San Marcos spring datasets
ccf(data1_kalman_interp,data3.zoom)

# plot with J-17 in dual y-axis figure
# combine two dataset
rownum = 32983-24387
data1_delrow <- data1_kalman_interp[-(1:rownum),]
data13 <- data.frame(
  day = datex2,
  a = data1_delrow,
  b = data3.zoom
)

# transfer the y axis range
a.diff <- max(data13$a) - min(data13$a)
b.diff <- max(data13$b) - min(data13$b)
a.min <- min(data13$a)
b.min <- min(data13$b)

ggplot(data13, aes(x=day)) +
  geom_line(aes(y=a), color = "blue") +
  geom_line(aes(y = (b - b.min) / b.diff * a.diff + a.min), color = "red") +
  scale_x_continuous(name = "Day") +
  scale_y_continuous(name = "J-17",
                     sec.axis = sec_axis(trans = ~((. -a.min) * b.diff / a.diff) + b.min,
                                         name = "San Marcos")) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"))