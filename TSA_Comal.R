library(zoo)
library(imputeTS)
library(forecast)
library(trend)

path <- 'C:\\Users\\Aalok\\OneDrive - lamar.edu\\0000CVEN6301_ML\\TimeSeriesClass'
setwd(path)

#dATA FRO USGS 08168710 Comal Spgs at New Braunfels, TX
comal <- read.csv('Comal.csv')


#Create sequence of data for j17 data
bgn <- as.Date('1927-12-19') #Beginning date
end <- as.Date('2023-04-16') #Ending date
seq_dates <- seq.Date(bgn,end,'day') #Sequqence for each day
zoo_dates <- zoo(,seq_dates) #Convert the sequence of dates to a zoo object

data <- data.frame(comal$datetime, comal$Mean_Dis_cfs)
colnames(data) <- c('date', 'MeanDis')

#Comal data
data$date <- as.Date(data$date,format='%m/%d/%Y')  #Recognize as Date
df <- zoo(data$MeanDis,data$date) #COnvert to zoo
summary(df)

#Merge two dataframes where missing values gets filled up with NA
merged_df <- merge(zoo_dates, df, all = TRUE)

#Summary of merged dataframe
summary(merged_df)

#Interpolated
imp_data <- na_kalman(merged_df,model = "StructTS")
summary(imp_data)

#Plot timeseries before and after imputation
plot(imp_data, col='red', main='Mean Discharge at Comal Spring', ylab='Discharge (cfs)', xlab = 'Year')
lines(merged_df, col='blue')
legend("bottomleft", legend = c("Observed", "Interpolated"), col = c("blue", "red"), lty = c(1, 1))

#Mann-Kendall Test
mk.test(as.vector(imp_data)) #From trend library
MannKendall(as.vector(imp_data)) #From Kendall library


#ADF Test
adf.test(as.vector(imp_data))


#Visualise the trends
plot(imp_data, xlab='Date', ylab='Discharge(cfs)')
lines(lowess(time(imp_data),imp_data), col='blue')

#Decompose
TS <- decompose(ts(imp_data,frequency=365))
plot(TS)


#10 day average
# Compute 10-day rolling averages
rolling_10day <- rollapply(imp_data, width = 10, FUN = mean, by = 1, align = "right")
plot(rolling_10day, col='blue', main='10-day rolling mean of High Water Level', ylab='Ht. in feet', xlab = 'Year')

Acf(imp_data, lag.max=1000, main = 'ACF', xlab = 'Lag (Days)')
Pacf(imp_data, lag.max=25, main = 'Partial ACF', xlab = 'Lag (Days)')

Acf(rolling_10day, lag.max=1000, main = 'ACF - 10 Day Rolling Avg', xlab = 'Lag (Days)')
Pacf(rolling_10day, lag.max=50, main = 'Partial ACF - 10 Day Rolling Avg', xlab = 'Lag (Days)')


#Monthly Mean
# Calculate the mean for each YYYY-MM
monthly_mean <- aggregate(imp_data, by = list(YYYY_MM = format(index(imp_data), "%Y-%m")), mean)
MannKendall(as.vector(monthly_mean)) #From Kendall library

#Yearly Mean
yearly_mean <- aggregate(imp_data, as.numeric(format(index(imp_data), "%Y")), mean)
MannKendall(as.vector(yearly_mean))

yearly_max <- aggregate(imp_data, as.numeric(format(index(imp_data), "%Y")), max)
MannKendall(as.vector(yearly_max))

yearly_min <- aggregate(imp_data, as.numeric(format(index(imp_data), "%Y")), min)
MannKendall(as.vector(yearly_min))

plot(yearly_min)
lines(lowess(time(yearly_min),yearly_min), col='blue')





