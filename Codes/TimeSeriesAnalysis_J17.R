library(zoo)
library(imputeTS)
library(forecast)
library(trend)
library(Kendall)
library('tseries')

path <- 'C:\\Users\\Aalok\\OneDrive - lamar.edu\\0000CVEN6301_ML\\TimeSeriesClass'
setwd(path)

#Bexar Index Well (J-17) hISTORICAL dATA
j17 <- read.csv('J17.csv')

#dATA FRO USGS 08168710 Comal Spgs at New Braunfels, TX
comal <- read.csv('Comal.csv')

#USGS 08170000 San Marcos Spgs at San Marcos, TX
sm <- read.csv('SanMarcos.csv')


#Create sequence of data for j17 data
bgn <- as.Date('1932-11-12') #Beginning date
end <- as.Date('2023-03-02') #Ending date
seq_dates <- seq.Date(bgn,end,'day') #Sequqence for each day
zoo_dates <- zoo(,seq_dates) #Convert the sequence of dates to a zoo object

#J-17 data
j17$DailyHighDate <- as.Date(j17$DailyHighDate,format='%m/%d/%Y')  #Recognize as Date
df <- zoo(j17$WaterLevelElevation,j17$DailyHighDate) #COnvert to zoo

#Merge two dataframes where missing values gets filled up with NA
merged_df <- merge(zoo_dates, df, all = TRUE)

#Summary of merged dataframe
summary(merged_df)

#Interpolated
j17_int <- na_kalman(merged_df,model = "StructTS")
summary(j17_int)

#Plot timeseries before and after imputation
plot(j17_int, col='red', main='Daily High Water Level at Well', ylab='Ht. in feet', xlab = 'Year')
lines(merged_df, col='blue', main='Daily High Water Level at Well', ylab='Ht. in feet', xlab = 'Year')
legend("bottomleft", legend = c("Observed", "Interpolated"), col = c("blue", "red"), lty = c(1, 1))

#Mann-Kendall Test
mk.test(as.vector(j17_int)) #From trend library
MannKendall(as.vector(j17_int)) #From Kendall library

#Visualise the trends
plot(j17_int, xlab = 'Year', ylab='Water level')
lines(lowess(time(j17_int),j17_int), col='blue')

#Seasonal MannKendall Test
zz <- as.ts(j17_int) #Create timneserties object
SeasonalMannKendall(zz)
#smk.test(zz,alternative = "greater", continuity = TRUE)



#10 day average
# Compute 10-day rolling averages
rolling_10day <- rollapply(j17_int, width = 10, FUN = mean, by = 1, align = "right")
plot(rolling_10day, col='blue', main='10-day rolling mean of High Water Level', ylab='Ht. in feet', xlab = 'Year')

Acf(j17_int, lag.max=1000, main = 'ACF', xlab = 'Lag (Days)')
Pacf(j17_int, lag.max=50, main = 'Partial ACF', xlab = 'Lag (Days)')

Acf(rolling_10day, lag.max=1000, main = 'ACF - 10 Day Rolling Avg', xlab = 'Lag (Days)')
Pacf(rolling_10day, lag.max=50, main = 'Partial ACF - 10 Day Rolling Avg', xlab = 'Lag (Days)')


#Monthly Mean
# Calculate the mean for each YYYY-MM
monthly_mean <- aggregate(j17_int, by = list(YYYY_MM = format(index(j17_int), "%Y-%m")), mean)
MannKendall(as.vector(monthly_mean)) #From Kendall library

#Yearly Mean
yearly_mean <- aggregate(j17_int, as.numeric(format(index(j17_int), "%Y")), mean)
MannKendall(as.vector(yearly_mean))

yearly_max <- aggregate(j17_int, as.numeric(format(index(j17_int), "%Y")), max)
MannKendall(as.vector(yearly_max))

yearly_min <- aggregate(j17_int, as.numeric(format(index(j17_int), "%Y")), min)
MannKendall(as.vector(yearly_min))



data <- j17_int

#ADF test
#https://www.r-bloggers.com/2022/06/augmented-dickey-fuller-test-in-r/
adf_res <- adf.test(as.vector(data))
adf_res

#KPSS test
#https://www.statology.org/kpss-test-in-r/
kpss_res <- kpss.test(data, null = 'Trend')
kpss_res

#Decompose
TS <- ts(data, frequency = 365)
output <- decompose(TS)
plot(output)


