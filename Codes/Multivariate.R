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



#J17
data <- data.frame(j17$DailyHighDate, j17$WaterLevelElevation)
colnames(data) <- c('DailyHighDate', 'WaterLevelElevation')
data$DailyHighDate <- as.Date(data$DailyHighDate,format='%m/%d/%Y')  #Recognize as Date
df_i <- zoo(data$WaterLevelElevation,data$DailyHighDate) #COnvert to zoo

#Create sequence of data for j17 data
bgn <- as.Date('1932-11-12') #Beginning date
end <- as.Date('2023-03-02') #Ending date
seq_dates <- seq.Date(bgn,end,'day') #Sequqence for each day
zoo_dates <- zoo(,seq_dates) #Convert the sequence of dates to a zoo object

#Merge two dataframes where missing values gets filled up with NA
df_j <- merge(zoo_dates, df_i, all = TRUE)

#Interpolated
df <- na_kalman(df_j,model = "StructTS")
summary(df)












#Comal
data1 <- data.frame(comal$datetime, comal$Mean_Dis_cfs)
colnames(data1) <- c('date', 'MeanDis')
data1$date <- as.Date(data1$date,format='%m/%d/%Y')  #Recognize as Date
df1_i <- zoo(data1$MeanDis,data1$date) #COnvert to zoo

#Create sequence of data for j17 data
bgn <- as.Date('1927-12-19') #Beginning date
end <- as.Date('2023-04-16') #Ending date
seq_dates <- seq.Date(bgn,end,'day') #Sequqence for each day
zoo_dates <- zoo(,seq_dates) #Convert the sequence of dates to a zoo object


#Merge two dataframes where missing values gets filled up with NA
df1_j <- merge(zoo_dates, df1_i, all = TRUE)
summary(df1_j)

#Interpolated
df1 <- na_kalman(df1_j,model = "StructTS")
summary(df1)



#San Marcos
data2 <- data.frame(sm$datetime, sm$Mean_Dis_cfs)
colnames(data2) <- c('date', 'MeanDis')
data2$date <- as.Date(data2$date,format='%m/%d/%Y')  #Recognize as Date
df2_i <- zoo(data2$MeanDis,data2$date) #COnvert to zoo

#Create sequence of data for j17 data
bgn <- as.Date('1956-05-26') #Beginning date
end <- as.Date('2023-04-14') #Ending date
seq_dates <- seq.Date(bgn,end,'day') #Sequqence for each day
zoo_dates <- zoo(,seq_dates) #Convert the sequence of dates to a zoo object

#Merge two dataframes where missing values gets filled up with NA
df2_j <- merge(zoo_dates, df2_i, all = TRUE)
summary(df2_j)

#Interpolated
df2 <- na_kalman(df2_j,model = "StructTS")
summary(df2)






#Create sequence of data for j17 data
bgn <- as.Date('1927-12-19') #Beginning date
end <- as.Date('2023-04-16') #Ending date
seq_dates <- seq.Date(bgn,end,'day') #Sequqence for each day
zoo_dates <- zoo(,seq_dates) #Convert the sequence of dates to a zoo object


#Merge two dataframes where missing values gets filled up with NA
merged_df <- merge(zoo_dates, df, df1,df2, all = TRUE)



j17_comal <- data.frame(merged_df$df, merged_df$df1)
j17_comal <- na.omit(j17_comal)
colnames(j17_comal) <- c('j17','comal')
Ccf(j17_comal$j17, j17_comal$comal, lag.max=200, main = 'Ccf of J17 Well Water Depth with Comal Discharge')


j17_sm <- data.frame(merged_df$df, merged_df$df2)
j17_sm <- na.omit(j17_sm)
colnames(j17_sm) <- c('j17','sm')
Ccf(j17_sm$j17, j17_sm$sm, lag.max=200, main = 'Ccf of J17 Well Water Depth with San Marcos Discharge')


comal_sm <- data.frame(merged_df$df1, merged_df$df2)
comal_sm <- na.omit(comal_sm)
colnames(comal_sm) <- c('comal','sm')
Ccf(comal_sm$comal, comal_sm$sm, lag.max=200, main = 'Ccf of Comal Discharge with San Marcos Discharge')




















