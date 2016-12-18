install.packages("lubridate")
install.packages("StreamMetabolism")


library(dplyr)
library(lubridate)
library(StreamMetabolism)



df <- read.csv("cpod data.csv" )
names(df)

# create a data set with just two 
df <- df %>% select(Minute, kHz)

# rename Minute into date 
df[df == "Minute"]<- date
# try to convert the time variable 
as.POSIXct(df$Minute)

df$Minute <- as.POSIXct(strptime(df$Minute, "%Y-%m-%d %H:%M:%S"))
strptime(df$Minute, "%Y-%m-%d %H:%M:%S")



# try day/night 

sunrise.set(49.246292, -123.116226, "2016/09/01", num.days = 6)

# rename Minute into date 
df$date <- df$Minute

# select date and kHz 
df <- select(df, date, kHz)
df$date <- as.POSIXct(df$date)

# Add a date column (with whatever timezone you want)
df$date <- as.Date(df$date, tz = "America/Vancouver")

# Following generates the sunrise and sunset times for the two example dates
sunRise <- c(rep(as.POSIXct('2016-09-01 13:29:34'), 6), rep(as.POSIXct('2016-09-02 13:31:01'),42), rep(as.POSIXct('2016-09-03 13:32:27'),372), rep(as.POSIXct('2016-09-04 13:33:53'), 245),
             rep(as.POSIXct('2016-09-05 13:35:20'),52), rep(as.POSIXct('2016-09-06 13:36:46'),15))
sunSet <- c(rep(as.POSIXct('2016-09-02 02:53:53'),6), rep(as.POSIXct('2016-09-03 02:51:48'), 42), rep(as.POSIXct('2016-09-04 02:49:42'),372), rep(as.POSIXct('2016-09-05 02:47:36'), 245),
            rep(as.POSIXct('2016-09-06 02:45:30'), 52 ), rep(as.POSIXct('2016-09-07 02:43:23'), 15))
sun <- data.frame(date = as.Date(sunRise, tz = "America/Vancouver"), sunRise = sunRise, sunSet = sunSet)


# Join the two tables and compute night/day
df1 <- merge(df, sun)
df$dayNight <- ifelse(df1$Mitue > df1$sunRise & df$Minute < df$sunSet, 'day', 'night')

