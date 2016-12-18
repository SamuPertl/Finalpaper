library(dplyr)

df <- read.csv("cpod data.csv" )
names(df)

# create a data set with just two 
df <- df %>% select(Minute, kHz)
