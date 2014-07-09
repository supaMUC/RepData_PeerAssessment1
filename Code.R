data <- read.csv(unz("activity.zip", "activity.csv"), header=T, quote="\"", ## read data
                 sep=",",colClasses=c("numeric","factor","numeric"))

data$date <- as.Date(data$date)  ##Convert date column to Date type

