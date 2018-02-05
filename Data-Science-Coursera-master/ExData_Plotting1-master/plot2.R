data_full <- read.csv("./household_power_consumption.txt", header=T, sep=';', na.strings="?", nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
data_full$Date <- as.Date(data_full$Date, format="%d/%m/%Y")

## Extracting only the required data
data <- subset(data_full, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(data_full)

## Date Conversion
datetime <- paste(data$Date, data$Time)
data$Datetime <- as.POSIXct(datetime)


## Code for Plot 2
plot(data$Global_active_power~data$Datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

## Saving Plot 2 to PNG file
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()