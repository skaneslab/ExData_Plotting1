


### read dataset

filename <- "data.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists

if (!file.exists("household_power_consumption")) { 
  unzip(filename) 
}

dataset <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

dataset$Date <- as.Date(dataset$Date, "%d/%m/%Y")

dataset <- subset(dataset,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))


x<- paste(dataset$Date, dataset$Time, sep=" ")


datetime <- as.POSIXct(x)


dataset <- cbind(datetime, dataset)

#plot1
hist(dataset$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
#Save plot1
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()
#plot2 
plot(dataset$Global_active_power~dataset$datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
#Save plot2
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()
#plot3 
with(dataset, {
  plot(Sub_metering_1~datetime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~datetime,col='Red')
  lines(Sub_metering_3~datetime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#Save plot3
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()

#Plot 4

par(mfrow=c(2,2), mar=c(4,4,2,1))
with(dataset, {
  plot(Global_active_power~datetime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~datetime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~datetime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~datetime,col='Red')
  lines(Sub_metering_3~datetime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~datetime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

#Save plot4
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
