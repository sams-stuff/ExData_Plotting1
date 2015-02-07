fourthplot <- function() {
     # use read.table() to read the text data file into R
     alldata <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";")
     
     # subset the two days in February into an R object
     data <- alldata[which(alldata$Date == "1/2/2007" | alldata$Date == "2/2/2007"), ]
     
     # copy the subset to another object for comparison
     data2 <- data
     
     # change the classes of columns 3-8 from factor to numeric
     for(i in 3:8) {
          data2[, i] <- as.numeric(as.character(data[, i]))
     }
     
     # combine the Date and Time columns into one column called Date/Time
     # and use as.Date() to set the values/class of the Date column
     temp <- paste(data$Date, data$Time)
     temp2 <- strptime(temp, "%d/%m/%Y %H:%M:%S")
     data2$Date <- as.Date(temp2)
     data2$Time <- temp2
     names(data2)[2] <- "DateTime"
     
     # reset the graphic parameters
     graphic_default <- par(no.readonly = TRUE)
     par(graphic_default)
     
     # generate the plot of plot4
     # set mfrow for two plots on top of two plots
     par(mfrow = c(2, 2))
     
     # create the four different plots
     with(data2, {
          # first plot in upper left with arguments similar to previous plots
          plot(data2$DateTime, data2$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power", cex.lab = 0.7, cex.axis = 0.7)
          
          # second plot in upper right with arguments mostly similar to previous plots
          plot(data2$DateTime, Voltage, type = "l", xlab = "datetime", ylab = "Voltage", cex.lab = 0.7, cex.axis = 0.7)
          
          # third plot in lower left similar to plot3 with some different arguments for the legend
          plot(data2$DateTime, data2$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering", cex.lab = 0.7, cex.axis = 0.7)
          lines(data2$DateTime, data2$Sub_metering_2, type = "l", col = "red")
          lines(data2$DateTime, data2$Sub_metering_3, type = "l", col = "blue")
          legend("topright", 
                 c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                 lty= c(1, 1, 1), 
                 lwd = c(1.5, 1.5, 1.5), 
                 col = c("black", "red", "blue"), 
                 cex = 0.7, 
                 seg.len = 1.0, 
                 adj = c(-0.6, 0.5), # adjust the spacing between the legend text and the upper right plot corner
                 x.intersp = -2.5, # adjust the spacing between the colored lines and plot labels
                 y.intersp = 0.5, # adjust the vertical spacing of plot labels
                 bty = "n", # remove the box border of the legend
                 inset = c(0, -0.08) # move the legend closer to the corner of the plot
                 )
          
          # fourth plot in lower right
          plot(data2$DateTime, data2$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power", cex.lab = 0.7, cex.axis = 0.7)
     })
     
     # copy the graphic to a png device called plot4.png
     dev.copy(png, file = "plot4.png")
     dev.off()
}