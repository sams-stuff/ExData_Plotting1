thirdplot <- function() {
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
     
     # reset the graphic parameters and set mfrow to generate one plot
     graphic_default <- par(no.readonly = TRUE)
     par(graphic_default)
     par(mfrow = c(1, 1))

     # generate the plot of plot3
     plot(data2$DateTime, # the data for the x-axis
          data2$Sub_metering_1, # first set of data for the y-axis
          type = "l", # plot type is line
          xlab = "", # no label for the x-axis
          ylab = "Energy sub metering", # label for the y-axis
          cex.lab = 0.7, # set the font size for the labels
          cex.axis = 0.7 # set the width of the axes
          )
     
     # second set of data for the y-axis
     lines(data2$DateTime, data2$Sub_metering_2, type = "l", col = "red")
     
     # third set of data for the y-axis
     lines(data2$DateTime, data2$Sub_metering_3, type = "l", col = "blue")
     
     # add a legend
     legend("topright", # place the legend in the upper right
            c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), # label each of the three plots
            lty= c(1, 1, 1), # each plot is line type
            lwd = c(1.5, 1.5, 1.5), # set the width of each colored line in the legend
            col = c("black", "red", "blue"), # set the color of each plot
            cex = 0.7, # set the font size of the plot labels
            seg.len = 1, # adjust the length of each colored line in the legend
            adj = c(-0.4, 0.5), # adjust the spacing between the legend text and the upper right plot corner
            x.intersp = -1.8, # adjust the spacing between the colored lines and plot labels
            y.intersp = 0.8 # adjust the vertical spacing of plot labels
            )
     
     # copy the graphic to a png device called plot3.png
     dev.copy(png, file = "plot3.png")
     dev.off()
}