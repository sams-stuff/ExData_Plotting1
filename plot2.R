secondplot <- function() {
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
     
     # reset the graphic parameters and set mfrow for one plot
     graphic_default <- par(no.readonly = TRUE)
     par(graphic_default)
     par(mfrow = c(1, 1))
     
     # generate the plot of plot2
     plot(data2$DateTime, # data for the x-axis
          data2$Global_active_power, # data for the y-axis
          type = "l", # plot type is line
          xlab = "", # no label for the x-axis
          ylab = "Global Active Power (kilowatts)", # label the y-axis
          cex.lab = 0.7, # set the font size for the labels
          cex.axis = 0.7 # set the width of the axes
          )
     
     # copy the graphic to a png device called plot2.png
     dev.copy(png, file = "plot2.png")
     dev.off()
}