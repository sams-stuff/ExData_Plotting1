firstplot <- function() {
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
     par(no.readonly = TRUE)
     graphic_default <- par()
     par(graphic_default)
     
     # generate the histogram of plot1
     hist(data2$Global_active_power, # use global active power
          col = "red", # make the bars of the histogram red
          main = "Global Active Power", # label the main title of the plot
          cex.main = 0.9, # adjust the main title font size as desired
          xlab = "Global Active Power (kilowatts)", # label the x-axis of the plot
          cex.lab = 0.7, # adjust the x- and y- label font size as desired
          freq = TRUE, # plot the counts of global active power
          axes = FALSE # generate the axes ticks separately below
          )
     
     # set the tick marks and magnification of axis annotation for each axis
     axis(2, at=seq(0, 1200, 200), cex.axis = 0.7)
     axis(1, at=seq(0, 6, 2), cex.axis = 0.7)
     
     # copy the graphic to a png device called plot1.png
     dev.copy(png, file = "plot1.png")
     dev.off()
}