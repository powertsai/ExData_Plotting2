#setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

#require dplyr package
library(dplyr)
#readRDS("summarySCC_PM25.rds") from cached for plot
#data <- cacheData(cacheDF)
data <- readRDS("summarySCC_PM25.rds")
#group by year, calculate total emission by sum
sumdata <- data %>% group_by(year) %>%
           summarise(total.emissions = sum(Emissions)) %>%
           mutate(total.emissions = total.emissions/1000000)
#plot total emmision by year
par( mar = c(4,5,2,1))
plot(sumdata, xlim = range(sumdata$year),  xaxt = "n", 
       ylim=range(sumdata$total.emissions),
       ylab=expression('Total Emission PM'[2.5] * '(million tons)') )

#Fit a line robustly
lmdata <- line(sumdata)
abline(coef(lmdata), col="red")

#place axis by year
with(sumdata, axis(1, at=year,labels=year, col.axis="black") )
## Copy my plot to a PNG file
dev.copy(png, file = "plot1.png", width=480, height=480, units = "px")  
## Don't forget to close the PNG device!
dev.off()  
