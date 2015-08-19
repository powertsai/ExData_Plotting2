#setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

#require dplyr package
library(dplyr)
#readRDS("summarySCC_PM25.rds") from cached for plot
#data <- cacheData(cacheDF)
data <- readRDS("summarySCC_PM25.rds")

#group by year, calculate total emission by sum
sumdata.maryland <- data %>% 
  filter(fips == "24510") %>% # filter Baltimore City
  group_by(year) %>%  # group by year
  summarise(total.emissions = sum(Emissions))  # summarize total emissions
summary(sumdata.maryland)
#  %>% mutate(total.emissions = total.emissions/1000000) # tranfer unit to million tons
#plot total emmision by year
par( mar = c(4,5,2,1))
plot(sumdata.maryland, xlim = range(sumdata.maryland$year),  xaxt = "n", 
     ylim=range(sumdata.maryland$total.emissions),
     ylab=expression('Total Emission PM'[2.5] * '(tons)') )

#Fit a line robustly
lmdata <- line(sumdata.maryland)
abline(coef(lmdata), col="red")

#place axis by year
with(sumdata.maryland, axis(1, at=year,labels=year, col.axis="black") )
## Copy my plot to a PNG file
dev.copy(png, file = "plot2.png", width=480, height=480, units = "px")  
## Don't forget to close the PNG device!
dev.off()  

