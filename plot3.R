#set work directory
#setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

#readRDS("summarySCC_PM25.rds") from cached for plot
#data <- cacheData(cacheDF)
data <- readRDS("summarySCC_PM25.rds")

#require dplyr package
library(dplyr)
#group by year, calculate total emission by sum
sum.type.maryland <- data %>% 
  filter(fips == "24510") %>% # filter Baltimore City
  group_by(year, type) %>%  # group by year
  summarise(total.emissions = sum(Emissions))  # summarize total emissions

#require ggplot2 package
library(ggplot2)
p <- qplot(year , total.emissions, col=type, data=sum.type.maryland) +
     geom_point(aes(x=year, y= total.emissions, col=type), 
                size=6,shape=4, data=sum.type.maryland)  + 
     geom_smooth(method = "lm")+ 
     scale_x_continuous(breaks=unique(sum.type.maryland$year)) +
     labs(title = expression('Total Emission of PM'[2.5] * ' in the Baltimore City') ) + 
     labs(y = expression('Total Emissions PM'[2.5] * '(tons)')) +
     labs(x = "year")
plot(p)

## Copy my plot to a PNG file
dev.copy(png, file = "plot3.png", width=480, height=480, units = "px")  
## Don't forget to close the PNG device!
dev.off()  

