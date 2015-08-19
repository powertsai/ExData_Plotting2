setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

data <- cacheData(cacheDF)
#data <- readRDS("summarySCC_PM25.rds")

#require dplyr package
library(dplyr)
head(data)

# load Source_Classification_Code.rds find SCC for Mobile Vehicles
scc <- readRDS("Source_Classification_Code.rds")
vehicles <- grep("vehi", scc$EI.Sector, ignore.case = TRUE)
vehicles.scc <- scc[vehicles, c("SCC")]

#filter data set by Mobile Vehicles and county in Baltimor and Los Angels
vehicles.data <- data %>% 
  filter(SCC %in% vehicles.scc & fips %in% c("24510", "06037") )
head(vehicles.data)

scc.ei.sector <- scc[, c("SCC", "EI.Sector")]
scc.data.sector <- merge(vehicles.data, scc.ei.sector, by = "SCC")
#require library maps to transfer fips to county name
#install.packages("maps")
library(maps)
#load data county.fips
data(county.fips)
head(county.fips)

scc.sata.county <- merge(scc.data.sector, county.fips, by="fips")
head(scc.sata.county)
head(scc.sata.county)
scc.data <- scc.sata.county %>% 
  mutate(county = polyname) %>%
  group_by(year, EI.Sector, county) %>%  # group by year, EI.Sector, county
  summarise(mean.emissions = mean(Emissions, na.rm = TRUE)) #mean emissions ()
head(scc.data)

scc.sata.county <- merge(scc.data, county.fips, by="fips")
head(scc.data.county)

#merge scc data with polyname from county.fips
scc.data.county <- merge(scc.data.sector, county.fips, by = "fips")


library(ggplot2)
p <- qplot(year , mean.emissions, col=EI.Sector, data=scc.data) +
  geom_point(aes(x=year, y= mean.emissions, col=EI.Sector), 
             size=4,shape=4, data=scc.data)  + 
  geom_smooth(method = "lm")+ 
  scale_x_continuous(breaks=unique(scc.data$year)) +
  labs(title = expression('Mobile Vehicles Average Emission of PM'[2.5] * ' in Baltimore City and Los Angeles') ) + 
  labs(y = expression('Average Emissions PM'[2.5] * '(tons)')) +
  labs(x = "year")
plot(p)


## Copy my plot to a PNG file
dev.copy(png, file = "plot6.png", width=1024, height=768, units = "px")  
## Don't forget to close the PNG device!
dev.off()  
