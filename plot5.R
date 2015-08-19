setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

data <- cacheData(cacheDF)
#data <- readRDS("summarySCC_PM25.rds")

#require dplyr package
library(dplyr)
head(data)

# load Source_Classification_Code.rds
scc <- readRDS("Source_Classification_Code.rds")
vehicles <- grep("vehi", scc$EI.Sector, ignore.case = TRUE)

#filter data set by EI.Sector matched with comb & coal
vehicles.scc <- scc[vehicles, c("SCC")]
vehicles.data <- data %>% 
  filter(SCC %in% vehicles.scc & fips == "24510")
head(vehicles.data)

scc.ei.sector <- scc[, c("SCC", "EI.Sector")]
scc.data.sector <- merge(vehicles.data, scc.ei.sector, by = "SCC")
scc.data <- scc.data.sector %>% 
  group_by(year, EI.Sector) %>%  # group by year, EI.Sector
  summarise(mean.emissions = mean(Emissions, na.rm = TRUE))    #summarize average emissions ()
head(scc.data)

library(ggplot2)
p <- qplot(year , mean.emissions, col=EI.Sector, data=scc.data) +
  geom_point(aes(x=year, y= mean.emissions, col=EI.Sector), 
             size=4,shape=4, data=scc.data)  + 
  geom_smooth(method = "lm")+ 
  scale_x_continuous(breaks=unique(scc.data$year)) +
  labs(title = expression('Mobile Vehicles Average Emission of PM'[2.5] * ' in Baltimore City') ) + 
  labs(y = expression('Average Emissions PM'[2.5] * '(tons)')) +
  labs(x = "year")
plot(p)


## Copy my plot to a PNG file
dev.copy(png, file = "plot5.png", width=1024, height=768, units = "px")  
## Don't forget to close the PNG device!
dev.off()  
