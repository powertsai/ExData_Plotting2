setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

data <- cacheData(cacheDF)
#data <- readRDS("summarySCC_PM25.rds")

#require dplyr package
#install.packages("dplyr")
library(dplyr)
head(data)
table(data$year)
# load Source_Classification_Code.rds
scc <- readRDS("Source_Classification_Code.rds")
coal <- grep("coal", scc$EI.Sector, ignore.case = TRUE)
comb <- grep("comb", scc$EI.Sector, ignore.case = TRUE)
both <- intersect(coal, comb)
head(scc)

#filter data set by EI.Sector matched with comb & coal
comb.coal.scc <- scc[both, c("SCC")]
coal.comb.data <- data %>% 
      filter(SCC %in% comb.coal.scc)
head(coal.comb.data)
scc.ei.sector <- scc[, c("SCC", "EI.Sector")]
scc.data.sector <- merge(coal.comb.data, scc.ei.sector, by = "SCC")
dim(scc.data.sector)
head(scc.data.sector)

#require library maps to transfer fips to county name
#install.packages("maps")
library(maps)
#load data county.fips
data(county.fips)
head(county.fips)

#merge scc data with polyname from county.fips
scc.data.county <- merge(scc.data.sector, county.fips, by = "fips")
head(scc.data.county)
#require tidyr to separate fips polyname into state, city
#install.packages("tidyr")
library(tidyr)
scc.data.state <- scc.data.county %>% 
       separate(polyname, c("state", "city"), sep = ",") %>%
       select(c(3:8)) %>%
       group_by(year, EI.Sector, state) %>%  # group by year, EI.Sector, state
       summarise(mean.emissions = mean(Emissions, na.rm = TRUE))    #summarize average emissions ()

head(scc.data.state)
table(scc.data.state$year )
scc.data <- filter(scc.data.state, state %in% 
                     c("maryland", "new york", 
                       "texas",  "florida" ,
                       "north carolina", "south carolina", 
                       "maine", "washington") 
                   #& EI.Sector %in% c("Fuel Comb - Electric Generation - Coal", "Fuel Comb - Comm/Institutional - Coal")
                   ) 
             

library(ggplot2)

p <- qplot(year , mean.emissions, col=state, data=scc.data, facets = . ~ EI.Sector) +
  geom_point(aes(x=year, y= mean.emissions, col=state), 
             size=4,shape=4, data=scc.data)  + 
  geom_smooth(method = "lm")+ 
  scale_x_continuous(breaks=unique(scc.data$year)) +
  labs(title = expression('Aveage Emission of PM'[2.5] * ' of EI.Sector at different states') ) + 
  labs(y = expression('Average Emissions of PM'[2.5] * ' (tons)')) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        legend.text = element_text(vjust=0.5, size=8))  +
  labs(x = "year")
plot(p)

## Copy my plot to a PNG file
dev.copy(png, file = "plot4.png", width=1024, height=768, units = "px")  
## Don't forget to close the PNG device!
dev.off()  
