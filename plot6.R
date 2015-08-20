setwd("/Users/hadoop/Dropbox/R/ExploratoryData/CourseProject2")

# cached data set for summarySCC_PM25.rds
source("CourseProject2.R")
cacheDF <- makeCacheDataFrame("summarySCC_PM25.rds")
data <- cacheData(cacheDF)
#data <- readRDS("summarySCC_PM25.rds")

#require dplyr package
library(dplyr)
head(data)

# load Source_Classification_Code.rds find SCC for Mobile Vehicles
scc <- readRDS("Source_Classification_Code.rds")
vehicles <- grep("vehi", scc$EI.Sector, ignore.case = TRUE)
scc.vehicles <- scc[vehicles, ]
scc.vehicles <- scc.vehicles[,c(1:4,7:10)]
scc.vehicles$SCC <- as.character(scc.vehicles$SCC)

#require library maps to transfer fips to county name
#install.packages("maps")
library(maps)
#load data county.fips
data(county.fips)
head(county.fips)

library(tidyr)
#filter data set by Mobile Vehicles and county in Baltimor and Los Angels
vehicles.scc <- scc[vehicles, c("SCC")]
vehicles.data <- data %>% 
  filter(SCC %in% vehicles.scc & fips %in% c("24510", "06037") ) %>%
  mutate(fips = as.numeric(fips))  %>%
  inner_join(scc.vehicles, by= c("SCC" = "SCC")) %>%
  inner_join(county.fips, by = c("fips" = "fips")) %>%
  separate(polyname, c("state", "city"), sep = ",") %>%
  arrange(desc(Emissions))
head(vehicles.data,100)

library(ggplot2)
#library(plyr)
ggplot(vehicles.data, aes(x=EI.Sector, y=log(Emissions), fill =SCC.Level.Three))+
  geom_boxplot()+
  facet_grid(state ~ year)+
  labs(x="EI.Sector")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))



#function create plot (mean emissoin ~ year) by input EI.Sector facet by SCC.Level.Three
myplot <- function(data = vehicles.data, sector) {
  vehicles.data.stat <- data %>% 
    filter(EI.Sector %in%  c(sector)) %>%
    group_by(year,  SCC.Level.Three, city) %>%  # group by year, EI.Sector, county
    summarise(mean.emissions = mean(Emissions, na.rm = TRUE))
  print(head(vehicles.data.stat))
  p <- qplot(year , mean.emissions, col=city, data=vehicles.data.stat) +
    geom_point(aes(x=year, y= mean.emissions, col=city), 
               size=4,shape=4, data=vehicles.data.stat)  + 
    geom_smooth(method = "lm")+ 
    facet_grid( .~ SCC.Level.Three)+
    scale_x_continuous(breaks=unique(vehicles.data.stat$year)) +
    labs(title = substitute( a * ' Average Emission of PM'[2.5] , list(a = sector))  ) + 
    labs(y = expression('Average Emissions PM'[2.5] * '(tons)')) +
    labs(x = "year")
}

#[1] Mobile - On-Road Diesel Heavy Duty Vehicles
p1 <- myplot(vehicles.data, "Mobile - On-Road Diesel Heavy Duty Vehicles")
plot(p1)
#[2] Mobile - On-Road Diesel Light Duty Vehicles  
p2 <- myplot(vehicles.data, "Mobile - On-Road Diesel Light Duty Vehicles")
plot(p2)
#[3] Mobile - On-Road Gasoline Heavy Duty Vehicles
p3 <- myplot(vehicles.data, "Mobile - On-Road Gasoline Heavy Duty Vehicles")
plot(p3)
#[4] Mobile - On-Road Gasoline Light Duty Vehicles
p4 <- myplot(vehicles.data, "Mobile - On-Road Gasoline Light Duty Vehicles")
plot(p4)

#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#create plot 
png.device <- png(file = "plot6.png", width=1024, height=768, units = "px")
multiplot(p1,p4, p2, p3, cols=1)
## Don't forget to close the PNG device!
dev.off()  
