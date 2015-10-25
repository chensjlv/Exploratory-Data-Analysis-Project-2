library(dplyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds") %>% mutate(type = tolower(gsub('-', '', type)))
SCC <- readRDS("Source_Classification_Code.rds") %>% rename(type = Data.Category)
SCC$type <- tolower(SCC$type)

## filter the data to vehicle sources
fullDescription <- paste(SCC$SCC.Level.One, SCC$SCC.Level.Two, SCC$SCC.Level.Three, SCC$SCC.Level.Four)
motorV <- SCC[grepl('Motorcycles', fullDescription) | grepl('Motor Vehicle', fullDescription),]

## join with SCC data, limit the data to Baltimore City and LA, and summarize the data by year and city
summaryByYear <- NEI %>% filter(fips == "06037" | fips == "24510") %>% inner_join(motorV) %>% group_by(year, fips) %>% summarise(pm25=sum(Emissions))
## label the city
summaryByYear$City <- sapply(summaryByYear$fips, function(x) if(x == "06037") "Los Angeles County" else "Baltimore City")
## fit a linear model to the data and plot it
model <- lm(pm25~year, summaryByYear)

# plotting
png(filename = "plot6.png", width = 640, height = 480, bg="transparent")
g <- ggplot(summaryByYear, aes(year, pm25, color = City))
g + geom_point() + 
  ggtitle(expression(atop("Total PM2.5 emissions per year", atop(italic("motor vehicle sources"), "")))) +
  ylab("PM2.5 (tons)") + 
  geom_smooth(method = "lm", linetype = "dashed") + 
  theme_bw()
dev.off()
## Based on our model, it shows that Los Angeles County has greater changes over time.
