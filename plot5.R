library(dplyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds") %>% mutate(type = tolower(gsub('-', '', type)))
SCC <- readRDS("Source_Classification_Code.rds") %>% rename(type = Data.Category)
SCC$type <- tolower(SCC$type)

## filter the data to vehicle sources
fullDescription <- paste(SCC$SCC.Level.One, SCC$SCC.Level.Two, SCC$SCC.Level.Three, SCC$SCC.Level.Four)
motorV <- SCC[grepl('Motorcycles', fullDescription) | grepl('Motor Vehicle', fullDescription),]

## join with SCC data, limit the data to Baltimore City and summarize the data by year
fire <- NEI %>% filter(fips == "24510") %>% inner_join(motorV) %>% filter(Short.Name == 'Motor Vehicle Fires /Unspecified') %>% group_by(year) %>% summarise(pm25=sum(Emissions))
fire$reason <- 'Motor Vehicle Fires'
summaryByYear <- NEI %>% filter(fips == "24510") %>% inner_join(motorV) %>% filter(Short.Name != 'Motor Vehicle Fires /Unspecified') %>% group_by(year) %>% summarise(pm25=sum(Emissions))
summaryByYear$reason <- 'Other'
summaryByYear <- rbind(fire, summaryByYear)
## fit a linear model to the data and plot it
model <- lm(pm25~year, summaryByYear)

## plotting
png(filename = "plot5.png", width = 640, height = 480, bg="transparent")
g <- ggplot(summaryByYear, aes(year, pm25, color = reason))
g + geom_point(size = 5) + 
  ggtitle(expression(atop("Total PM2.5 emissions per year for Baltimore City", atop(italic("motor vehicle sources"), "")))) +
  ylab("PM2.5 (tons)") + 
  geom_hline(aes(yintercept=0), linetype="dashed", color = "red") + 
  geom_smooth(method = "lm") + 
  theme_bw()
dev.off()
## Based on our linear model, the PM2.5 emission stays stable. The main reason it peaked in 2002 and 2005 was because vehicle fires caused by accident
