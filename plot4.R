library(dplyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds") %>% mutate(type = tolower(gsub('-', '', type)))
SCC <- readRDS("Source_Classification_Code.rds") %>% rename(type = Data.Category)
SCC$type <- tolower(SCC$type)

## filter the data to combustion-related sources
fullDescription <- paste(SCC$SCC.Level.One, SCC$SCC.Level.Two, SCC$SCC.Level.Three, SCC$SCC.Level.Four)
coalComb <- SCC[grepl('Comb', fullDescription) & grepl('Coal', fullDescription),]

## join with SCC data and summarize the data by year
summaryByYear <- NEI %>% inner_join(coalComb) %>% group_by(year) %>% summarise(pm25=sum(Emissions))
## fit a linear model to the data and plot it
model <- lm(pm25~year, summaryByYear)

## plotting
png(filename = "plot4.png", width = 640, height = 480, bg="transparent")
g <- ggplot(summaryByYear, aes(year, pm25))
g + geom_point() + 
  ggtitle(expression(atop("Total PM2.5 emissions per year", atop(italic("from coal combustion-related sources"), "")))) +
  ylab("PM2.5 (tons)") + 
  geom_smooth(method = "lm") + 
  theme_bw()
dev.off()
## Based on our model, it shows that generally, total emissions of PM2.5 is decreasing from coal combustion-related sources