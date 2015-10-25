library(dplyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## summarize the data by year and type, limit the data to Baltimore City
summaryByYear <- NEI %>% filter(fips == "24510") %>% group_by(year, type) %>% summarise(pm25=sum(Emissions))
summaryByYear$year <- as.numeric(summaryByYear$year)
## fit a linear model to the data and plot it
model <- lm(pm25~year, summaryByYear)

## plotting
png(filename = "plot3.png", width = 640, height = 480, bg="transparent")
g <- ggplot(summaryByYear, aes(year, pm25))
g + geom_point() + 
  ggtitle(expression(atop("Total PM2.5 emissions per year for Baltimore", atop(italic("by source"), "")))) +
  ylab("PM2.5 (tons)") +
  geom_hline(aes(yintercept=0), linetype="dashed", color = "red") + 
  facet_wrap(~ type, scales = "free") + 
  geom_smooth(method = "lm") + 
  theme_bw()
dev.off()
## Based on our model, point type is the only one which shows a increasing trend.
