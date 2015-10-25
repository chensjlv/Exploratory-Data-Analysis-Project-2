library(dplyr)
library(quantreg)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

## summarize the data by year
summaryByYear <- NEI %>% group_by(year) %>% summarise(pm25=sum(Emissions))
## fit a linear model to the data and plot it
fm.orig <- lm(pm25~year, data = summaryByYear)
## fit a Quantile regression model
fm.rq <- rq(pm25~year, data = summaryByYear)

## plotting
png(filename = "plot1.png", width = 640, height = 480, bg="transparent")
plot(summaryByYear$year, summaryByYear$pm25, xlab = 'Year', ylab = 'PM2.5 (Tons)', main = "Total PM2.5 emission from all sources by year")
abline(fm.orig, col = "red", lw = 2)
abline(fm.rq, col = "blue", lw = 2)
legend("topright", legend = c('PM2.5', 'lm fit', 'rq fit'), pch = c(1, NA, NA), lty = c(NA, 1, 1), col = c("black", "red", "blue"))
dev.off()
## The result is kind of obvious. Based on our model, it shows that generally, total emissions of PM2.5 is increasing.
