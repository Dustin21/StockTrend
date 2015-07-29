# load dependencies
suppressPackageStartupMessages({
	library(kernlab)
	library(quantmod)
	library(plyr)
	library(dplyr)
	library(ggplot2)
})

# time-frame specification
from <- "2000-01-01"

# load data
StockData <- getSymbols("^GSPC", src = "yahoo", from = from, warnings = FALSE,
		auto.assign = getOption('getSymbols.auto.assign', FALSE))

# adjusted closing prices
close.adj <- StockData[,6]

# extract dates
dates <- time(close.adj)

# extract years from close.adj
years <- substr(dates, 1, 4)

# closing values
close <- data.frame(close = as.numeric(close.adj))

# count of number of days per year
day.count <- NULL
for(i in 1:length(unique(years))) {
	count <- 1:table(years)[[i]]
	day.count[(length(day.count) + 1):length(count)] <- count
}

# data.frame of years, number of days, and closing prices
data_by_year <- data.frame(year = as.factor(years), count = day.count, close)


