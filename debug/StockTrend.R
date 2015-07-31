# load dependencies
suppressPackageStartupMessages({
	library(kernlab)
	library(quantmod)
	library(plyr)
	library(dplyr)
	library(ggplot2)
	library(foreach)
	library(reshape2)
	options("getSymbols.warning4.0"= FALSE)
})

# time-frame specification
from <- "2010-01-01"

# load data
StockData <- getSymbols("TSLA", src = "yahoo", from = from, warnings = FALSE,
		auto.assign = getOption('getSymbols.auto.assign', FALSE), ...)

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
	day.count[length(day.count) + 1:length(count)] <- count
}

# data.frame of years, number of days, and closing prices
data_by_year <- data.frame(year = as.factor(years), count = day.count, close)

# remove NA values
remove_NA <- dcast(data_by_year, count ~ year) %>%
		na.omit()

# recompute number of days per year after the NA removal
day.count <- NULL
for(i in 1:length(unique(years))) {
	count <- 1:table(data_by_year$year)[[i]]
	day.count[length(day.count) + 1:length(count)] <- count
}

# data.frame years, closing prices, day in year
data_final <- data.frame(data_by_year, day = day.count)

# normalise closing prices
close.normalised <- data_final %>%
	mutate(close = (close - mean(close))/sd(close)) %>%
	group_by(day, year)

# normalise prices over each year
year.norm <- NULL
for(i in 1:length(unique(years))) {
	year.norm[i] <- filter(close.normalised, year == unique(years)[i] & day == 1)$close
}

for(j in 1:length(unique(years))) {
	close.normalised$close[which(
		close.normalised$year == as.factor(
		unique(years))[j])] <- close.normalised$close[which(
		close.normalised$year == as.factor(unique(years))[j])] - year.norm[j]
}


# plot stocks by year
stockPlot <- ggplot(close.normalised, aes(x = day, y = close)) +
		geom_line(aes(colour = year))
print(stockPlot)

# training set
train <- select(close.normalised, year, day, close)

x.train <- cbind(as.integer(train$year), as.numeric(train$day))
y.train <- as.numeric(train$close)

day_ahead <- 100

# forecast period
forecast <- data.frame(

		year = if(
			(table(train$year)[[dim(table(train$year))-1]] -
			table(train$year)[[dim(table(train$year))]]) >= day_ahead
			) { 
			
			rep(dim(table(train$year)), day_ahead)

			} else {
						
			c(
			rep(dim(table(train$year)), 
				(table(train$year)[[dim(table(train$year))-1]] -
				table(train$year)[[dim(table(train$year))]])),

			rep(dim(table(train$year))+1, 
				(day_ahead-(table(train$year)[[dim(table(train$year))-1]] -
				table(train$year)[[dim(table(train$year))]])))
			)},
 
		day = if(
			(table(train$year)[[dim(table(train$year))-1]] - 
			table(train$year)[[dim(table(train$year))]]) >= day_ahead
			) {

			(table(train$year)[[dim(table(train$year))]] + 1):
			(day_ahead + table(train$year)[[dim(table(train$year))]])
			
			} else {
			
				if(table(train$year)[[dim(table(train$year))]] !=
				table(train$year)[[dim(table(train$year))-1]]) {

				c(
				table(train$year)[[dim(table(train$year))]]:
				table(train$year)[[dim(table(train$year))-1]],

				1:(day_ahead - length(table(train$year)[[dim(table(train$year))]]:
				table(train$year)[[dim(table(train$year))-1]]))
				
				)} else {1:day_ahead}
			}
		)
		
		

# model the data with a GP regression
mod <- gausspr(x = x.train, y = y.train)

# predict the next period
pred <- predict(mod, newdat = forecast, type = "response")

# plot the forecasted trend
predicted <- data.frame(year = forecast$year, day = forecast$day, 
		close = pred, count = (nrow(train)+1):(nrow(train)+nrow(pred)))
train2 <- data.frame(train, count = 1:nrow(train))
train2$year <- as.integer(train2$year)
plot_trend <- train2 %>%
	filter(year > (dim(table(train$year))-2)) %>%
	ggplot(aes(x = count, y = close)) + geom_line() +
	geom_line(data = predicted, aes(x = count, y = close), colour = "blue")
print(plot_trend)
