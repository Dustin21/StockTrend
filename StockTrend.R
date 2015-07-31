stockTrend <- function(ticker = "AAPL", from = "2010-01-01", days_ahead = 100, ...) {

	 
	# load dependencies
	suppressPackageStartupMessages({
		require(kernlab)
		require(quantmod)
		require(plyr)
		require(dplyr)
		require(ggplot2)
		require(foreach)
		require(reshape2)
		options("getSymbols.warning4.0"= FALSE)
	})
	
	# load data
	StockData <- getSymbols(ticker, src = "yahoo", from = from, warnings = FALSE,
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
		day.count[length(day.count) + 1:length(count)] <- count
	}

	# data.frame of years, number of days, and closing prices
	data_by_year <- data.frame(year = as.factor(years), count = day.count, close)

	# remove NA values
	remove_NA <- dcast(data_by_year, count ~ year, value.var = 'close') %>%
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

	# training set
	train <- select(close.normalised, year, day, close)

	x.train <- cbind(as.integer(train$year), as.numeric(train$day))
	y.train <- as.numeric(train$close)

	# forecast period
	forecast <- data.frame(

			year = if(
				(table(train$year)[[dim(table(train$year))-1]] -
				table(train$year)[[dim(table(train$year))]]) >= days_ahead
				) { 
				
				rep(dim(table(train$year)), days_ahead)

				} else {
							
				c(
				rep(dim(table(train$year)), 
					(table(train$year)[[dim(table(train$year))-1]] -
					table(train$year)[[dim(table(train$year))]])),

				rep(dim(table(train$year))+1, 
					(days_ahead-(table(train$year)[[dim(table(train$year))-1]] -
					table(train$year)[[dim(table(train$year))]])))
				)},
	 
			day = if(
				(table(train$year)[[dim(table(train$year))-1]] - 
				table(train$year)[[dim(table(train$year))]]) >= days_ahead
				) {

				(table(train$year)[[dim(table(train$year))]] + 1):
				(days_ahead + table(train$year)[[dim(table(train$year))]])
				
				} else {
				
					if(table(train$year)[[dim(table(train$year))]] !=
					table(train$year)[[dim(table(train$year))-1]]) {

					c(
					table(train$year)[[dim(table(train$year))]]:
					table(train$year)[[dim(table(train$year))-1]],

					1:(days_ahead - length(table(train$year)[[dim(table(train$year))]]:
					table(train$year)[[dim(table(train$year))-1]]))
					
					)} else {1:days_ahead}
				}
			)
			
			

	# model the data with a GP regression
	mod <- gausspr(x = x.train, y = y.train)

	# predict the next period
	pred <- predict(mod, newdat = forecast, type = "response")


	return(list(Predictions = pred, Model.output = mod, forecast_set = forecast,
			training_set = train, Closing.normalised = close.normalised))
}	
