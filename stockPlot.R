stockPlot <- function(StockTrend, plot.type = "years", years.back = 1) {

	suppressPackageStartupMessages({
		require(ggplot2)
		require(plyr)
		require(dplyr)
	})
	
	pred <- StockTrend$Predictions
	forecast <- StockTrend$forecast_set
	train <- StockTrend$training_set
	closing.normalised <- data.frame(StockTrend$Closing.normalised, 
			year.int = as.integer((StockTrend$Closing.normalised)$year))

	if(plot.type == "years") {
		
		stock.years <- filter(closing.normalised, year.int > 
					(dim(table(year.int))-years.back)) %>%
					ggplot(aes(x = day, y = close)) +
					geom_line(aes(colour = year)) +
					theme_bw() +
					ggtitle("Normalised Closing Prices by Year") +
					xlab("Time (in days)") +
					ylab("Normalised Closing Price")
		return(stock.years)

		} else {

		if(plot.type == "trend") {

			predicted <- data.frame(year = forecast$year, day = forecast$day,
						close = pred, 
						count = (nrow(train)+1):(nrow(train)+nrow(pred)))
			
			train.count <- data.frame(train, count = 1:nrow(train))
			train.count$year <- as.integer(train.count$year)

			trend.plot <- train.count %>%
					filter(year > (dim(table(train$year))-years.back)) %>%
					ggplot(aes(x = count, y = close)) + geom_line() +
					geom_line(data = predicted, aes(x = count, y = close),
							colour = "blue") +
					theme_bw() +
					ggtitle("Stock Trend Forecast") +
					xlab("nth day in time frame") +
					ylab("Normalised Closing Price")
			return(trend.plot)

		} else {

		stop("Invalid plotting scheme! Please choose either 'years' or 'trend'.")	
		
		}
	}

}
	
