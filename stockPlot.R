stockPlot <- function(StockTrend, plot.type = "years") {

	suppressPackageStartupMessages({
		require(ggplot2)
		require(plyr)
		require(dplyr)
	})
	
	pred <- StockTrend$Predictions
	forecast <- StockTrend$forecast_set
	train <- StockTrend$training_set
	closing.normalised <- StockTrend$Closing.normalised

	if(plot.type == "years") {
		
		stock.years <- ggplot(closing.normalised, aes(x = day, y = close)) +
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
					filter(year > (dim(table(train$year))-year)) %>%
					ggplot(aes(x = count, y = close)) + geom_line() +
					geom_line(data = predicted, aes(x = count, y = close),
							colour = "blue")
			return(trend.plot)

		} else {

		stop("Invalid plotting scheme! Please choose either 'years' or 'trend'.")	
		
		}
	}

}
	
