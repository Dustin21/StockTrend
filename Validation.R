Validate <- function(x) {
#------------------------------------------------------------------------------------
# A quasi cross-validation algorithm for the model StockTrend. The validation approach
# starts by taking the first two years of the data set as the trainings set. The 
# remaining years serve as multiple test sets of 50 days each. The algorithm iteratively
# trains and predicts the next 50 days, then retrains on those 50 days and predicts
# the following 50 days, etc., then calculates the % of times the trend was correct.
#
# 2015 v.0.0.1 
#------------------------------------------------------------------------------------
	
	# train and test data
	train <- filter(x, year %in% unique(x$year)[1:2])
	testset <- filter(x, !(year %in% unique(x$year)[1:2]))
	
	# split test data into 50-day chunks
	test <- split(testset, (seq(length(testset$close))-1) %/% 50) 
	
	# number of iterations over test data
	n <- length(test)	

	# iterate model predictions over each test chunk
	validation_iterate <- foreach(m = 1:n, .combine = rbind) %do% {

		train_update <- train

		# coerce data into matrix format
		x.train <- cbind(as.integer(train_update$year), as.numeric(train_update$day))
		y.train <- as.numeric(train_update$close)
		
		x.test <- cbind(as.integer(test[[m]]$year), as.numeric(test[[m]]$day))
		y.test <- as.numeric(test[[m]]$close)		
		
		# fit GP regression model
		model <- gausspr(x = x.train, y = y.train)
		
		# predict from test data chunk
		predict <- predict(model, newdata = x.test, type = "response")
		
		# prepare training set for following 50 predictions
		train <- rbind_list(train_update, test[[m]]) 

		return(list(model, predict, y.test))
	}	 

	return(validation_iterate)

}	
