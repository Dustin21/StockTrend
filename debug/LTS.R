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
	day.count[length(day.count) + 1:length(count)] <- count
}

# data.frame of years, number of days, and closing prices
data_by_year <- data.frame(year = as.factor(years), count = day.count, close)

# remove NA values
remove_NA <- dcast(data_by_year, count ~ year) %>%
		na.omit() %>%
		select(-count) %>%
		melt(value.name = "close", variable.name = "year")

# recompute number of days per year after the NA removal
day.count <- NULL
for(i in 1:length(unique(years))) {
	count <- 1:table(remove_NA$year)[[i]]
	day.count[length(day.count) + 1:length(count)] <- count
}

# data.frame years, closing prices, day in year
data_final <- data.frame(remove_NA, day = day.count)

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

#******************* model validation

train <- filter(close.normalised, year %in% unique(close.normalised$year)[1:2])
test <- filter(close.normalised, !(year %in% unique(close.normalised$year)[1:2]))
test2 <- split(test, (seq(length(test$close))-1) %/% 50)

x.train <- cbind(as.integer(train$year), as.numeric(train$day)) 
y.train <- as.numeric(train$close)

x.test <- cbind(as.integer(test$year), as.numeric(test$day)) 
y.test <- as.numeric(test$close)

cool <- gausspr(x=x.train, y=y.train)

rbind_list(train, test2[[1]])






# training set
close.train <- close.normalised[1:(dim(close.normalised)[1] - 200),]
close.train <- cbind(as.integer(close.train$year), 
		as.numeric(close.train$close), as.integer(close.train$day))
colnames(close.train) <- c("year", "close", "day")

# test set
close.test <- close.normalised[(dim(close.normalised)[1]-199):dim(close.normalised)[1],]
close.test <- cbind(as.integer(close.test$year), 
		as.numeric(close.test$close), as.integer(close.test$day))
colnames(close.test) <- c("year", "close", "day")

# split test set into groups of 50
x.test <- split(data.frame(close.test[,-2]), (seq(nrow(close.test))-1) %/% 50)
y.test <- split(close.test[,2], (seq(length(close.test[,2]))-1) %/% 50)

for(i in 1:length(x.test)) {
	x.test[[i]] <- cbind(x.test[[i]]$year, x.test[[i]]$day)
	colnames(x.test[[i]]) <- c("year", "day")
}

# iterate model prediction over concecutive test sets
n <- length(x.test)	
validation <- foreach(m = 1:n, .combine = rbind) %do% {
	mod <- gausspr(x = x.train, y = y.train)
	pred <- predict(mod, newdata = x.test[[m]])
	x.train <- rbind(x.train, x.test[[m]])
	y.train <- rbind(y.train, y.test[[m]]) 
	return(list(mod, pred))
}
	
validate <- foreach(m = 1:n, .combine = rbind) %do% {
	mod <- gausspr(x = x.train




