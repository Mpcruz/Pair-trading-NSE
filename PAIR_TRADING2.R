rm(list=ls())
library(stringr)
library(TTR)
library(curl)
library("quantmod")


# Use getSymbols to import the data
IGL <- na.omit(getSymbols('IGL.NS', src="yahoo", from = '2013-01-01',
                          to = '2018-02-28', adjust = T, auto.assign = FALSE))

GAIL <- na.omit(getSymbols('GAIL.NS', src="yahoo", from = '2013-01-01',
                           to = '2018-02-28', adjust = T, auto.assign = FALSE))

Sys.setenv(TZ = "UTC")


# Function to calculate the spread
calculate_spread <- function(x, y, beta) {
  return(y - beta * x)
}

# Function to calculate the beta and level
# given start and end dates
calculate_beta_and_level <- function(x, y,
                                     start_date, end_date) {
  require(xts)
  
  time_range <- paste(start_date, "::",
                      end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  
  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp( ~ dx + dy)
  
  beta <- r$rotation[2, 1] / r$rotation[1, 1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  
  return(outL)
}

# Function to calculate buy and sell signals
# with upper and lower threshold
calculate_buy_sell_signals <- function(spread, beta,
                                       level, lower_threshold, upper_threshold) {
  
  buy_signals <- ifelse(spread <= level -
                          lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level +
                           upper_threshold, 1, 0)
  
  # bind these vectors into a matrix
  output <- cbind(spread, buy_signals,
                  sell_signals)
  colnames(output) <- c("spread", "buy_signals",
                        "sell_signals")
  
  return(output)
}

# Implementation
# Pick an in-sample date range // Backtesting

start_date <- "2017-01-01"
end_date <- "2017-12-31"

x <- IGL[, 6]
y <- GAIL[, 6]

calc_insample_data <- calculate_beta_and_level(x,y,start_date,end_date) 

calc_insample_data$spread
threshold <- sd(calc_insample_data$spread, na.rm = TRUE)

upper_threshold = threshold
lower_threshold = threshold

buy_sell <- calculate_buy_sell_signals(calc_insample_data$spread, calc_insample_data$beta,
                                       calc_insample_data$level,lower_threshold
                                       ,upper_threshold)

plot(calc_insample_data$spread, main = "GAIL - beta * IGL",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = calc_insample_data$level, lwd = 5)
abline(h = calc_insample_data$level+upper_threshold, lwd = 2)
abline(h = calc_insample_data$level-lower_threshold, lwd = 2)
abline(h = calc_insample_data$level+2*upper_threshold, lwd = 2)
abline(h = calc_insample_data$level-2*lower_threshold, lwd = 2)


#out of sample range // Forward testing

start_date_outof_sample <- "2018-01-01"
end_date_outof_sample <- "2018-02-28"

x <- IGL[, 6]
y <- GAIL[, 6]


calc_outofsample_data <- calculate_beta_and_level(x,y,start_date_outof_sample,end_date_outof_sample)

buy_sell_outof_range <- calculate_buy_sell_signals(calc_outofsample_data$spread, calc_insample_data$beta,
                                       calc_insample_data$level,lower_threshold
                                       ,upper_threshold)

# Interpretation
# Sell IGL Futures 340 - Close 304.7  -- 2750 (Lot size)
# Buy Gail Futures 510 - close 457 -- 2000 (Lot size)
# buy 1 lot of GAIL and sell 3 lots of IGL on 1 Jan 2018(since beta ~4)










