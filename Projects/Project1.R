

# ------------------------------------------------------------------------------------------

#                             3.1.1 Basic Statistics

# ------------------------------------------------------------------------------------------

" Download JP Morgan stock historical prices from Yahoo Finance
  Period: February 1, 2018 - December 30, 2018
  Frequency: Daily
  
  Price considered in analysis: Adjusted Close"

# Libraries used

install.packages("quantmod")
library(quantmod)

# Get the daily price for JP Morgan

getSymbols("JPM", src = "yahoo", from = "2018-02-01", to = "2018-12-30")
jpm_adjusted <- JPM[,"JPM.Adjusted"]

# Verify the data

head(jpm_adjusted)

# Perform some visualizations to explore the data

plot(jpm_adjusted, main = "JP Morgan Adjusted Close", lwd = 2, col = " red")

chartSeries(JPM, type = "line", subset = "2018", theme = chartTheme("white"))

# 1.1. Get Average Stock Value

mean(jpm_adjusted)

# 1.2. Get Stock Volatility

## 231 because 252 are the normal trading days in a year but we lose one month
## as we start the analysis on February

log_returns_jpm <- periodReturn(JPM, period = "daily", type = "log", )
annualized_volatility_jpm <- sd(log_returns_jpm) * sqrt(231) * 100
daily_volatility_jpm <- sd(log_returns_jpm)


# 1.3. Get Daily Stock Return

daily_returns_jpm <- dailyReturn(jpm_adjusted)


# ------------------------------------------------------------------------------------------

#                             3.1.2 Linear Regression

# ------------------------------------------------------------------------------------------

" Implement a two-variable regression in R 
  
  Explained variable: JP Morgan stock (adjusted close price)
  Explanatory variable: S&P500"

# Get S&P 500 stock information

getSymbols("^GSPC", src = "yahoo", from = "2018-02-01", to = "2018-12-30")
sp500_adjusted <- GSPC[, "GSPC.Adjusted"]
sp500_adjusted <- as.data.frame(sp500_adjusted)
jpm_adjusted <- as.data.frame(jpm_adjusted)


# Two variable regression with S&P 500

## Regression Plot

plot(y = jpm_adjusted[,1],x=sp500_adjusted[,1], main ="Regression Between Stocks", col = "blue",
     pch = 20, xlab = "S&P 500", ylab = "JP Morgan")
abline(lm(jpm_adjusted[,1] ~ sp500_adjusted[,1]), col = "red")

## Regression Summary

model <- lm(formula = jpm_adjusted[,1] ~ sp500_adjusted[,1])
summary(model)
