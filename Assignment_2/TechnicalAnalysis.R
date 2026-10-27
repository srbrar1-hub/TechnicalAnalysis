library(quantmod)

# Load portfolio stocks
portfolio <- c("AAPL", "MSFT", "NVDA")

# Download stock data
getSymbols(portfolio,
           from = "2007-01-03",
           to = "2026-10-26")

# Create stock data
stock_data <- merge(Cl(AAPL), Cl(MSFT), Cl(NVDA))
colnames(stock_data) <- portfolio

# Daily returns
returns <- na.omit(ROC(stock_data, type = "discrete"))

# Descriptive statistics
summary(returns)

# Correlation
cor(returns)

# 20-day Simple Moving Average charts
quantmod::chartSeries(AAPL, theme = quantmod::chartTheme("white"), TA = "addSMA(n = 20)")

quantmod::chartSeries(MSFT, theme = quantmod::chartTheme("white"), TA = "addSMA(n = 20)")

quantmod::chartSeries(NVDA, theme = quantmod::chartTheme("white"), TA = "addSMA(n = 20)")