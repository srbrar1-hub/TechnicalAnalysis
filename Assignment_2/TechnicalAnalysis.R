library(quantmod)

# Load portfolio stocks
portfolio <- scan("portfolio.txt", what = "character", quiet = TRUE)

load_stock_data <- function(symbols) {
  getSymbols(symbols,
             from = "2007-01-03",
             to = "2026-10-26")
}
# Download stock data
load_stock_data(portfolio)

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


# Portfolio Performance Analysis
portfolio_returns <- rowMeans(returns)

mean(portfolio_returns)
sd(portfolio_returns)
sd(portfolio_returns) * sqrt(252)
((1 + mean(portfolio_returns))^252 - 1) * 100