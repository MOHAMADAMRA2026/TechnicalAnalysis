# BDA400 Assignment 2 - Technical Analysis Utility Script
library(quantmod)
library(TTR)

# Helper function to compute mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 1. Function to import stock data from portfolio.txt
load_stock_data <- function(file_path = "portfolio.txt") {
  symbols <- readLines(file_path)
  symbols <- trimws(symbols)
  
  stock_env <- new.env()
  getSymbols(symbols, src = "yahoo", env = stock_env, auto.assign = TRUE)
  return(stock_env)
}

# 2. Function to compute basic statistics & moving average
calculate_statistics <- function(stock_xts) {
  close_prices <- na.omit(Cl(stock_xts))
  
  # Compute 20-day Simple Moving Average (SMA)
  sma_20 <- SMA(close_prices, n = 20)
  
  stats <- list(
    Mean = mean(close_prices),
    Median = median(close_prices),
    Mode = get_mode(round(close_prices, 2)),
    Std_Dev = sd(close_prices),
    Latest_20_SMA = as.numeric(tail(sma_20, 1))
  )
  return(stats)
}

# 3. Utility function to display output & visualizations
display_portfolio_analysis <- function(file_path = "portfolio.txt") {
  stock_env <- load_stock_data(file_path)
  symbols <- ls(stock_env)
  
  for (symbol in symbols) {
    cat("\n========================================\n")
    cat("  STOCK SYMBOL:", symbol, "\n")
    cat("========================================\n")
    
    data <- stock_env[[symbol]]
    
    cat("\n--- Recent Price Data ---\n")
    print(tail(data, 5))
    
    stats <- calculate_statistics(data)
    cat("\n--- Statistical Summary ---\n")
    print(stats)
    
    # Display Chart
    chartSeries(data, name = symbol, theme = chartTheme("white"))
  }
}

# Execute the analysis
display_portfolio_analysis("portfolio.txt")

