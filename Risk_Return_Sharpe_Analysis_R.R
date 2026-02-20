# ==============================
# Finance Project in R
# ==============================

print("Finance Project Started")

# Investment details
assets <- c("Equity", "Debt", "Gold")
invested <- c(60000, 30000, 10000)
current  <- c(78000, 33000, 14000)

portfolio <- data.frame(assets, invested, current)
portfolio

portfolio$absolute_return <- portfolio$current - portfolio$invested
portfolio$return_percent <- (portfolio$absolute_return / portfolio$invested) * 100

portfolio

total_invested <- sum(portfolio$invested)
total_current <- sum(portfolio$current)

portfolio_return <- ((total_current - total_invested) / total_invested) * 100

total_invested
total_current
portfolio_return

# Monthly returns (%)
equity_returns <- c(2.5, -1.2, 3.8, 4.1, -0.5, 2.9)
debt_returns   <- c(0.6, 0.5, 0.7, 0.6, 0.4, 0.5)
gold_returns   <- c(1.2, -0.8, 2.0, 1.5, -1.1, 1.8)

sd(equity_returns)
sd(debt_returns)
sd(gold_returns)

sd(equity_returns) * sqrt(12)
sd(debt_returns) * sqrt(12)
sd(gold_returns) * sqrt(12)

risk <- c(
  sd(equity_returns) * sqrt(12),
  sd(debt_returns) * sqrt(12),
  sd(gold_returns) * sqrt(12)
)

barplot(risk,
        names.arg = c("Equity", "Debt", "Gold"),
        col = "steelblue",
        main = "Annualized Risk Comparison",
        ylab = "Volatility")

# ==============================
# Sharpe Ratio Calculation
# ==============================

# Risk-free rate (annual, %)
risk_free_rate <- 4

# Average monthly returns
avg_equity <- mean(equity_returns)
avg_debt   <- mean(debt_returns)
avg_gold   <- mean(gold_returns)

# Annualized returns
annual_equity_return <- avg_equity * 12
annual_debt_return   <- avg_debt * 12
annual_gold_return   <- avg_gold * 12

# Annualized volatility
equity_risk <- sd(equity_returns) * sqrt(12)
debt_risk   <- sd(debt_returns) * sqrt(12)
gold_risk   <- sd(gold_returns) * sqrt(12)

# Sharpe Ratio
sharpe_equity <- (annual_equity_return - risk_free_rate) / equity_risk
sharpe_debt   <- (annual_debt_return - risk_free_rate) / debt_risk
sharpe_gold   <- (annual_gold_return - risk_free_rate) / gold_risk

sharpe_equity
sharpe_debt
sharpe_gold

sharpe <- c(sharpe_equity, sharpe_debt, sharpe_gold)

barplot(sharpe,
        names.arg = c("Equity", "Debt", "Gold"),
        col = "darkblue",
        main = "Sharpe Ratio Comparison",
        ylab = "Sharpe Ratio")