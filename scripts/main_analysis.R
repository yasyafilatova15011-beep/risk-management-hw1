# 1. Встановлення та підключення пакетів
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}

if (!requireNamespace("PerformanceAnalytics", quietly = TRUE)) {
  install.packages("PerformanceAnalytics")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
# 2. Завантаження котирувань за останні 3 роки
end_date <- Sys.Date()
start_date <- seq(end_date, by = "-3 years", length.out = 2)[2]

aapl_data <- getSymbols(
  "AAPL", src = "yahoo",
  from = start_date, to = end_date,
  auto.assign = FALSE
)

sp500_data <- getSymbols(
  "^GSPC", src = "yahoo",
  from = start_date, to = end_date,
  auto.assign = FALSE
)

# Збереження котирувань
write.zoo(aapl_data, "data/AAPL_prices.csv", sep = ",")
write.zoo(sp500_data, "data/SP500_prices.csv", sep = ",")

# Перевірка завантажених даних
head(aapl_data)
head(sp500_data)
# 3. Скориговані ціни закриття
prices <- merge(Ad(aapl_data), Ad(sp500_data), all = FALSE)
colnames(prices) <- c("AAPL", "SP500")

# Логарифмічні дохідності та видалення NA
returns <- na.omit(Return.calculate(prices, method = "log"))

# Збереження і перевірка
write.zoo(returns, "data/log_returns.csv", sep = ",")
head(returns)
colSums(is.na(returns))
nrow(returns)
# 4. Розрахунок VaR та ES
risk_table <- do.call(rbind, lapply(colnames(returns), function(asset) {
  r <- returns[, asset]
  
  data.frame(
    Asset = asset,
    Historical_VaR_95 = -as.numeric(
      VaR(r, p = 0.95, method = "historical", invert = TRUE)
    ) * 100,
    Historical_VaR_99 = -as.numeric(
      VaR(r, p = 0.99, method = "historical", invert = TRUE)
    ) * 100,
    Normal_VaR_95 = -as.numeric(
      VaR(r, p = 0.95, method = "gaussian", invert = TRUE)
    ) * 100,
    Normal_VaR_99 = -as.numeric(
      VaR(r, p = 0.99, method = "gaussian", invert = TRUE)
    ) * 100,
    Historical_ES_95 = -as.numeric(
      ES(r, p = 0.95, method = "historical", invert = TRUE)
    ) * 100
  )
}))

print(risk_table, digits = 4, row.names = FALSE)
write.csv(risk_table, "data/risk_metrics.csv", row.names = FALSE)
# 5. Гістограми дохідностей із лініями VaR
plot_data <- data.frame(
  Asset = rep(colnames(returns), each = nrow(returns)),
  Return_pct = c(
    as.numeric(returns[, "AAPL"]),
    as.numeric(returns[, "SP500"])
  ) * 100
)

var_lines <- do.call(rbind, lapply(seq_len(nrow(risk_table)), function(i) {
  data.frame(
    Asset = risk_table$Asset[i],
    Method = c("Historical", "Historical", "Normal", "Normal"),
    Confidence = c("95%", "99%", "95%", "99%"),
    Threshold = -c(
      risk_table$Historical_VaR_95[i],
      risk_table$Historical_VaR_99[i],
      risk_table$Normal_VaR_95[i],
      risk_table$Normal_VaR_99[i]
    )
  )
}))

var_chart <- ggplot(plot_data, aes(x = Return_pct)) +
  geom_histogram(bins = 50, fill = "lightblue", colour = "white") +
  geom_vline(
    data = var_lines,
    aes(xintercept = Threshold,
        colour = Confidence, linetype = Method),
    linewidth = 0.8
  ) +
  facet_wrap(~Asset, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("95%" = "orange", "99%" = "red")) +
  labs(
    title = "Daily log returns and VaR",
    x = "Daily log return (%)",
    y = "Number of days"
  ) +
  theme_minimal()

print(var_chart)

ggsave(
  "data/var_chart.png",
  plot = var_chart,
  width = 10, height = 8, dpi = 150
)