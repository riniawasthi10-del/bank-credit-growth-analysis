# ============================================================
# Macroeconomic Determinants of Bank Credit Growth — India
# 02: Stationarity Tests & OLS Regression
# Rini Awasthi | MA Economics, Madras School of Economics
# ============================================================

library(tidyverse)
library(tseries)
library(urca)
library(stargazer)

df <- read.csv("data/credit_data.csv")
df$date <- as.Date(df$date)

vars <- c("credit_growth","gdp_growth","repo_rate","cpi_inflation","m3_growth")

# ── 1. ADF Stationarity Tests ────────────────────────────────
cat("═══════════════════════════════════════════\n")
cat("  ADF STATIONARITY TESTS — LEVELS\n")
cat("═══════════════════════════════════════════\n")

adf_results <- data.frame(Variable=character(), ADF_Stat=numeric(),
                           P_Value=numeric(), Stationary=character())

for (v in vars) {
  test   <- adf.test(df[[v]], alternative = "stationary")
  status <- ifelse(test$p.value < 0.05, "Stationary ✓", "Non-stationary ✗")
  cat(sprintf("  %-20s | ADF = %6.3f | p = %.4f | %s\n",
              v, test$statistic, test$p.value, status))
  adf_results <- rbind(adf_results, data.frame(
    Variable   = v,
    ADF_Stat   = round(test$statistic, 3),
    P_Value    = round(test$p.value, 4),
    Stationary = status
  ))
}

cat("\n--- First Differences ---\n")
for (v in vars) {
  diff_series <- diff(df[[v]])
  test        <- adf.test(diff_series, alternative = "stationary")
  status      <- ifelse(test$p.value < 0.05, "Stationary ✓", "Non-stationary ✗")
  cat(sprintf("  d(%s) | ADF = %6.3f | p = %.4f | %s\n",
              v, test$statistic, test$p.value, status))
}

# ── 2. OLS Regression ────────────────────────────────────────
cat("\n═══════════════════════════════════════════\n")
cat("  OLS REGRESSION RESULTS\n")
cat("═══════════════════════════════════════════\n")

# Baseline OLS
ols1 <- lm(credit_growth ~ gdp_growth + repo_rate + cpi_inflation, data = df)

# Full model with M3
ols2 <- lm(credit_growth ~ gdp_growth + repo_rate + cpi_inflation + m3_growth, data = df)

# Lagged repo rate (monetary policy has delayed effect)
df$repo_lag2 <- c(rep(NA, 2), df$repo_rate[1:(nrow(df)-2)])
ols3 <- lm(credit_growth ~ gdp_growth + repo_lag2 + cpi_inflation + m3_growth,
           data = df, na.action = na.omit)

cat("\nModel 1 — Baseline OLS:\n"); print(summary(ols1))
cat("\nModel 2 — Full Model with M3:\n"); print(summary(ols2))
cat("\nModel 3 — Lagged Repo Rate:\n"); print(summary(ols3))

# Save regression table
stargazer(ols1, ols2, ols3,
          type = "text",
          title = "OLS Regression — Determinants of Bank Credit Growth",
          dep.var.label = "Bank Credit Growth (%)",
          covariate.labels = c("GDP Growth","Repo Rate","Repo Rate (Lag 2Q)",
                               "CPI Inflation","M3 Growth"),
          out = "outputs/02_ols_table.txt")
cat("Saved: outputs/02_ols_table.txt\n")

# ── 3. Coefficient Plot ──────────────────────────────────────
coef_df <- data.frame(
  term = names(coef(ols2))[-1],
  estimate = coef(ols2)[-1],
  se = summary(ols2)$coefficients[-1, 2]
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    significant = ifelse(abs(estimate/se) > 1.96, "Significant", "Not Significant"),
    term = recode(term,
      gdp_growth    = "GDP Growth",
      repo_rate     = "Repo Rate",
      cpi_inflation = "CPI Inflation",
      m3_growth     = "M3 Growth"
    )
  )

p <- ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate,
                          color = significant)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 1) +
  coord_flip() +
  scale_color_manual(values = c("Significant" = "#1f77b4",
                                "Not Significant" = "#aec7e8")) +
  labs(title = "OLS Coefficient Plot — Bank Credit Growth",
       subtitle = "Error bars show 95% confidence intervals",
       x = NULL, y = "Coefficient Estimate",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")

ggsave("outputs/02_ols_coefficients.png", p, width = 9, height = 5, dpi = 150)
print(p)
cat("Saved: outputs/02_ols_coefficients.png\n")

# ── 4. Actual vs Fitted ──────────────────────────────────────
df_fit <- df %>%
  mutate(fitted = fitted(ols2),
         residual = residuals(ols2))

p2 <- ggplot(df_fit, aes(x = date)) +
  geom_line(aes(y = credit_growth, color = "Actual"), size = 1) +
  geom_line(aes(y = fitted, color = "OLS Fitted"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "#1f77b4", "OLS Fitted" = "#d62728")) +
  labs(title = "Bank Credit Growth — Actual vs OLS Fitted",
       x = NULL, y = "Credit Growth (%)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        legend.position = "bottom")

ggsave("outputs/02_actual_vs_fitted.png", p2, width = 10, height = 5, dpi = 150)
print(p2)
cat("Saved: outputs/02_actual_vs_fitted.png\n")

cat(sprintf("\nOLS R-squared      : %.4f\n", summary(ols2)$r.squared))
cat(sprintf("OLS Adj R-squared  : %.4f\n", summary(ols2)$adj.r.squared))
cat("\nOLS analysis complete.\n")
