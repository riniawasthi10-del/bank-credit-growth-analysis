# ============================================================
# Macroeconomic Determinants of Bank Credit Growth — India
# 03: VAR Model & Impulse Response Analysis
# Rini Awasthi | MA Economics, Madras School of Economics
# ============================================================

library(tidyverse)
library(vars)
library(tseries)

df <- read.csv("data/credit_data.csv")
df$date <- as.Date(df$date)

# ── 1. First difference all variables ────────────────────────
df_diff <- df %>%
  mutate(across(-date, ~ c(NA, diff(.)))) %>%
  drop_na()

cat("Observations after differencing:", nrow(df_diff), "\n")

# ── 2. Lag Selection ─────────────────────────────────────────
var_data <- df_diff[, c("credit_growth","gdp_growth","repo_rate",
                         "cpi_inflation","m3_growth")]

cat("\n═══════════════════════════════════════════\n")
cat("  VAR LAG SELECTION\n")
cat("═══════════════════════════════════════════\n")
lag_select <- VARselect(var_data, lag.max = 8, type = "const")
print(lag_select$criteria)
optimal_lag <- lag_select$selection["AIC(n)"]
cat(sprintf("\nOptimal lag (AIC): %d\n", optimal_lag))

# ── 3. Fit VAR Model ─────────────────────────────────────────
var_model <- VAR(var_data, p = optimal_lag, type = "const")
cat("\nVAR Model Summary:\n")
print(summary(var_model))

# Stability check
roots <- roots(var_model)
cat(sprintf("\nVAR Stability: All roots inside unit circle = %s\n",
            all(roots < 1)))

# ── 4. Granger Causality Tests ───────────────────────────────
cat("\n═══════════════════════════════════════════\n")
cat("  GRANGER CAUSALITY TESTS\n")
cat("  (Does X Granger-cause Credit Growth?)\n")
cat("═══════════════════════════════════════════\n")

for (v in c("gdp_growth","repo_rate","cpi_inflation","m3_growth")) {
  gc_test <- causality(var_model, cause = v)
  p_val   <- gc_test$Granger$p.value
  cat(sprintf("  %-20s → Credit Growth: p = %.4f %s\n",
              v, p_val, ifelse(p_val < 0.05, "✓ Significant", "")))
}

# ── 5. Impulse Response Functions ────────────────────────────
irf_repo  <- irf(var_model, impulse = "repo_rate",     response = "credit_growth",
                 n.ahead = 12, boot = TRUE, ci = 0.95)
irf_gdp   <- irf(var_model, impulse = "gdp_growth",    response = "credit_growth",
                 n.ahead = 12, boot = TRUE, ci = 0.95)
irf_m3    <- irf(var_model, impulse = "m3_growth",     response = "credit_growth",
                 n.ahead = 12, boot = TRUE, ci = 0.95)
irf_cpi   <- irf(var_model, impulse = "cpi_inflation", response = "credit_growth",
                 n.ahead = 12, boot = TRUE, ci = 0.95)

# Convert IRFs to data frames for ggplot
irf_to_df <- function(irf_obj, impulse_name) {
  data.frame(
    quarter  = 0:12,
    response = as.numeric(irf_obj$irf[[1]]),
    lower    = as.numeric(irf_obj$Lower[[1]]),
    upper    = as.numeric(irf_obj$Upper[[1]]),
    impulse  = impulse_name
  )
}

irf_df <- bind_rows(
  irf_to_df(irf_repo, "Repo Rate Shock"),
  irf_to_df(irf_gdp,  "GDP Growth Shock"),
  irf_to_df(irf_m3,   "M3 Growth Shock"),
  irf_to_df(irf_cpi,  "CPI Inflation Shock")
)

colors <- c("Repo Rate Shock"    = "#d62728",
            "GDP Growth Shock"   = "#1f77b4",
            "M3 Growth Shock"    = "#2ca02c",
            "CPI Inflation Shock"= "#ff7f0e")

p_irf <- ggplot(irf_df, aes(x = quarter, y = response, color = impulse)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = impulse), alpha = 0.15,
              color = NA) +
  geom_line(size = 1.2) +
  facet_wrap(~impulse, scales = "free_y", ncol = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values  = colors) +
  labs(title    = "Impulse Response of Bank Credit Growth",
       subtitle = "Response to one standard deviation shock | 95% confidence bands",
       x = "Quarters after shock", y = "Response (% change in credit growth)") +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "none",
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(color = "grey40"),
        strip.text       = element_text(face = "bold"))

ggsave("outputs/03_irf.png", p_irf, width = 12, height = 8, dpi = 150)
print(p_irf)
cat("Saved: outputs/03_irf.png\n")

# ── 6. Forecast Error Variance Decomposition ─────────────────
fevd_result <- fevd(var_model, n.ahead = 12)
fevd_credit <- as.data.frame(fevd_result$credit_growth)
fevd_credit$quarter <- 1:nrow(fevd_credit)

fevd_long <- fevd_credit %>%
  pivot_longer(-quarter, names_to = "variable", values_to = "share") %>%
  mutate(variable = recode(variable,
    credit_growth = "Credit Growth",
    gdp_growth    = "GDP Growth",
    repo_rate     = "Repo Rate",
    cpi_inflation = "CPI Inflation",
    m3_growth     = "M3 Growth"
  ))

p_fevd <- ggplot(fevd_long, aes(x = quarter, y = share * 100, fill = variable)) +
  geom_area(alpha = 0.85) +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Forecast Error Variance Decomposition — Bank Credit Growth",
       subtitle = "Share of forecast variance explained by each variable",
       x = "Quarters ahead", y = "Share of Variance (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")

ggsave("outputs/03_fevd.png", p_fevd, width = 10, height = 6, dpi = 150)
print(p_fevd)
cat("Saved: outputs/03_fevd.png\n")

# ── 7. Print key findings ────────────────────────────────────
repo_peak <- min(irf_to_df(irf_repo, "Repo")$response)
gdp_peak  <- max(irf_to_df(irf_gdp,  "GDP")$response)

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║           KEY FINDINGS SUMMARY                  ║\n")
cat("╠══════════════════════════════════════════════════╣\n")
cat(sprintf("║  Peak repo rate shock effect : %.3f pp           ║\n", repo_peak))
cat(sprintf("║  Peak GDP growth shock effect: %.3f pp           ║\n", gdp_peak))
cat(sprintf("║  Optimal VAR lag             : %d quarter(s)       ║\n", optimal_lag))
cat(sprintf("║  VAR stable                  : %s                ║\n",
            ifelse(all(roots < 1), "YES ✓", "NO ✗")))
cat("╚══════════════════════════════════════════════════╝\n")

cat("\nVAR & IRF analysis complete.\n")
