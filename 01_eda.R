# ============================================================
# Macroeconomic Determinants of Bank Credit Growth — India
# 01: Exploratory Data Analysis
# Rini Awasthi | MA Economics, Madras School of Economics
# ============================================================

library(tidyverse)
library(gridExtra)

df <- read.csv("data/credit_data.csv")
df$date <- as.Date(df$date)

cat("Rows:", nrow(df), "\n")
cat("Period:", format(min(df$date)), "to", format(max(df$date)), "\n")
print(summary(df[,-1]))

# ── Plot 1: All series over time ─────────────────────────────
df_long <- df %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
    credit_growth = "Bank Credit Growth (%)",
    gdp_growth    = "GDP Growth (%)",
    repo_rate     = "Repo Rate (%)",
    cpi_inflation = "CPI Inflation (%)",
    m3_growth     = "M3 Money Supply Growth (%)"
  ))

p1 <- ggplot(df_long, aes(x = date, y = value, color = variable)) +
  geom_line(size = 0.9) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(title = "Indian Macroeconomic Indicators (2005–2024)",
       x = NULL, y = "Value (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 13))

ggsave("outputs/01_time_series.png", p1, width = 10, height = 12, dpi = 150)
print(p1)
cat("Saved: outputs/01_time_series.png\n")

# ── Plot 2: Scatter plots — credit growth vs each variable ───
vars  <- c("gdp_growth","repo_rate","cpi_inflation","m3_growth")
names <- c("GDP Growth","Repo Rate","CPI Inflation","M3 Growth")
colors <- c("#1f77b4","#d62728","#ff7f0e","#2ca02c")

plots <- mapply(function(v, nm, col) {
  ggplot(df, aes_string(x = v, y = "credit_growth")) +
    geom_point(color = col, alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", color = "black", se = TRUE, size = 0.8) +
    labs(title = paste("Credit Growth vs", nm),
         x = nm, y = "Credit Growth (%)") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
}, vars, names, colors, SIMPLIFY = FALSE)

p2 <- do.call(grid.arrange, c(plots, ncol = 2))
ggsave("outputs/01_scatter_plots.png", p2, width = 12, height = 9, dpi = 150)
cat("Saved: outputs/01_scatter_plots.png\n")

# ── Plot 3: Correlation matrix ───────────────────────────────
cor_matrix <- cor(df[, -1])
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1","Var2","Correlation")

p3 <- ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#d62728", mid = "white", high = "#1f77b4",
                       midpoint = 0, limits = c(-1,1)) +
  labs(title = "Correlation Matrix — Macroeconomic Variables",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title  = element_text(face = "bold", size = 13))

ggsave("outputs/01_correlation.png", p3, width = 8, height = 7, dpi = 150)
print(p3)
cat("Saved: outputs/01_correlation.png\n")

cat("\nEDA complete.\n")
