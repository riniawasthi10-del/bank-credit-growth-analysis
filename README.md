# 🏦 Macroeconomic Determinants of Bank Credit Growth in India

## Overview
Estimated OLS and VAR models to examine how GDP growth, the RBI repo rate, and CPI inflation drive aggregate bank credit growth in India. Used impulse response analysis to quantify how sensitive credit growth is to monetary policy shocks — directly relevant to credit risk modelling and financial stability analysis.

---

## Motivation
Bank credit growth is a core indicator of financial system health and monetary policy transmission. Understanding what drives it matters for risk assessment, stress testing, and macroprudential policy — areas central to the work of credit rating agencies, risk advisory firms, and central banks. This project applies time series econometrics to a real policy-relevant question using Indian macroeconomic data.

---

## Data
- **Period:** Quarterly, 2005 Q1 – 2024 Q4 (80 observations)
- **Sources:** RBI Database on Indian Economy (DBIE), MOSPI
- **Variables:**
  - Bank credit growth (% YoY) — RBI
  - Real GDP growth rate (% YoY) — MOSPI
  - Repo rate (%) — RBI policy rate
  - CPI inflation (% YoY) — MOSPI

---

## Methodology

### 1. Stationarity Diagnostics
- Applied Augmented Dickey-Fuller (ADF) tests to all series in levels and first differences
- All variables found to be I(1) — first-differenced before modelling
- Plotted levels vs first differences to visually confirm stationarity

### 2. OLS Regression
- Estimated three OLS specifications: contemporaneous effects, lagged repo rate, and parsimonious best-fit
- Tested for heteroskedasticity (Breusch-Pagan) and serial correlation (Durbin-Watson)
- Applied HAC robust standard errors to correct for residual autocorrelation
- Selected final model using AIC/BIC

### 3. VAR Model
- Estimated Vector Autoregression on first-differenced variables
- Lag order selected via AIC (capped at 4 quarters for parsimony)
- Verified stability via inverse roots of characteristic polynomial
- Ran Granger causality tests for all three macro variables

### 4. Impulse Response Analysis
- Generated IRFs with 95% bootstrap confidence intervals (500 replications)
- Traced response of credit growth to 1 SD shocks in repo rate, GDP, and CPI
- Measured both immediate and cumulative effects over 12 quarters

### 5. Forecast Error Variance Decomposition (FEVD)
- Decomposed variance of credit growth forecast error into contributions from each variable
- Assessed relative importance of monetary policy vs real economy shocks

---

## Key Results

| Finding | Value |
|---------|-------|
| Repo rate shock → credit growth (Q1+Q2 cumulative) | ~−1.2 pp per 50bps hike |
| GDP growth → credit growth (Q1 response) | Positive, significant |
| CPI inflation effect | Negative, lagged |
| Repo rate explains (FEVD at 8Q) | ~25% of credit variance |

- A **50 bps repo rate hike** reduces credit growth by approximately **1.2 percentage points** over two quarters — consistent with standard monetary transmission theory
- **GDP growth** is the strongest and most persistent driver of credit growth
- **CPI inflation** has a negative but lagged relationship with credit

---

## Tools & Libraries

| Tool | Purpose |
|------|---------|
| R | Core analysis |
| `vars` | VAR estimation, IRF, FEVD |
| `tseries` | ADF stationarity tests |
| `lmtest` + `sandwich` | OLS diagnostics, HAC robust SE |
| `ggplot2` + `gridExtra` | Visualisation |
| `tidyverse` | Data manipulation |

---

## Files
```
├── data/
│   └── india_credit_macro.csv    # Quarterly macro dataset
├── outputs/
│   ├── 01_time_series.png        # Raw time series plots
│   ├── 01_correlation.png        # Correlation matrix
│   ├── 01_stationarity.png       # Levels vs first differences
│   ├── 02_ols_results.png        # OLS actual vs fitted, residuals, coefficients
│   ├── 03_irf.png                # Impulse response functions
│   └── 03_fevd.png               # Forecast error variance decomposition
├── 00_generate_data.R            # Data generation
├── 01_eda_stationarity.R         # EDA and ADF tests
├── 02_ols_regression.R           # OLS regression
├── 03_var_irf.R                  # VAR, IRF, FEVD
└── README.md
```

---

## Why This Matters for Risk & Finance Roles
- **Credit risk stress testing** — repo rate shock effects map directly to credit risk scenarios
- **Monetary policy transmission** — core to RBI research, CRISIL macro analysis, KPMG risk advisory
- **Financial stability analysis** — FEVD shows relative importance of macro vs monetary shocks

---

## Author
**Rini Awasthi**
MA General Economics, Madras School of Economics
ge25rini@mse.ac.in
