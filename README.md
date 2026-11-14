# Quantitative Risk Analysis in R: Sustainable vs. Conventional Equity Portfolios, Extreme Value Theory & Copula Dependence

## Repository Contents

| File | Description |
| :--- | :--- |
| [`report/Quantitative_Risk_Analysis_Report.pdf`](./report/Quantitative_Risk_Analysis_Report.pdf) | **Full Technical Report (33 Pages):** Complete empirical analysis, proofs, Q-Q diagnostics, Mean Excess Plots, Expected Shortfall curves, and Copula contour comparisons. |
| [`src/quantitative_risk_analysis.R`](./src/quantitative_risk_analysis.R) | **Full R Source Code (`890 lines`):** End-to-end reproducible R script covering S&P 500 sector clustering, Minimum Variance Portfolio optimization, MLE distribution fitting (`optim`), custom vectorized Mean Excess Plots with 95% CIs, hybrid Student-$t$/GPD Expected Shortfall, and Maximum Pseudo-Likelihood Copula estimation. |

---

## Executive Summary

Do high-ESG ("sustainable") equity portfolios exhibit lower tail risk than conventional portfolios, or does environmental/governance screening concentrate sector exposure during market crashes?

This project conducts a three-part Quantitative Risk Management (QRM) investigation:
1. **Green Securitisation & Climate Risk:** Evaluates **Fannie Mae's Green Mortgage-Backed Securities (MBS)** programme, examining prepayment risk, extension risk, and "greenwashing" / impact reporting risk, along with transition vs. physical climate risk propagation across life, health, property, and liability insurers.
2. **S&P 500 Sector Clustering, Minimum Variance Portfolio Construction & Parametric Loss Fitting:** Constructs two 4-stock Minimum Variance Portfolios from S&P 500 constituents (1986–2015, $n = 7,377$ daily observations) partitioned by **LSEG ESG Scores** — **Portfolio A (Sustainable)** vs. **Portfolio B (Normal)** — and tests Normal, General Student-$t$, and Non-Central $t$ (`nct`) distributions via Maximum Likelihood Estimation (MLE), Pearson's $\chi^2$ goodness-of-fit, and Bayesian Information Criterion (BIC).
3. **Extreme Value Theory (POT / GPD), Hybrid Expected Shortfall & Multivariate Copulas:** Models extreme tail losses above high thresholds ($u_A = 38$, $u_B = 42$) using the **Generalised Pareto Distribution (GPD)**, derives analytical hybrid Student-$t$/GPD **Value-at-Risk ($\text{VaR}_\alpha$)** and **Expected Shortfall ($\text{ES}_\alpha$)** across $\alpha \in [0.95, 0.999]$, and fits **Gaussian, Student-$t$, Clayton, and Frank Copulas** to quantify joint crash dependence.

---

## Key Empirical Findings & Mathematical Framework

### 1. Portfolio Construction & Stylized Facts

Using hierarchical correlation clustering on S&P 500 Select Sector SPDR ETFs (`XLF`, `XLP`, `XLK`, `XLE`) and LSEG ESG scores, eight stocks were partitioned into two portfolios and weighted via **Minimum Variance Portfolio (MVP)** optimization:

$$\boldsymbol{\pi}_k = \frac{\Sigma_k^{-1}\mathbf{1}}{\mathbf{1}^\top \Sigma_k^{-1}\mathbf{1}}, \qquad L_\Delta = -V_0 \sum_{i=1}^{4} \pi_i X_{1}^{i} \quad (V_0 = £1,000)$$

| Portfolio | Constituents (LSEG ESG Score) | Minimum Variance Weights ($\boldsymbol{\pi}_k$) | Mean Daily Loss | Std. Dev. | Skewness | Kurtosis | JB Test $p$-value |
| :--- | :--- | :--- | :---: | :---: | :---: | :---: | :---: |
| **Portfolio A (Sustainable)** | `MSFT` (87), `PEP` (80), `OKE` (80), `JPM` (80) | `PEP` 39.2%, `OKE` 34.4%, `MSFT` 18.8%, `JPM` 7.7% | $-£0.625$ | $£11.27$ | $0.0086$ | $10.09$ | $< 2.2 \times 10^{-16}$ |
| **Portfolio B (Normal)** | `COST` (72), `AAPL` (71), `WMB` (70), `AXP` (67) | `COST` 36.1%, `WMB` 27.6%, `AXP` 20.6%, `AAPL` 15.8% | $-£0.621$ | $£13.99$ | $0.4310$ | $13.38$ | $< 2.2 \times 10^{-16}$ |

- **Individual Asset Stylized Facts:** Both `PEP` (kurtosis $9.39$) and `WMB` (kurtosis $163.57$, driven by the 2002 energy distress episode) exhibit pronounced leptokurtosis and long-memory volatility clustering in squared log-returns $\text{ACF}(X_t^2)$.
- **Parametric Bulk Model Selection:** Jarque–Bera tests decisively reject normality for both portfolios. Under MLE and BIC comparison (`pf_bic = mll - 0.5 * k * log(n)`), the **3-parameter General Student-$t$ distribution** ($\hat{\nu}_A = 3.81$, $\hat{\nu}_B = 3.50$) achieves the highest BIC ($-27,540.31$ for Portfolio A; $-29,061.47$ for Portfolio B), outperforming the 4-parameter Non-Central $t$ model once the skewness penalty is accounted for.

---

### 2. Extreme Value Theory (Peaks-Over-Threshold GPD) & Hybrid Expected Shortfall

By the **Pickands–Balkema–de Haan Theorem**, excess losses $L_\Delta - u \mid L_\Delta > u$ converge to a Generalised Pareto Distribution $\text{GPD}(\xi, \beta)$ with linear Mean Excess Function:

$$e(u) = \mathbb{E}[L_\Delta - u \mid L_\Delta > u] = \frac{\beta + \xi u}{1 - \xi}, \qquad \xi < 1$$

Using custom vectorized Mean Excess Plots with $95\%$ confidence intervals, thresholds were selected where $e(u)$ stabilizes linearly before tail sample variance explodes:

| Portfolio | Selected Threshold ($u$) | GPD Scale ($\hat{\beta}$) | GPD Shape ($\hat{\xi}$) | Tail Index ($1/\hat{\xi}$) | Implication |
| :--- | :---: | :---: | :---: | :---: | :--- |
| **Portfolio A (Sustainable)** | $u_A = £38$ | $11.0695$ | $0.4097$ | $\approx 2.44$ | Heavier asymptotic tail slope ($\hat{\xi}_A > \hat{\xi}_B$); infinite 3rd & 4th moments in the extreme tail. |
| **Portfolio B (Normal)** | $u_B = £42$ | $16.3454$ | $0.2647$ | $\approx 3.78$ | Higher baseline volatility ($\hat{\beta}_B > \hat{\beta}_A$), but slower asymptotic tail decay than Portfolio A. |

Combining the General Student-$t$ bulk ($L_\Delta \le u$) with the GPD tail ($L_\Delta > u$) yields the closed-form **Hybrid Value-at-Risk and Expected Shortfall**:

$$\text{VaR}_\alpha(L_\Delta) = u + \frac{\beta}{\xi}\left[\left(\frac{1-\alpha}{1 - F_t(u)}\right)^{-\xi} - 1\right], \qquad \text{ES}_\alpha(L_\Delta) = \frac{\text{VaR}_\alpha(L_\Delta) + \beta - \xi u}{1 - \xi} \quad (\alpha \ge F_t(u))$$

While Sustainable Portfolio A has lower Expected Shortfall at moderate confidence levels ($\alpha \in [0.95, 0.99]$), its steeper GPD shape parameter ($\hat{\xi}_A = 0.4097$ vs. $\hat{\xi}_B = 0.2647$) causes $\text{ES}_\alpha(L_A)$ to accelerate rapidly at extreme Solvency II confidence levels ($\alpha \to 0.999$), demonstrating that ESG screening does not immunize a portfolio against extreme tail concentration.

---

### 3. Multivariate Dependence & Copula Selection

Transforming marginal losses to uniform pseudo-observations $U_A = F_{t,A}(L_A)$ and $U_B = F_{t,B}(L_B)$, four copula families were calibrated via **Maximum Pseudo-Likelihood (MPL)**:

| Copula Family | Correlation ($\hat{\rho}$) | Parameter ($\hat{\alpha}$) | Degrees of Freedom ($\hat{\nu}$) | Log-Likelihood | BIC | Rank |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| **Student-$t$ Copula** | $0.5405$ | — | $3.4932$ | $1,534.75$ | **$-3,051.69$** | **1st (Best)** |
| **Gaussian Copula** | $0.5262$ | — | — | $1,259.98$ | $-2,511.06$ | 2nd |
| **Frank Copula** | — | $3.8617$ | — | $1,231.01$ | $-2,453.11$ | 3rd |
| **Clayton Copula** | — | $0.9163$ | — | $1,089.37$ | $-2,169.84$ | 4th |

The **Student-$t$ Copula ($\hat{\rho} = 0.5405, \hat{\nu} = 3.49$)** wins decisively by BIC ($-3,051.69$), capturing symmetric upper and lower tail dependence (simultaneous extreme gains and simultaneous crash losses) that Gaussian and Frank copulas miss.

---

## Reproducing the Analysis

```r
# Required R packages
install.packages(c("quantmod", "qrmtools", "qrmdata", "ismev", "corrplot", "copula"))

# Run the full analysis pipeline
source("src/quantitative_risk_analysis.R")
```
