# install.packages('quantmod')
library(quantmod)
library(qrmtools)
library(qrmdata)
library(ismev)

## ------
## This is geting the sector data
## ------

# The following are just the Select Sector SPDR ETFs that track the S&P 500 sector indicies
sector.tickers <- c('XLK', 'XLF', 'XLV', 'XLE', 'XLI', 'XLY', 'XLP', 'XLB', 'XLU', 'XLRE')

## get all the data for the tickers
getSymbols(sector.tickers, src='yahoo', from='1998-12-16', to='2015-10-12')

# get all the close priceses for the index using lapply
close.list <- lapply(sector.tickers, function(ticker){
  # Dynamically retrieve the xts objects by its name
  data.obj <- get(ticker)
  # Extract only the closing price
  Cl(data.obj)
})

# Merge the list of xts objects into a single object
# `do.call(merge, ...)` applies the merge function to all elements in the list
sector.index.close <- do.call(merge, close.list)

colnames(sector.index.close) <- sector.tickers

head(sector.index.close)

# remove the real estate ticker
sector.index.close <- sector.index.close[, -which(colnames(sector.index.close) == 'XLRE')]

# Since we are done aggregating the data, remove the individual sector tickers
rm(list=sector.tickers)

## ------
## Analysis of the sector starts here
## ------

library(corrplot)

# This one is an analysis of the raw prices
# I naturally expect to generally move in the same direction (inflation aspect)
correlation.matrix <- cor(sector.index.close)
corrplot(correlation.matrix,
         method='color',
         type='lower',
         order='hclust',
         title='Sector Daily Close Price Correlation Heatmap',
         mar=c(0,0,1,0) # Adjusted margins for the heading
         )

# I am now going to analyse the daily log returns to date
log.returns <- diff(log(sector.index.close))[-1]
return.corr <- cor(log.returns)
corrplot(return.corr,
         method = 'color',
         type = 'lower',
         order = 'hclust',
         title = 'Sector Daily Close log returns Correlation Heatmap',
         mar=c(0,0,1,0)
         )

# Our chosen tickers are therefore (XLF, XLP) and (XLK, XLE)
# The challenge now is to choose stocks which are considered sustainable.
# I am going to use the LSEG ESG Scores on stocks (score is out of 100)

stock.matrix.info <- matrix(
  data = c(
    'AAPL', 'Apple Inc', 71,
    'MSFT', 'Microsoft Corporation', 87,
    'COST', 'Costco Wholesale', 72,
    'PEP', 'PepsiCo', 80,
    'OKE', 'ONEOK Inc', 80,
    'WMB', 'Williams Companies', 70,
    'AXP', 'American Express', 67,
    'JPM', 'JPMorgan Chase & Co', 80
  ),
  nrow = 8,
  ncol = 3,
  byrow = T
)
stock.picks <- data.frame(stock.matrix.info)
colnames(stock.picks) <- c('Ticker', 'Name', 'ESG_Score')

# The stocks are picked, I just now have to do the portfolio pick.
# To partition the stocks into two portfolios, all the 'sustainable'
# stocks are going to go on to one portfolio, and all the non-sustainable
# stocks are going to go on to the other one.

## ------
## Individual Stocks Stuff
## ------

# 4 and 6
data('SP500_const')

pep <- SP500_const['1986-07-09/', 'PEP']
wmb <- SP500_const['1986-07-09/', 'WMB']

pep.lr <- diff(log(pep))[-1]
wmb.lr <- diff(log(wmb))[-1]

## Statistical Facts and Stylised facts (comment on the dates the data is defined)
# Show that there is volatility clustering for both data series
# The acf plots also show that squared returns have a significant autocorrelation
# The normal plots show signs of volatility clustering (that is extreme returns
# appearing in clusters).
plot(pep.lr)
acf(pep.lr^2)

plot(wmb.lr) # Clumps are visible for when the company was almost bankrupt (2002-2004)
acf(wmb.lr^2, main='ACF of Squared Log Returns (Volatility Persistence)')

# distribution plots for the two stocks
# It can be seen that both distributions have heavy tails (combine with qqplot)
# Especially the WMB ticker
# So the two tickers have a Leptokurtic distribution
par(mfrow=c(1,2))
hist(pep.lr, freq=F, nclass = 100, main='Histogram of PEP Log Returns')
hist(wmb.lr, freq=F, nclass = 100, main='Histogram of WMB Log Returns')
par(mfrow=c(1,1))

# Statistical Facts about the data
# PEP shows a skewness value close to zero but a kurtosis value of around 9
# WMB shows a skewnes value of -3 and a kurtosis value of 163
wmb.mean <- mean(wmb.lr)
wmb.sig <- sd(wmb.lr)
wmb.sk <- mean((wmb.lr - wmb.mean)^3) / wmb.sig^3
wmb.kt <- mean((wmb.lr - wmb.mean)^4) / wmb.sig^4
wmb.data <- c('Williams Corp', 'WMB', 
              wmb.mean, wmb.sig, wmb.sk, wmb.kt, min(wmb.lr), max(wmb.lr))

pep.mean <- mean(pep.lr)
pep.sig <- sd(pep.lr)
pep.sk <- mean((pep.lr - pep.mean)^3) / pep.sig^3
pep.kt <- mean((pep.lr - pep.mean)^4) / pep.sig^4
pep.data <- c('PepsiCo', 'PEP', pep.mean, pep.sig, pep.sk, pep.kt, 
              min(pep.lr), max(pep.lr)
              )

stats.facts <- matrix(
  c(pep.data, wmb.data),
  ncol=8,
  nrow=2,
  byrow = T
)
colnames(stats.facts) <- c('Company', 'Ticker', 'Mean', 'Standard Deviation',
                           'Skewness', 'Kurtosis', 'Min', 'Max')

# These plots would confirm that for both tickers, they have heavier tails compared
# to the normal distribution
par(mfrow=c(1,2))
qqnorm(pep.lr)
qqline(pep.lr, col='red', lwd=2)
qqnorm(wmb.lr)
qqline(wmb.lr, col='red', lwd=2)
par(mfrow=c(1,1))


## -------
##  Portfolio A
## -------
pf.a.tickers <- c('MSFT', 'PEP', 'OKE', 'JPM')
pf.a.data <- SP500_const['1986-07-09/', pf.a.tickers]

## I have to use the minimum variance portfolio theory to choose my portfolio
## Care should be taken so that the proportion assignments are non-zero

# log-returns
pf.a.lg <- diff(log(pf.a.data))[-1, ]

# Covariance matrix
a_cov_matrix <- cov(pf.a.lg)

# Solve for the minimum variance portfolio weights
a_inv_cov <- solve(a_cov_matrix)
a_one <- rep(1, ncol(a_cov_matrix)) # ones column vector
# Weights
pi_A <- (a_inv_cov %*% a_one) / as.numeric(t(a_one) %*% a_inv_cov %*% a_one)

## --------
##  Portfolio B
## --------
pf.b.tickers <- c('AAPL', 'COST', 'WMB', 'AXP')
pf.b.data <- SP500_const['1986-07-09/', pf.b.tickers]

# Same selection method for B as that of A
pf.b.lg <- diff(log(pf.b.data))[-1, ]     # First difference
b_cov_matrix <- cov(pf.b.lg)              # Covariance Matrix
b_inv_cov <- solve(b_cov_matrix)          # Inverse Covariance Matrix
b_one <- rep(1, ncol(b_cov_matrix))       # Ones column vector
# Weights
pi_B <- (b_inv_cov %*% b_one) / as.numeric(t(b_one) %*% b_inv_cov %*% b_one)


## -------
##  Analysis of the portfolio
## -------
V <- 1000 # Initial wealth today

L_A <- -V * (pf.a.lg %*% pi_A)      # Linear loss for portfolio A
L_B <- -V * (pf.b.lg %*% pi_B)      # Linear loss for portfolio B

mu_A_norm <- mean(L_A)
mu_B_norm <- mean(L_B)

sig_A_norm <- sd(L_A)
sig_B_norm <- sd(L_B)

sk_A_norm <- mean((L_A - mu_A_norm)^3) / sig_A_norm^3
sk_B_norm <- mean((L_B - mu_B_norm)^3) / sig_B_norm^3

kt_A_norm <- mean((L_A - mu_A_norm)^4) / sig_A_norm^4
kt_B_norm <- mean((L_B - mu_B_norm)^4) / sig_B_norm^4

## (1) Analyse if they follow the Normal distribution

# Fit the normal distribution for both plots

# Histograms are not giving us a lot of information (if we combine them
# with our fitted estimates we can get a good look at the bad fit)

# The function clips extreme outliers, i.e, force them to stay within 6 standard 
# deviations
windowed.distribution <- function(data) {
  n <- length(data)
  mu <- mean(data)
  sd <- sd(data)
  data1 <- data*(data>-6*sd)*(data<6*sd) - 6*sd*(data<=-6*sd) + 6*sd*(data>=6*sd)
  breaks <- 6*sd*(-100:100) / 100
  bin.width <- breaks[2] - breaks[1]
  hist(data1, breaks=breaks, xlab='Daily Linear Loss')
  lines(breaks, dnorm(breaks, mu, sd)*bin.width*n, col='red', lwd=1.5)
}

# The plots show that the normal doesnt cover the fat tailed nature of the distribution
# on both ends of the plot.
par(mfrow=c(1,2))
windowed.distribution(L_A)
windowed.distribution(L_B)
par(mfrow=c(1,1))

# What about qqplot?
# Shows us that the linear loss has particularly heavy tails on both ends
# Expectation is that the normal distribution is not a good fit
par(mfrow=c(1,2))
qqnorm(L_A)
qqline(L_A, col='red', lwd=1.5)
qqnorm(L_B)
qqline(L_B, col='red', lwd=1.5)
par(mfrow=c(1,1))

# Perform the JB Test on both the random variables (test for normality)
# Result: Both portfolio variables strongly exhibit a non-normal distribution

T_A = (length(L_A) / 6) * (sk_A_norm^2 + ((kt_A_norm - 3)^2 / 4))
T_B = (length(L_B) / 6) * (sk_B_norm^2 + ((kt_B_norm - 3)^2 / 4))

p_A <- pchisq(T_A, 2, lower.tail = F)
p_B <- pchisq(T_B, 2, lower.tail = F)

# Results: The linear loss for both portfolios is not linear

## (2) Test if the t-distribution is a good fit.

# Create the log-likelihood function of the general t
ll.cen_t <- function(par, y) {
  loc <- par[1]   # Location
  scl <- par[2]   # Scale
  df <- par[3]    # Degrees of freedom
  
  if (scl <= 0 || df <= 0) return(Inf)      # Inf if scl and df are negative
  
  x <- (y - loc) / scl                      # Standardized data
  ll.vals <- log(dt(x, df=df)) - log(scl)   # Log-likelihood
  return(sum(ll.vals))
}

# Initialise the parameters
initial_params_A <- c('loc'=mean(L_A), 'scl'=sqrt(var(L_A) / 3), 'df'=3)
initial_params_B <- c('loc'=mean(L_B), 'scl'=sqrt(var(L_B) / 3), 'df'=3)

# Fit central t using mle approach
mle_params_A <- optim(
  par=initial_params_A, 
  fn=ll.cen_t, 
  y=L_A,
  method='L-BFGS-B',  # Methods that accepts bounds
  lower=c(-Inf, 0.001, 2.001), # Constrain scl >0 and df > 2.001
  upper=c(Inf, Inf, Inf),
  control=list(fnscale=-1)
  )

mle_params_B <- optim(
  par=initial_params_A, 
  fn=ll.cen_t, 
  y=L_B,
  method='L-BFGS-B',           # Methods that accepts bounds
  lower=c(-Inf, 0.001, 2.001), # Constrain scl >0 and df > 2.001
  upper=c(Inf, Inf, Inf),
  control=list(fnscale=-1)
  )

mle_params_A$par
mle_params_B$par
mle_params_A$value
mle_params_B$value

# Plot the qqplot for the data 

# Function creates a qqplot for the central t distribution
qqplot_ct <- function(data, par) {
  n <- length(data)
  indx <- ((1:n) - 0.5) / n
  obs <- quantile(data, indx)
  real <- qt(indx, df=par[3])*par[2] + par[1]
  
  plot(real, obs)
  qqline(
    real,
    distribution=function(p) par[1] + par[2]*qt(p, df=par[3]),
    col='red',
    lwd='1.5'
  )
}

# qqplots for the t-distribution
par(mfrow=c(1,2))
qqplot_ct(L_A, mle_params_A$par)
qqplot_ct(L_B, mle_params_B$par)
par(mfrow=c(1,2))

# Perform the goodness of fit test on the t-distribution
# Will need to generalise this function later
goodness.fit_t_test <- function(data, params) {
  n <- length(data)     # Length of data
  c <- length(params)   # Number of parameters
  
  # Create k bins (rule of thumb: k <= n/5 )
  k <- floor(0.5 * (n/5))
  
  # Find the quantiles that divide fitted distribution into k equal probabilities
  probs <- seq(0, 1, length.out = k+1)
  if (is.na(params[4])) {
    q_breaks <- qt(probs, df=params[3]) * params[2] + params[1]
  } else {
    q_breaks <- qt(probs, df=params[3], ncp=params[4]) * params[2] + params[1]
  }
  
  # Handle potential -Inf or Inf for strict coverage
  q_breaks[1] <- -Inf
  q_breaks[length(q_breaks)] <- Inf
  
  # Cut data into the bins with right closed intervals
  bins <- cut(data, breaks=q_breaks, right=T)
  
  # Calculate observed and expected counts
  observed <- as.numeric(table(bins))
  expected_prob <- diff(probs)            
  expected <- sum(observed)*expected_prob
  
  # Calculate the chi squared statistic
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  
  # calculate p-value
  df <- k - 1 - c
  p_value <- pchisq(chi_sq_stat, df=df, lower.tail=F)
  
  # Results
  #cat("--- Pearson's Chi-Squared Goodness of Fit ---\n")
  #cat("Observed bins:               ", k, "\n")
  #cat("Estimated parameters (c):    ", c, "\n")
  #cat("Degrees of freedom (k-1-c):  ", df, "\n")
  #cat("Chi-Squared Statistic:       ", round(chi_sq_stat, 4), "\n")
  #cat("p-value:                     ", format.pval(p_value, digits=4), "\n")
  
  return(list(chisq_stat=chi_sq_stat, p_value))
}

# Both goodness of fit tests fail to reject the null hypothesis that 
# The data follows a t-distribution
goodness.fit_t_test(L_A, mle_params_A$par)
goodness.fit_t_test(L_B, mle_params_B$par)


## (3) Test if the Non-Central t-distribution is a good fit

# Create a log-likelihood for the non-central t distribution
ll.nct <- function(data, par) {
  loc = par[1]
  scl = par[2]
  df = par[3]
  ncp = par[4]
  
  if (scl <= 0 || df <= 0) return(Inf)
  
  z <- (data - loc) / scl
  ll.vals <- dt(z, df=df,ncp=ncp, log=T) - log(scl)
  return(sum(ll.vals))
}

mle_nct_A <- optim(
  par=c('mu'=mean(L_A), 'sd'=sd(L_A), 'df'=5, 'ncp'=0),
  fn=ll.nct,
  data=L_A,
  control=list(fnscale=-1)
)
mle_nct_A$par

mle_nct_B <- optim(
  par=c('mu'=mean(L_B), 'sd'=sd(L_B), 'df'=5, 'ncp'=0),
  fn=ll.nct,
  data=L_B,
  control=list(fnscale=-1)
)
mle_nct_B$par

# Create the qqplot for the nct

qqplot_nct <- function(data, par) {
  n <- length(data)
  indx <- ((1:n) - 0.5) / n
  obs <- quantile(data, indx)
  real <- qt(indx, df=par[3], ncp=par[4])*par[2] + par[1]
  
  plot(real, obs)
  qqline(
    real,
    distribution=function(p) par[1] + par[2]*qt(p, df=par[3]),
    col='red',
    lwd='1.5'
  )
}

# Visual qqplot confirms that this is a potentially good fit
par(mfrow=c(1,2))
qqplot_nct(L_A, mle_nct_A$par)
qqplot_nct(L_B, mle_nct_B$par)
par(mfrow=c(1,1))

# We will perform a goodness of fit test for the non-central distribution
# Tests for both portfolios fail to reject the null
# Rejection by portfolio B stronger than that of A
goodness.fit_test(L_A, mle_nct_A$par)
goodness.fit_test(L_B, mle_nct_B$par)


## We can look at the likelihoods of the different models to see how the compare
ll.norm <- function(data) {
  mu <- mean(data)
  sd <- sd(data)
  
  z <- (data - mu) / sd
  ll.vals <- log(dnorm(z)) - log(sd)
  return(sum(ll.vals))
}


# tables show that:
# A: nct is a good fit (should we check the emperical skewness?)
# B: t is a good fit (not much improvement with nct)
# Much more robust reasoning is needed here
pf_bic <- function(mll, k, n) mll - 0.5*k*log(n);

bics <- matrix(
  c(
    pf_bic(ll.norm(L_A), 2, length(L_A)),
    pf_bic(mle_params_A$value, length(mle_params_A$par), length(L_A)),
    pf_bic(mle_nct_A$value, length(mle_nct_A$par), length(L_A)),
    pf_bic(ll.norm(L_B), 2, length(L_B)),
    pf_bic(mle_params_B$value, length(mle_params_B$par), length(L_B)),
    pf_bic(mle_nct_B$value, length(mle_nct_B$par), length(L_B))
  ),
  ncol=3,
  nrow=2,
  byrow=T
)
colnames(bics) <- c('norm', 't', 'nct')
rownames(bics) <- c('A', 'B')
bics

## ---------------------------------------
## Part III
## ---------------------------------------

## (a) Modelling extremes of the distribution
# I am going to create my own mean excess plot with my own set of confidence 
# intervals
mep_points <- function(data, u, alpha=0.1) {
  n <- length(data[data > u])
  
  if (n <= 1) return(c('mep'=NA, 'CIL'=NA, 'CIU'=NA))
  
  me_f <- mean(data[data > u] - u)
  sd <- sd(data[data > u] - u)
  ci_lower <- me_f - qnorm(1 - alpha / 2) * (sd / sqrt(n))
  ci_upper <- me_f + qnorm(1 - alpha / 2) * (sd / sqrt(n))
  
  return(c('mep'=me_f, 'CIL'=ci_lower, 'CIU'=ci_upper))
}

mep_points <- Vectorize(mep_points, vectorize.args = 'u')

# ME plot function
plot_mep <- function(data) {
  prop_str <- t(mep_points(data, sort(data)))
  #prop_str <- prop_str[which(!is.na(prop_str)),]
  par(mar=c(5,5,2,2))
  # Plot the mean excessx function
  plot(sort(data), prop_str[,'mep'],
       main='Custom Mean Excess Plot With CI',
       xlab='Threshold (u)',
       ylab='Mean Excess e(u)',
       pch=19, cex=0.8, col='brown3', cex.lab=0.75
       )
  # Add confidence intervals
  lines(
    sort(data), prop_str[,'CIL'], 
    col='gray50', lty=2
    )
  
  lines(
    sort(data), prop_str[,'CIU'], 
    col='gray50', lty=2
    )
  
  legend('bottomright',
         legend=c('e(u)', '95% CI'),
         col=c('brown3', 'gray50'),
         lty=c(NA, 2),
         pch=c(19, NA), cex=0.8
         )
  # reset margins:
  par(mar=c(5, 4, 4, 2) + 0.1)
}

gpd_me <- function(par,u1, u2, data) {
  return(as.numeric(mep_points(data, u1)[1,]) + (par[1]/(1-par[1]))*(u2 - u1))
}

# we can select our thresholds using these plots
# A: any threshold in the interval [20, 35] works well. I am choosing 34
# B: any threshold in the interval [40, 50] works well. I am choosing 42
# Both decisions on A and B are based on the width of the CIs
plot_mep(L_A[L_A > 0])
u_A <- 38
abline(v=u_A, col='blue', lty=2)
lines(
  c(u_A, max(sort(L_A) - u_A)),
  c(as.numeric(mep_points(L_A, u_A)[1,]), 
    gpd_me(gpd_A$par, 34, max(sort(L_A) - u_A), L_A)),
  col='blue',
  lty=2,
  lwd=1.5
  )

plot_mep(L_B[L_B > 0])
u_B <- 42
abline(v=u_B, col='blue', lty=2)
lines(
  c(u_B, max(sort(L_B) - 42)),
  c(
    as.numeric(mep_points(L_B, u_B)[1,]),
    gpd_me(gpd_B$par, u_B, max(sort(L_B) - u_B), L_B)
  ),
  col='blue',
  lty=2,
  lwd=1.5
)

# This would give me the Pareto loglikelihood
ll.pareto <- function(par, u, data) {
  shape <- par[1]
  scale <- par[2]
  
  if (scale <= 0) return(1e12) # constrain the scale to be positive
  
  L <- data[data > u] - u
  ll.vals <- log(dGPD(L, shape=shape, scale=scale))
  return(sum(ll.vals))
}

gpd_A <- optim(
  par=c('shape'=0.1, 'scale'=0.1),
  fn=ll.pareto,
  u=38,
  data=L_A,
  control=list(fnscale=-1)
)

gpd_B <- optim(
  par=c('shape'=0.1, 'scale'=0.1),
  fn=ll.pareto,
  u=42,
  data=L_B,
  control=list(fnscale=-1)
)

# Now I just need to draw the qq-plots for the GPD fitted at u
qqplot_gpd <- function(data, par, u){
  W <- data[data > u] - u
  n <- length(W)
  indx <- ((1:n)) / (n+1)
  obs <- sort(W)
  real <- qGPD(indx, shape=par[1], scale=par[2])
  
  plot(real, obs)
  qqline(
    real,
    distribution=function(p) qGPD(p, shape=par[1], scale=par[2]),
    col='red',
    lwd='1.5'
  )
}

# It is easier to justify B, A I would maybe have to think about using the
# exponential where xi=0
par(mfrow=c(1,2))
qqplot_gpd(L_A, gpd_A$par, u_A)
qqplot_gpd(L_B, gpd_B$par, u_B)
par(mfrow=c(1,1))

# A chi-squared test should be carried out to confirm fit
# (would this be okay though?)


## (b) Reporting on the expected shortfall for several values of alpha
# We have the VaR_alpha and ES_alpha for the GPD

es_gen_t <- function(gen_t_par, alpha) {
  loc <- gen_t_par['loc']
  scl <- gen_t_par['scl']
  df <- gen_t_par['df']
  
  q <- qt(alpha, df=df)
  pdf_q <- dt(q, df=df)
  
  h <- (df + q^2)/ (df - 1)
  
  es_st <- (pdf_q / (1 - alpha)) * h
  return(loc + scl * es_st)
}

# Function computes the mixed model value at risk formula for the GPD
var_gen_t_gpd <- function(gen_t_par, gpd_par, u, alpha) {
  t_loc <- gen_t_par['loc']     # t-location
  t_scl <- gen_t_par['scl']     # t scale
  t_df <- gen_t_par['df']       # t degrees of freedom
  
  gpd_shp <- gpd_par['shape']   # GPD shape
  gpd_scl <- gpd_par['scale']   # GPD scale
  
  
  st_t <- (u - t_loc) / t_scl # standardised threshold
  ssr <- gpd_scl / gpd_shp    # GPD scale-shape ratio
  
  # Calculate tail probability at u based on t distribution
  p_u <- pt(st_t, df=t_df)
  
  # VaR formula for GPD tail
  lvl_bulk <- ((1-alpha) / (1 - p_u))^(-gpd_shp)
  var_thresh <- u + ssr*(lvl_bulk - 1)
  
  return(var_thresh)
}

# Function computes the expected shortfall at level alpha for mixed model
es_gen_t_gpd <- function(gen_t_par, gpd_par, u, alpha) {
  
  t_loc <- gen_t_par['loc']     # t-location
  t_scl <- gen_t_par['scl']     # t scale
  t_df <- gen_t_par['df']       # t degrees of freedom
  
  gpd_shp <- gpd_par['shape']   # GPD shape (xi)
  gpd_scl <- gpd_par['scale']   # GPD scale (beta)
  
  # Check invalid shape parameter
  if(gpd_shp <= 0 || gpd_shp >= 1) return(rept(Inf, length(alpha)))
  
  # Find probability at threshold u
  st_t <- (u - t_loc) / t_scl   # Standardised threshold
  p_u <- pt(st_t, df=t_df)
  
  # Initialise output vector
  results <- numeric(length(alpha))
  results[] <- NA
  
  # indentify indicies
  # Bulk: alpha < p_u
  idx_low <- which(alpha < p_u)
  # Tail: alpha >= p_u
  idx_high <- which(alpha >= p_u)
  
  # Calculate bulk ES
  if (length(idx_low) > 0) {
    results[idx_low] <- es_gen_t(gen_t_par, alpha[idx_low])
  }
  
  # Calculate Tail ES
  if (length(idx_high) > 0) {
    var_high <- var_gen_t_gpd(gen_t_par, gpd_par, u, alpha[idx_high])
    
    numerator <- var_high + gpd_scl - gpd_shp * u
    results[idx_high] <- numerator / (1 - gpd_shp)
  }
  
  return(results)
}

# Plot the expected shortfall for both portfolios
alphas <- seq(0.95, 0.999, length.out=100)
es_s_A <- es_gen_t_gpd(mle_params_A$par, gpd_A$par, u_A, alphas)
es_s_B <- es_gen_t_gpd(mle_params_B$par, gpd_B$par, u_B, alphas)

es_title <- bquote(
  'Expected Shortfall at level' ~ alpha
)
col_A <- 'brown3'
col_B <- 'steelblue'
x_limits <- c(min(alphas), max(alphas))
y_limits <- range(c(es_s_A, es_s_B), na.rm=T)


plot(alphas, es_s_A,
     type='n',
     lwd=2,
     main=es_title,
     ylim=y_limits,
     xlim=x_limits,
     cex.main=0.7,
     yaxs='i',
     xaxs='i',
     ylab=NA,
     xlab=NA,
     axes=FALSE
)

grid(col = 'gray90', lty = 'dotted')

lines(alphas, es_s_A, col=col_A, lwd=1.5)
lines(alphas, es_s_B, col=col_B, lwd=1.5, lty=2)

# custom axis
axis(1, col='gray50', col.axis='gray30', lwd=0, lwd.ticks=1)
axis(2, col='gray50', col.axis='gray30', lwd=0, lwd.ticks=1, las=1)

# add axis title
title(xlab=bquote(alpha), line=2.5, col.lab='gray30', cex.lab=2)
title(ylab=bquote(ES[alpha]), line=2.5, col.lab='gray30')

# add L shaped box
box(bty='l', col='gray30')

legend('topleft',
       legend=c('Portfolio A', 'Portfolio B'),
       col=c(col_A, col_B),
       lwd=2, lty=c(1,2),
       bty='n', cex=0.9, inset=0.02
)


## d. we now attempt to model the dependence structure between A and B
plot(L_A, L_B, type='p', pch='.')
abline(0, cor(L_A, L_B), col='red')

# The Archimedean copulas not seen in class is the Frank copula
library(copula)

# First use the mle parameters to transform the raw data:
U_A <- pt(
  (L_A-mle_params_A$par['loc'])/mle_params_A$par['scl'], df=mle_params_A$par['df']
  )
U_B <- pt(
  (L_B-mle_params_B$par['loc'])/mle_params_B$par['scl'], df=mle_params_B$par['df']
  )

U_data <- cbind(U_A, U_B)

# Copula shell
norm_cop_spec <- normalCopula(dim=2, dispstr='un')
t_cop_spec <- tCopula(dim=2, dispstr='un', df.fixed=FALSE)
clayton_spec <- claytonCopula(dim=2)
frank_spec <- frankCopula(dim=2)

# fit using MLE
fit_norm <- fitCopula(norm_cop_spec, data=U_data, method='mpl')
fit_t <- fitCopula(t_cop_spec, data=U_data, method='mpl')
fit_clayton <- fitCopula(clayton_spec, data=U_data, method='mpl')
fit_frank <- fitCopula(frank_spec, data=U_data, method='mpl')


# summary of fit
copula_fits <- list(
  'Gaussian'=fit_norm,
  't-Copula'=fit_t,
  'Clayton'=fit_clayton,
  'Frank'=fit_frank
)

model_comparison <- data.frame(
  Model=names(copula_fits),
  rho=c(fit_norm@estimate, fit_t@estimate[1], '-', '-'),
  alpha=c('-', '-', fit_clayton@estimate, fit_frank@estimate),
  df=c('-', fit_t@estimate[2], '-', '-'),
  BIC=sapply(copula_fits, BIC),
  LogLik=sapply(copula_fits, logLik)
)

model_comparison <- model_comparison[order(model_comparison$BIC),]
print(model_comparison)


# Visualise the copulas side by side
# set seed somewhere here to ensure reproducibility
xlab <- bquote(U[A])
ylab <- bquote(U[B])

par(mfrow=c(2,2), pty='s')

# 1. observed data
plot(U_A, U_B, type='p', pch='.',
     main='Observed Data', xlab=bquote(U[A]), ylab=bquote(U[B])
     )

# 2. t-Copula simulation
sim_t <- rCopula(length(L_A), fit_t@copula)
plot(sim_t, pch='.', col='brown3', 
     main='Fitted t-Copula', xlab=xlab, ylab=ylab
     )

# 3. Clayton Simulation
sim_clay <- rCopula(length(L_A), fit_clayton@copula)
plot(
  sim_clay, pch='.', col='steelblue', 
  main='Fitted Clayton', xlab=xlab, ylab=ylab
)

# 4. Frank Simulation
sim_frank <- rCopula(length(L_A), fit_frank@copula)
plot(
  sim_frank, pch='.', col='darkgreen', 
  main='Fitted Frank', xlab=xlab, ylab=ylab
)


par(mfrow=c(1,1))




set.seed(487206)
n_sim <- length(L_A)    # number of simulations

# simulated correlated uniform random variables
U_sim <- rCopula(n_sim, fit_t@copula)

# transform into simulated losses for portfolio A
L_sim_A <- mle_params_A$par['loc'] +
  mle_params_A$par['scl']*qt(U_sim[,1], df=mle_params_A$par['df'])

# transform into simulated losses for portfolio B
L_sim_B <- mle_params_B$par['loc'] +
  mle_params_B$par['scl']*qt(U_sim[,2], df=mle_params_B$par['df'])

# Combine into data-frame
sim_losses <- data.frame(L_A=L_sim_A, L_B=L_sim_B)

# Plot the simulated joint-losses
plot(sim_losses,
     pch='.', col='brown3',
     main='Simulated Joint Losses\n(t-Copula + t-Marginals)',
     xlab='Simulated Loss A', 'Simulated Loss B',
     ylim=c(-60, 60),
     xlim=c(-60, 60)
)
# overlay the original data
points(L_A, L_B, pch='.')

legend(
  'bottomright', legend=c('Simulated', 'Observed'),
  col=c('brown3', 'black'), pch=15, cex=0.6
)




