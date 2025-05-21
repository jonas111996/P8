#— Require packages ----------------------------------------------------------
library(dplyr)     # for data manipulation
library(tidyr)     # for pivoting
library(goftest)   # for ad.test()
library(quantmod)  # for getSymbols, dailyReturn
library(bbmle)     # for mle2()
library(ggplot2)   # for plotting
library(quantmod)    # getSymbols, dailyReturn
library(gridExtra)   # arrange multiple plots
library(tseries)     # jarque.bera.test
library(moments)     # skewness, kurtosis

#--- 1. Download & Compute Log-Returns ----------------------------------------
symbols <- c(
  "NFLX", "AAPL", "MSFT",  # Technology
  "TSLA", "F",               # Automotive
  "PFE",                       # Healthcare
  "JPM", "MS"                # Financials
)
getSymbols(symbols, src = "yahoo", from = "2013-01-01", to = "2015-12-31", auto.assign = TRUE)

returns_list <- lapply(symbols, function(sym) {
  dr <- dailyReturn(Ad(get(sym)), type = "log")
  colnames(dr) <- sym
  dr
})
all_returns_xts <- do.call(merge, returns_list)

df_returns <- fortify.zoo(all_returns_xts) %>%
  rename(Date = Index) %>%
  pivot_longer(-Date, names_to = "Ticker", values_to = "LogReturn") %>%
  mutate(
    Sector = case_when(
      Ticker %in% c("NFLX","AAPL","MSFT") ~ "Technology",
      Ticker %in% c("TSLA","F")            ~ "Automotive",
      Ticker == "PFE"                        ~ "Healthcare",
      Ticker %in% c("JPM","MS")            ~ "Financials"
    )
  )

#--- 2. Time-Series Plot of the log-returns ------------------------------------
event_dates <- as.Date(c("2013-01-02","2015-12-31"))

p_ts <- ggplot(df_returns, aes(Date, LogReturn, colour = Ticker)) +
  geom_line(linewidth = 1.4, colour = "white") +
  geom_line(linewidth = 1.0, alpha = 0.8) +
  geom_vline(xintercept = event_dates, linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~Sector, scales = "free_y") +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(y = "Log-Return") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",        # frees horizontal space
    panel.spacing.x = unit(0.8, "lines")
  )

print(p_ts)

#--- 3. Histograms & QQ-Plots for normal distribution--------------------------
for (sym in symbols) {
  df_sym <- filter(df_returns, Ticker == sym)
  μ <- mean(df_sym$LogReturn, na.rm = TRUE)
  σ <- sd(df_sym$LogReturn,   na.rm = TRUE)
  
  p1 <- ggplot(df_sym, aes(LogReturn)) +
    geom_histogram(aes(y = ..density..),
                   bins = 40,
                   fill = "steelblue",
                   alpha = 0.6) +
    stat_function(
      fun = dnorm,
      args = list(mean = μ, sd = σ),
      color = "darkred",
      size = 1
    ) +
    labs(title = sym, x = "Log-Return", y = "Density") +
    theme_minimal()
  
  p2 <- ggplot(df_sym, aes(sample = LogReturn)) +
    stat_qq(size = 0.6) +
    stat_qq_line(color = "darkred") +
    labs(title = sym, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  grid.arrange(p1, p2, nrow = 1)
}

#--- 4. Jarque-Bera Test ------------------------------------------------------
jb_results <- df_returns %>% group_by(Sector,Ticker) %>% summarize(
  JB = jarque.bera.test(na.omit(LogReturn))$statistic,
  p  = jarque.bera.test(na.omit(LogReturn))$p.value,
  .groups = "drop"
)
print(jb_results)

#---- 5. Student-t distribition fit-------------------------------------------
#— Neg-Log-Likelihood for location–scale Student-t ---------------------------
nll_t <- function(mu, sigma, nu, x) {
  if (sigma <= 0 || nu <= 2) return(Inf)
  -sum(dt((x - mu) / sigma, df = nu, log = TRUE) - log(sigma))
}

#— Loop over tickers, fit, test, and plot -------------------------------------
results <- list()

for(sym in unique(df_returns$Ticker)) {
  x <- df_returns$LogReturn[df_returns$Ticker == sym]
  
  # 1) Fit with retry on failure-----------------------------------------------
  fit <- try(
    mle2(nll_t,
         start = list(mu    = mean(x, na.rm=TRUE),
                      sigma = sd(x,   na.rm=TRUE),
                      nu    = 5),
         data   = list(x = x),
         method = "L-BFGS-B",
         lower  = c(mu = -Inf, sigma = 1e-6, nu = 2.01)),
    silent = TRUE
  )
  if(inherits(fit, "try-error")) {
    # retry with a different nu start
    fit <- try(
      update(fit,
             start = list(mu    = mean(x, na.rm=TRUE),
                          sigma = sd(x,   na.rm=TRUE),
                          nu    = 4)),
      silent = TRUE
    )
  }
  if(inherits(fit, "try-error")) {
    warning("MLE failed for ", sym)
    next
  }
  
  # 2) Extract parameters & AD‐test--------------------------------------------
  params <- coef(fit)
  ad      <- ad.test(
    x,
    null = function(v) pt((v - params["mu"]) / params["sigma"],
                          df = params["nu"])
  )
  
  # 3) Store results-----------------------------------------------------------
  results[[sym]] <- data.frame(
    Symbol    = sym,
    mu        = params["mu"],
    sigma     = params["sigma"],
    nu        = params["nu"],
    AD_stat   = unname(ad$statistic),
    AD_pvalue = ad$p.value
  )
  
  # 4) Plot: histogram + fitted density----------------------------------------
  grid_x <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = 400)
  dens   <- dt((grid_x - params["mu"]) / params["sigma"],
               df = params["nu"]) / params["sigma"]
  dens_df <- data.frame(x = grid_x, density = dens)
  
  p1 <- ggplot(data.frame(x), aes(x)) +
    geom_histogram(aes(y = ..density..),
                   bins = 40, fill = "gray80", color = "white") +
    geom_line(data = dens_df,
              aes(x = x, y = density),
              color = "steelblue", size = 1) +
    labs(x = "Log-Return", y = "Density") +
    ggtitle(paste0(NULL, sym)) +
    theme_minimal()
  
  # 5) Plot: QQ of standardized residuals--------------------------------------
  z <- (x - params["mu"]) / params["sigma"]
  p2 <- ggplot(data.frame(z), aes(sample = z)) +
    stat_qq(distribution = qt, dparams = list(df = params["nu"])) +
    stat_qq_line(distribution = qt, color = "steelblue", dparams = list(df = params["nu"])) +
    labs(x = "Theoretical t-Quantiles",
         y = "Standardized Residual Quantiles") +
    ggtitle(paste0(sym, NULL)) +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol = 2)
}

#— Combine all results ---------------------------------------------------------
results_df <- do.call(rbind, results)
print(results_df)




