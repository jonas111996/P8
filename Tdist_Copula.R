#Load libraries
library(quantmod)       #getSymbols, dailyReturn
library(MASS)           #fitdistr for Student's t
library(ADGofTest)      #ad.test
library(grid)           #for custom plot labels

set.seed(123)


#Get stock data
symbols <- c("NFLX","AAPL","TSLA","PFE","MSFT","JPM","MS","F")
getSymbols(symbols, src = "yahoo", from = "2013-01-01", to = "2015-12-31")

#Compute log‐returns
rets <- lapply(symbols,
               function(sym) dailyReturn(Ad(get(sym)), type = "log"))
names(rets) <- symbols

#Fit Student's t to each return series, can easily be changed to NIG
t_fits <- lapply(rets, function(x) {
  fitdistr(as.numeric(x), densfun = "t")
})
names(t_fits) <- symbols

#Compute CDFs for each fitted distribution
u_scores <- mapply(function(x, fit) {
  mu <- fit$estimate["m"]
  s  <- fit$estimate["s"]
  nu <- fit$estimate["df"]
  pt((coredata(x)-mu)/s, df = nu)
}, rets, t_fits, SIMPLIFY = FALSE)

#Anderson–Darling goodness‐of‐fit test
ad_results <- mapply(function(x, fit) {
  mu <- fit$estimate["m"]
  s  <- fit$estimate["s"]
  nu <- fit$estimate["df"]
  ad.test(x = as.numeric(x),
          distr.fun = function(q) pt((q-mu)/s, df = nu))
}, rets, t_fits, SIMPLIFY = FALSE)
names(ad_results) <- symbols

#Print AD statistics and p‐values
for(sym in symbols) {
  cat(sym, ": AD =", round(ad_results[[sym]]$statistic,3),
      ", p-value =", round(ad_results[[sym]]$p.value,3), "\n")
}

#Build U‐matrix of pseudo‐observations
U <- do.call(cbind, u_scores)
colnames(U) <- symbols

library(copula)

#Define empty t, Clayton, Gaussian (normal), and Gumbel copulas
tcop    <- tCopula(dim = length(symbols), df = 4, df.fixed = TRUE)
claycop <- claytonCopula(dim = length(symbols))
normcop <- normalCopula(dim = length(symbols))
gumbcop <- gumbelCopula(dim = length(symbols))

#Fit each copula by maximum pseudo‐likelihood
fit_tcop   <- fitCopula(tcop,    data = U, method = "mpl")
fit_clay   <- fitCopula(claycop, data = U, method = "mpl")
fit_norm   <- fitCopula(normcop, data = U, method = "mpl")
fit_gumb   <- fitCopula(gumbcop, data = U, method = "mpl")

#Print fitted parameters
fit_tcop@copula@parameters
fit_clay@copula@parameters
fit_norm@copula@parameters
fit_gumb@copula@parameters

#Goodness‐of‐fit tests using multivariate multiplier bootstrap (N = 1,000)
gof_tcop  <- gofCopula(fit_tcop@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Sn", N = 1000)

gof_clay  <- gofCopula(fit_clay@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Sn", N = 1000)

gof_norm  <- gofCopula(fit_norm@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Sn", N = 1000)

gof_gumb  <- gofCopula(fit_gumb@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Sn", N = 1000)

gof_tcop2  <- gofCopula(fit_tcop@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Rn", N = 1000)

gof_clay2  <- gofCopula(fit_clay@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Rn", N = 1000)

gof_norm2  <- gofCopula(fit_norm@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Rn", N = 1000)

gof_gumb2  <- gofCopula(fit_gumb@copula,  x = U,
                       simulation = "mult", estim.method = "mpl",
                       method = "Rn", N = 1000)

#Summarize GOF results
gof_list <- list(
  t_copula     = gof_tcop,
  clayton      = gof_clay,
  normal       = gof_norm,
  gumbel       = gof_gumb
)
gof_list2 <- list(
  t_copula2     = gof_tcop2,
  clayton2      = gof_clay2,
  normal2       = gof_norm2,
  gumbel2       = gof_gumb2
)


print(gof_list)
print(gof_list2)



