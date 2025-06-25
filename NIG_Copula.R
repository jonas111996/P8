library(quantmod)
library(fBasics)
library(copula)
library(PortfolioAnalytics)
library(ghyp)
library(QRM)
library(ADGofTest)
library(grid)
library(nloptr)
#library(qualityTools)

set.seed(123)

#Get stock data
getSymbols("NFLX", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("AAPL", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("TSLA", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("PFE", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("MSFT", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("JPM", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("MS", src = "yahoo", from = "2013-1-1", to = "2015-12-31")
getSymbols("F", src = "yahoo", from = "2013-1-1", to = "2015-12-31")

#Get adjusted log
AAPL_returns <- dailyReturn(AAPL$AAPL.Adjusted,type = "log")
NFLX_returns <- dailyReturn(NFLX$NFLX.Adjusted, type = "log")
TSLA_returns <- dailyReturn(TSLA$TSLA.Adjusted, type = "log")
PFE_returns  <- dailyReturn(PFE$PFE.Adjusted, type = "log")
MSFT_returns <- dailyReturn(MSFT$MSFT.Adjusted, type = "log")
JPM_returns  <- dailyReturn(JPM$JPM.Adjusted, type = "log")
MS_returns   <- dailyReturn(MS$MS.Adjusted, type = "log")
F_returns    <- dailyReturn(F$F.Adjusted, type = "log")

#Plot adjusted log-returns for each stock
par(mfrow = c(3, 2))
plot(AAPL_returns)
plot(NFLX_returns)
plot(TSLA_returns)
plot(PFE_returns)
plot(MSFT_returns)
plot(JPM_returns)
plot(MS_returns)
plot(F_returns)

#Fit all NIGs and plot
par(mfrow = c(1,1))
op <- par(no.readonly = TRUE)
AAPL_nig<-nigFit(AAPL_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
                 method = "mle", scale = TRUE, doplot = TRUE, 
                 span = "auto", trace = TRUE, title = NULL, description = NULL)


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "AAPL NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

NFLX_nig<-nigFit(NFLX_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
       method = "mle", scale = TRUE, doplot = TRUE, 
       span = "auto", trace = TRUE, title = NULL, description = NULL) 


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "NFLX NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

TSLA_nig<-nigFit(TSLA_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
       method = "mle", scale = TRUE, doplot = TRUE, 
       span = "auto", trace = TRUE, title = NULL, description = NULL) 


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "TSLA NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

PFE_nig<-nigFit(PFE_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
                 method = "mle", scale = TRUE, doplot = TRUE, 
                 span = "auto", trace = TRUE, title = NULL, description = NULL) 


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "PFE NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

MSFT_nig<-nigFit(MSFT_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
                 method = "mle", scale = TRUE, doplot = TRUE, 
                 span = "auto", trace = TRUE, title = NULL, description = NULL) 


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "MSFT NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

JPM_nig<-nigFit(JPM_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
                 method = "mle", scale = TRUE, doplot = TRUE, 
                 span = "auto", trace = TRUE, title = NULL, description = NULL) 


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    

plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "JPM NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

MS_nig<-nigFit(MS_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
                 method = "mle", scale = TRUE, doplot = TRUE, 
                 span = "auto", trace = TRUE, title = NULL, description = NULL) 


par(fig = c(0,1,0,1),    
    new = TRUE,         
    mar = c(0,0,0,0))    


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "MS NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

F_nig<-nigFit(F_returns, alpha = 1, beta = 0, delta = 1, mu = 0, 
               method = "mle", scale = TRUE, doplot = TRUE, 
               span = "auto", trace = TRUE, title = "Ford", description = "Hey") 

par(fig = c(0,1,0,1),
    new = TRUE,
    mar = c(0,0,0,0))


plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
rect(0, 0.9, 1, 1, col="white", border=NA)
text(0.5, 0.95, "F NIG Fit", cex=1.5, font=2)

rect(0, 0,1, 0.05, col = "white", border = NA)
text(0.5, 0.05, "Log-Returns", cex = 1.2)

rect(-0.1, 0.01, 0.010, 1, col = "white", border = NA)
text(0, 0.5, "Empirical Density", srt = 90, cex = 1.2)

par(op)

#Get estimated parameters for all nig fits
AAPL_params <- AAPL_nig@fit$par
NFLX_params <- NFLX_nig@fit$par
TSLA_params <- TSLA_nig@fit$par
PFE_params <- PFE_nig@fit$par
MSFT_params <- MSFT_nig@fit$par
JPM_params <- JPM_nig@fit$par
MS_params <- MS_nig@fit$par
F_params <- F_nig@fit$par

#Collect all nigFit objects into a list
nig_fits <- list(
  AAPL = AAPL_nig,
  NFLX = NFLX_nig,
  TSLA = TSLA_nig,
  PFE  = PFE_nig,
  MSFT = MSFT_nig,
  JPM  = JPM_nig,
  MS   = MS_nig,
  F    = F_nig
)

#Compute CDFs 
cdfAAPL <- pnig(AAPL_returns, alpha=AAPL_params[1], beta=AAPL_params[2], 
                delta = AAPL_params[3], mu=AAPL_params[4])
cdfNFLX <- pnig(NFLX_returns, alpha=NFLX_params[1], beta=NFLX_params[2], 
                delta = NFLX_params[3], mu=NFLX_params[4])
cdfTSLA <- pnig(TSLA_returns, alpha=TSLA_params[1], beta=TSLA_params[2], 
                delta = TSLA_params[3], mu=TSLA_params[4])
cdfPFE <- pnig(PFE_returns, alpha=PFE_params[1], beta=PFE_params[2], 
                delta = PFE_params[3], mu=PFE_params[4])
cdfMSFT <- pnig(MSFT_returns, alpha=MSFT_params[1], beta=MSFT_params[2], 
               delta = MSFT_params[3], mu=MSFT_params[4])
cdfJPM <- pnig(JPM_returns, alpha=JPM_params[1], beta=JPM_params[2], 
               delta = JPM_params[3], mu=JPM_params[4])
cdfMS <- pnig(MS_returns, alpha=MS_params[1], beta=MS_params[2], 
               delta = MS_params[3], mu=MS_params[4])
cdfF <- pnig(F_returns, alpha=F_params[1], beta=F_params[2], 
               delta = F_params[3], mu=F_params[4])

#Non-bootstrapping Anderson-Darling tests
ad.test(
  x = as.numeric(AAPL_returns),
  distr.fun = pnig,
  alpha = AAPL_params[1],
  beta  = AAPL_params[2],
  delta = AAPL_params[3],
  mu    = AAPL_params[4]
)

ad.test(
  x = as.numeric(NFLX_returns),
  distr.fun = pnig,
  alpha = NFLX_params[1],
  beta  = NFLX_params[2],
  delta = NFLX_params[3],
  mu    = NFLX_params[4]
)

ad.test(
  x = as.numeric(TSLA_returns),
  distr.fun = pnig,
  alpha = TSLA_params[1],
  beta  = TSLA_params[2],
  delta = TSLA_params[3],
  mu    = TSLA_params[4]
)

ad.test(
  x = as.numeric(PFE_returns),
  distr.fun = pnig,
  alpha = PFE_params[1],
  beta  = PFE_params[2],
  delta = PFE_params[3],
  mu    = PFE_params[4]
)

ad.test(
  x = as.numeric(MSFT_returns),
  distr.fun = pnig,
  alpha = MSFT_params[1],
  beta  = MSFT_params[2],
  delta = MSFT_params[3],
  mu    = MSFT_params[4]
)

ad.test(
  x = as.numeric(JPM_returns),
  distr.fun = pnig,
  alpha = JPM_params[1],
  beta  = JPM_params[2],
  delta = JPM_params[3],
  mu    = JPM_params[4]
)

ad.test(
  x = as.numeric(MS_returns),
  distr.fun = pnig,
  alpha = MS_params[1],
  beta  = MS_params[2],
  delta = MS_params[3],
  mu    = MS_params[4]
)

ad.test(
  x = as.numeric(F_returns),
  distr.fun = pnig,
  alpha = F_params[1],
  beta  = F_params[2],
  delta = F_params[3],
  mu    = F_params[4]
)

#Combine CDFs in a matrix
U <- as.matrix(cbind(cdfAAPL, cdfNFLX, cdfTSLA, cdfPFE, cdfMSFT, cdfJPM, cdfMS, cdfF))

#Define empty copulas
tcop <- tCopula(dim = 8, df=4, df.fixed = TRUE)
claycop <- claytonCopula(dim = 8)
normcop <- normalCopula(dim = 8)
gumbcop <- gumbelCopula(dim = 8)

#Fit different copulas using mpl
fit_tcop <- fitCopula(tcop, U, method = "mpl")
fit_claycop <- fitCopula(claycop, U, method = "mpl")
fit_normcop <- fitCopula(normcop, U, method = "mpl")
fit_gumbcop <- fitCopula(gumbcop, U, method = "mpl")

#Test goodness-of-fit using Sn
gof_result1 <- gofCopula(fit_tcop@copula, x = U, simulation = "mult", estim.method = "mpl",
                         method = "Rn", N = 10000)

gof_result2 <- gofCopula(fit_normcop@copula, x = U, simulation = "mult", estim.method = "mpl",
                         method = "Rn", N = 10000)

gof_result3 <- gofCopula(fit_claycop@copula, x = U, simulation = "mult", estim.method = "mpl",
                         method = "Rn", N = 10000)

gof_result4 <- gofCopula(fit_gumbcop@copula, x = U, simulation = "mult", estim.method = "mpl",
                         method = "Rn", N = 10000)

gof_result5 <- gofCopula(fit_tcop@copula, x = U, simulation = "mult", estim.method = "mpl",
                          method = "Sn", N = 10000)

gof_result6 <- gofCopula(fit_claycop@copula, x = U, simulation = "mult", estim.method = "mpl", 
                        method = "Sn", N = 10000)

gof_result7 <- gofCopula(fit_normcop@copula, x = U, simulation = "mult", estim.method = "mpl", 
                         method = "Sn", N = 10000)

gof_result8 <- gofCopula(fit_gumbcop@copula, x = U, simulation = "mult", estim.method = "mpl", 
                         method = "Sn", N = 10000)


gof_listRn <- list(
  t_copulaRn     = gof_result1,
  claytonRn      = gof_result2,
  normalRn       = gof_result3,
  gumbelRn       = gof_result4
)
gof_listSn <- list(
  t_copulaSn     = gof_result5,
  claytonSn      = gof_result6,
  normalSn       = gof_result7,
  gumbelSn       = gof_result8
)


print(gof_listRn)
print(gof_listSn)








