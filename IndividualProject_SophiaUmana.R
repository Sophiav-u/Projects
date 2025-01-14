library(quantmod)
library(e1071)
library(sn)
library(fBasics)
library(moments)
library(nortest)
library(ggplot2)
library(reshape2)
library(corrplot)
library(tibble)
library(factoextra)
library(psych) 

crypto_tickers = c("LTC-USD", "ADA-USD", "ETH-USD", "XRP-USD", "BTC-USD")
tickers <- c("USO", "CVX", "BP", "BNO", "XOM", crypto_tickers)
getSymbols(tickers, src = "yahoo", from = "2019-01-01", to = "2024-10-01",warnings=FALSE)
rf <- getSymbols("^IRX", src = "yahoo", from = "2019-01-01", to = "2024-10-01")
stocks <- data.frame(USO$USO.Close,CVX$CVX.Close,BP$BP.Close,BNO$BNO.Close,XOM$XOM.Close)
crypto <- data.frame(Cl(`BTC-USD`),Cl(`XRP-USD`),Cl(`ADA-USD`),Cl(`ETH-USD`),Cl(`LTC-USD`))
rf_n <- na.approx(IRX$IRX.Close) ## Impute missing values using linear interpolation
rf_rates_daily <- na.omit(rf_n/100/252)  
s_index <- index(USO)[-1]
c_index <- index(`BTC-USD`)[-1]

USO_return=na.omit(Delt(stocks$USO.Close))
CVX_return=na.omit(Delt(stocks$CVX.Close))
BP_return=na.omit(Delt(stocks$BP.Close))
BNO_return=na.omit(Delt(stocks$BNO.Close))
XOM_return=na.omit(Delt(stocks$XOM.Close))
stocks_return <- data.frame(USO_return,CVX_return,BP_return,BNO_return,XOM_return)
colnames(stocks_return)=c("USO", "CVX", "BP", "BNO", "XOM")
rownames(stocks_return) <- s_index

BTC_return=na.omit(Delt(crypto$BTC.USD.Close))
XRP_return=na.omit(Delt(crypto$XRP.USD.Close))
ADA_return=na.omit(Delt(crypto$ADA.USD.Close))
ETH_return=na.omit(Delt(crypto$ETH.USD.Close))
LTC_return=na.omit(Delt(crypto$LTC.USD.Close))
crypto_return <- data.frame(BTC_return,XRP_return,ADA_return,ETH_return,LTC_return)
colnames(crypto_return)=c( "BTC", "XRP", "ADA", "ETH","LTC")
rownames(crypto_return) <- c_index

rf_rates_daily=rf_rates_daily[index(USO_return)]

calc_stats <- function(ret) {
  list(
    mean = mean(ret, na.rm = TRUE),
    median = median(ret, na.rm = TRUE),
    variance = var(ret, na.rm = TRUE),
    sd = sd(ret, na.rm = TRUE),
    quantiles = quantile(ret, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
    kurtosis = kurtosis(ret, na.rm = TRUE),
    skewness = skewness(ret, na.rm = TRUE)
  )
}

stocks_stats <- lapply(stocks_return,calc_stats)
stocks_stats <- as.data.frame(do.call(cbind,stocks_stats))
stocks_stats

crypto_stats <- lapply(crypto_return,calc_stats)
crypto_stats <- as.data.frame(do.call(cbind,crypto_stats))
crypto_stats

# Outliers
boxplot(stocks_return, main = "Boxplots of Returns", ylab = "Values")
boxplot(crypto_return, main = "Boxplots of Returns", ylab = "Values")

## Function for univariate tests
normality_tests <- function(ret) {
  list(
    shapiro = shapiro.test(as.vector(ret)),
    jarque_bera = jarqueberaTest(as.vector(ret)),
    agostino = agostino.test(as.vector(ret), alternative = "two.sided"),
    ad = ad.test(as.vector(ret))
  )
}
# Apply tests
stock_normality_results <- lapply(stocks_return, normality_tests)
stock_normality_results

crypto_normality_results <- lapply(crypto_return, normality_tests)
crypto_normality_results


hist(crypto_return$LTC)


#Expected Return

USO_Expected_Return=mean(stocks_return$USO) 
CVX_Expected_Return=mean(stocks_return$CVX)
BP_Expected_Return=mean(stocks_return$BP) 
BNO_Expected_Return=mean(stocks_return$BNO) 
XOM_Expected_Return=mean(stocks_return$XOM) 
BTC_Expected_Return=mean(crypto_return$BTC) 
XRP_Expected_Return=mean(crypto_return$XRP) 
ADA_Expected_Return=mean(crypto_return$ADA) 
ETH_Expected_Return=mean(crypto_return$ETH) 
LTC_Expected_Return=mean(crypto_return$LTC)


# Risk / volatilities
var(stocks_return$USO)
var(stocks_return$CVX)
var(stocks_return$BP)
var(stocks_return$BNO)
var(stocks_return$XOM)
var(crypto_return$BTC)
var(crypto_return$XRP)
var(crypto_return$ADA)
var(crypto_return$ETH)
var(crypto_return$LTC)

USO_Risk=sd(stocks_return$USO)
CVX_Risk=sd(stocks_return$CVX)
BP_Risk=sd(stocks_return$BP)
BNO_Risk=sd(stocks_return$BNO)
XOM_Risk=sd(stocks_return$XOM)
BTC_Risk=sd(crypto_return$BTC)
XRP_Risk=sd(crypto_return$XRP)
ADA_Risk=sd(crypto_return$ADA)
ETH_Risk=sd(crypto_return$ETH)
LTC_Risk=sd(crypto_return$LTC)

#Risk Premium

mean(stocks_return$USO) - mean(rf_rates_daily)
mean(stocks_return$CVX) - mean(rf_rates_daily)
mean(stocks_return$BP) - mean(rf_rates_daily)
mean(stocks_return$BNO) - mean(rf_rates_daily)
mean(stocks_return$XOM) - mean(rf_rates_daily)
mean(crypto_return$BTC) - mean(rf_rates_daily)
mean(crypto_return$XRP) - mean(rf_rates_daily)
mean(crypto_return$ADA) - mean(rf_rates_daily)
mean(crypto_return$ETH) - mean(rf_rates_daily)
mean(crypto_return$LTC) - mean(rf_rates_daily)

#Excess return
stocks_return$USO - mean(rf_rates_daily)
stocks_return$CVX - mean(rf_rates_daily)
stocks_return$BP - mean(rf_rates_daily)
stocks_return$BNO - mean(rf_rates_daily)
stocks_return$XOM - mean(rf_rates_daily)
crypto_return$BTC - mean(rf_rates_daily)
crypto_return$XRP - mean(rf_rates_daily)
crypto_return$ADA - mean(rf_rates_daily)
crypto_return$ETH - mean(rf_rates_daily)
crypto_return$LTC - mean(rf_rates_daily)

#Sharpe Ratio

(USO_Expected_Return-mean(rf_rates_daily))/USO_Risk
(CVX_Expected_Return-mean(rf_rates_daily))/CVX_Risk
(BP_Expected_Return-mean(rf_rates_daily))/BP_Risk
(BNO_Expected_Return-mean(rf_rates_daily))/BNO_Risk
(XOM_Expected_Return-mean(rf_rates_daily))/XOM_Risk
(BTC_Expected_Return-mean(rf_rates_daily))/BTC_Risk
(XRP_Expected_Return-mean(rf_rates_daily))/XRP_Risk
(ADA_Expected_Return-mean(rf_rates_daily))/ADA_Risk
(ETH_Expected_Return-mean(rf_rates_daily))/ETH_Risk
(LTC_Expected_Return-mean(rf_rates_daily))/LTC_Risk

#Correlations

stocks_return <- rownames_to_column(stocks_return, var = "index")
crypto_return<- rownames_to_column(crypto_return, var = "index")
returns <- merge(stocks_return, crypto_return, by ="index" , all.x = TRUE)
rownames(returns) <- returns$index
returns$index <- NULL
cor <- cor(returns, method="spearman")
corrplot(cor, method = "number")

#Statistical Factor Model
pairs(returns)
pca <- prcomp(returns, scale=TRUE)
summary(pca)
fviz_eig(pca)

fa_result <- fa(returns, nfactors = 3, rotate = "varimax")
print(fa_result)
fa.diagram(fa_result)

fviz_pca_var(pca, col.var = "contrib", repel = TRUE) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red")

#Economic Factors - ESG
esg <- c("Cl=F","SUSA","ESGV","ICLN","ERTH", "VSGX","EASG","GNR","GRID")
getSymbols(esg, src = "yahoo", from = "2019-01-01", to = "2024-10-01", warnings=FALSE)


#Economic Factor Model
CLF<- na.approx(Cl(`CL=F`))[-1]
ESG_factor <- data.frame(CLF,SUSA$SUSA.Close,ESGV$ESGV.Close,ICLN$ICLN.Close,ERTH$ERTH.Close,VSGX$VSGX.Close, EASG$EASG.Close, GNR$GNR.Close, GRID$GRID.Close)
CLF <- na.omit(Delt(CLF))
SUSA <- na.omit(Delt(ESG_factor$SUSA.Close))
ESGV <- na.omit(Delt(ESG_factor$ESGV.Close))
ICLN <- na.omit(Delt(ESG_factor$ICLN.Close))
ERTH <- na.omit(Delt(ESG_factor$ERTH.Close))
VSGX <- na.omit(Delt(ESG_factor$VSGX.Close))
EASG <- na.omit(Delt(ESG_factor$EASG.Close))
GNR <- na.omit(Delt(ESG_factor$GNR.Close))
GRID <- na.omit(Delt(ESG_factor$GRID.Close))

norm_esg <- data.frame(CLF,SUSA,ESGV,ICLN,ERTH,VSGX,EASG,GNR,GRID)
colnames(norm_esg)=c("CLF","SUSA","ESGV","ICLN","ERTH", "VSGX","EASG","GNR","GRID")
rownames(norm_esg) <- s_index
cor2 <- cor(norm_esg, method="spearman")
corrplot(cor2, method = "number")

arFit = ar(cbind(CLF,ICLN,EASG,GNR))
res = arFit$resid[29:1445,]
returns_m <- as.matrix(returns)
lmfit = lm(returns_m[29:1445,]~res[,1]+res[,2]+res[,3]+res[,4])
slmfit=summary(lmfit)
rsq = rep(0,10) #create a variable rsq with nine 0 values

for (i in 1:10){rsq[i]= slmfit[[i]][[8]]}

beta_CLF = lmfit$coef[2,] 
beta_ICLN = lmfit$coef[3,] 
beta_EASG = lmfit$coef[4,] 
beta_GNR = lmfit$coef[5,] 

par(mfrow=c(1,5)) 
barplot(rsq,horiz=T,names=names(beta_CLF),main="R squared")
barplot(beta_CLF,hori=T,main="beta CLF") 
barplot(beta_ICLN,hori=T,main="beta ICLN") 
barplot(beta_EASG,hori=T,main="beta EASG") 
barplot(beta_GNR,hori=T,main="beta GNR") 


#Herding

exchange.herd = function(return)
{
  n=ncol(return)
  Rm = rowMeans(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}
rownames(crypto_return) <- crypto_return$index
crypto_return$index <- NULL

f = exchange.herd(crypto_return)
head (f)

CSAD.df = fortify.zoo(f)
CSAD.df$Rm2 = CSAD.df$Rm^2
CSAD.df = CSAD.df[-c(1),]
head (CSAD.df)
tail (CSAD.df)
y = CSAD.df$CSAD
x1 = abs (CSAD.df$Rm)
x2 =CSAD.df$Rm2
#Linear model
linearMod <- lm(y~x1+x2) # build linear regression
print(linearMod)
summary(linearMod)
