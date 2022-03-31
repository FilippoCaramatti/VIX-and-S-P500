library(tseries)
library(xts)
library(readxl)

#start and end date
s_date<-"1990-01-02"
e_date<-"2009-12-31"

#sp500 data
sp500<-as.xts(get.hist.quote("^GSPC", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
sp500_lr<-na.omit(diff(log(sp500))) 
#future 22 day volatility as historical volatility lagged backward of 22 days
sp500_sd_f22<-na.omit(
  lag.xts(
    rollapply(sp500_lr, width = 22, FUN = sd),
    k=-21
    )
  )*sqrt(252)*100

#vix data
vix<-as.xts(get.hist.quote("^VIX", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
vix_f22<-(vix[2:5021])

# verify it the lenght is the same to construct the model
length(sp500_sd_f22) 
length(vix_f22)

#plotting the f_22 model
model_f22<-lm(sp500_sd_f22~vix_f22)
summary(model_f22)
plot_f22<- cbind(coredata(vix_f22),(coredata(sp500_sd_f22)))
plot(plot_f22)
abline(model_f22)
