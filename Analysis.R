library(tseries)
library(xts)
library(readxl)
s_date<-"1990-01-02"
e_date<-"2009-12-31"
sp500<-as.xts(get.hist.quote("^GSPC", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
sp500_lr<-na.omit(diff(log(sp500))) 
#non ho messo na.omit così si vede se il lag ha funzionato: 
  # dando per scontato che la prima sia giusta quella della volatilità futura dovrebbe avere lo stesso numero di quella passata ma 21 giorni prima a livello di data nella time series
  #dovrebbe avere gli na alla fine e non all'inizio
sp500_sd_h22<-rollapply(sp500_lr, width = 22, FUN = sd)
sp500_sd_f22<-na.omit(lag.xts(sp500_sd_h22, k=-21))*sqrt(252)*100

length(sp500_sd_f22)

vix<-as.xts(get.hist.quote("^VIX", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
vix_f22<-(vix[2:5021])
length(vix_f22)

model_f22<-lm(sp500_sd_f22~vix_f22)
summary(model_f22)
plot_f22<- cbind(coredata(vix_f22),(coredata(sp500_sd_f22)))
plot_f22(plot_f22)
abline(model_f22)
