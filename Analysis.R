library(tseries)
library(xts)
library(readxl)
s_date<-"1990-01-22"
e_date<-"2022-03-24"
sp500<-as.xts(get.hist.quote("^GSPC", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
vix<-as.xts(get.hist.quote("^VIX", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
sp500_lr<-na.omit(diff(log(sp500))) 

#non ho messo na.omit così si vede se il lag ha funzionato: 
  # dando per scontato che la prima sia giusta quella della volatilità futura dovrebbe avere lo stesso numero di quella passata ma 21 giorni prima a livello di data nella time series
  #dovrebbe avere gli na alla fine e non all'inizio
sp500_sd_h22<-rollapply(sp500_lr, width = 22, FUN = sd)
sp500_sd_f22<-lag.xts(sp500_sd_h22, k=-21)

sp500_sd_h11<-na.omit(rollapply(sp500_lr, width = 11, FUN = sd))
sp500_sd_h22<-na.omit(rollapply(sp500_lr, width = 22, FUN = sd))

#altre cose - prove a caso
as.zoo(read_xlsx("sp500.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))

write.zoo(sp500, file="C:\\Users\\cinef\\Documents\\GitHub\\VIX_and_SP500\\sp500.csv")

%>% 