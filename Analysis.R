library(tseries)
library(xts)
library(readxl)

#start and end date
s_date <- "1990-01-02"
e_date <- "2009-12-31"

f_model <- function(d) {
  #sp500 data
  sp500 <- as.xts(
    get.hist.quote("^GSPC", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo")
    )
  sp500_lr <- na.omit(
    diff(
      log(sp500)
    )
  )
  #vix data
  vix <- as.xts(
    get.hist.quote("^VIX", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo")
  )
  # sp500 future 22 day volatility as historical volatility lagged backward of d days
  sp500_sd_f <- na.omit(
    lag.xts(
      rollapply(sp500_lr, width = d, FUN = sd),
      k = -(d-1)
    )
  )*sqrt(252)*100

  n <- length(vix)
  vix_f <- (vix[2:(n-(d-1))])

  model <- lm(sp500_sd_f~vix_f)
  plott <- cbind(coredata(vix_f),(coredata(sp500_sd_f)))
  correl <- cor(coredata(vix_f),(coredata(sp500_sd_f)))

  return(list(summary(model), correl, plot(plott), abline(model)))

}

f_model(d=66)
f_model(d=55)
f_model(d=44)
f_model(d=33)
f_model(d=22)
f_model(d=11)
f_model(d=5)


h_model<-function(d) {
  #sp500 data
  sp500 <- as.xts(get.hist.quote("^GSPC", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
  sp500_lr <- na.omit(diff(log(sp500)))
  #vix data
  vix <- as.xts(get.hist.quote("^VIX", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
  # sp500 future d day volatility as historical volatility lagged backward of d days
  sp500_sd_h <- na.omit(
    rollapply(sp500_lr, width = d, FUN = sd)
  )*sqrt(252)*100

  n <- length(vix)
  vix_h <- (vix[(2+(d-1)):n])
  model <- lm(sp500_sd_h~vix_h)
  plott <- cbind(coredata(vix_h),(coredata(sp500_sd_h)))
  correl <- cor(coredata(vix_h),(coredata(sp500_sd_h)))

  return(list(summary(model), correl, plot(plott), abline(model)))
}

#------------------------------------------------------#





#sp500 data
sp500<-as.xts(get.hist.quote("^GSPC", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))
sp500_lr <- na.omit(diff(log(sp500))) 
#vix data
vix <- as.xts(get.hist.quote("^VIX", start = s_date, end = e_date, quote = "Close", provider = "yahoo", retclass = "zoo"))

# sp500 future 22 day volatility as historical volatility lagged backward of 22 days
sp500_sd_f22 <- na.omit(
  lag.xts(
    rollapply(sp500_lr, width = 22, FUN = sd),
    k=-21
    )
  )*sqrt(252)*100
#vix compatibility model
vix_f22 <- (vix[2:5021])

# verify it the lenght is the same to construct the model
length(sp500_sd_f22) 
length(vix_f22)

#f_22 model
model_f22 <- lm(sp500_sd_f22~vix_f22)
summary(model_f22)
plot_f22 <- cbind(coredata(vix_f22),(coredata(sp500_sd_f22)))
plot(plot_f22)
abline(model_f22)
cor_f22 <- cor(coredata(vix_f22),(coredata(sp500_sd_f22)))

    linters: with_defaults(
    line_length_linter=NULL, 
    object_usage_linter=NULL,
    infix_spaces_linter=NULL
    )
