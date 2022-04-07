library(tseries)
library(xts)
library(readxl)
library(plotly)


#start and end date
s_date <- "1990-01-02"
e_date <- "2009-12-31"

f_model <- function(d) {
  #sp500 data
  sp500 <- as.xts(
    get.hist.quote(
      "^GSPC",
      start = s_date,
      end = e_date,
      quote = "Close",
      provider = "yahoo",
      retclass = "zoo"
    )
  )
  sp500_lr <- na.omit(
    diff(
      log(sp500)
    )
  )
  #vix data
  vix <- as.xts(
    get.hist.quote(
      "^VIX",
      start = s_date,
      end = e_date,
      quote = "Close",
      provider = "yahoo",
      retclass = "zoo"
    )
  )
  # sp500 future 22 day volatility as historical volatility lagged backward of d days
  sp500_sd_f <- na.omit(
    lag.xts(
      rollapply(
        sp500_lr,
        width = d,
        FUN = sd
      ),
      k = - (d-1)
    )
  )*sqrt(252)*100

  n <- length(vix)
  vix_f <- (vix[2:(n-(d-1))])

  model <- lm(sp500_sd_f~vix_f)

  correl <- cor(
    coredata(vix_f),
    coredata(sp500_sd_f)
  )
  
  #graph of the model
  vix_coredata <- c(coredata(vix_f))
  sp500_coredata <- c(coredata(sp500_sd_f))
  
  xy <- data.frame(
    vix_coredata,
    sp500_coredata
  )
  
  fig <- ggplot(xy, aes(x=vix_coredata, y=sp500_coredata))+
    geom_point(shape=20, size=3, alpha = 1/10, aes(color="VIX, S&P500 Future volatility 22d"))+
    theme_classic()+
    geom_smooth(method = "lm", se=F, size=1, aes(color="Regression line"))+
    labs(x="VIX", y="S&P500 forward volatility 22d")+
    scale_y_continuous(breaks = seq(10, 90, by = 10))+
    scale_x_continuous(breaks = seq(10, 80, by = 10))+
    scale_colour_manual(values = legend)+
    theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#EAEAEA"), legend.position = c(0.25,0.9), legend.title=element_blank())
  fig

  return(
    list(
      summary(model), 
      correl, 
      fig
    )
  )

}

correlations_f <- c(
  f_model(d=5)[[2]],
  f_model(d=11)[[2]],
  f_model(d=22)[[2]],
  f_model(d=33)[[2]],
  f_model(d=44)[[2]],
  f_model(d=55)[[2]],
  f_model(d=66)[[2]]
)

r_squareds_f <- c(
  f_model(d=5)[[1]]$r.squared,
  f_model(d=11)[[1]]$r.squared,
  f_model(d=22)[[1]]$r.squared,
  f_model(d=33)[[1]]$r.squared,
  f_model(d=44)[[1]]$r.squared,
  f_model(d=55)[[1]]$r.squared,
  f_model(d=66)[[1]]$r.squared
  )

r_corr_f<-data.frame(
  row.names=c(
    "Forward 5 days",
    "Forward 11 days",
    "Forward 22 days",
    "Forward 33 days",
    "Forward 44 days",
    "Forward 55 days",
    "Forward 66 days"
    ), 
  correlations_f,
  r_squareds_f
)

plot_ly(
  r_corr_f, 
  x =c(5,11,22,33,44,55,66),
  y = ~correlations_f,
  type = 'scatter',
  mode ="lines",
  name = "Correlation"
)%>% 
  add_trace(y = ~r_squareds_f, name = "R^2")%>% 
  layout(legend = list(x = 0.8, y = 1), title="Future volatility model", yaxis = list(title="Correlation", zeroline=F),xaxis = list(title="Days" ,zeroline=F))










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

correlations_h <- c(
  h_model(d=5)[[2]],
  h_model(d=11)[[2]],
  h_model(d=22)[[2]],
  h_model(d=33)[[2]],
  h_model(d=44)[[2]],
  h_model(d=55)[[2]],
  h_model(d=66)[[2]]
)

r_squareds_h <- c(
  h_model(d=5)[[1]]$r.squared,
  h_model(d=11)[[1]]$r.squared,
  h_model(d=22)[[1]]$r.squared,
  h_model(d=33)[[1]]$r.squared,
  h_model(d=44)[[1]]$r.squared,
  h_model(d=55)[[1]]$r.squared,
  h_model(d=66)[[1]]$r.squared
)

r_corr_h<-data.frame(
  row.names=c(
    "Forward 5 days",
    "Forward 11 days",
    "Forward 22 days",
    "Forward 33 days",
    "Forward 44 days",
    "Forward 55 days",
    "Forward 66 days"
  ), 
  correlations_h,
  r_squareds_h
)

a<-plot_ly(
  r_corr_h, 
  x =c(5,11,22,33,44,55,66),
  y = ~correlations_h,
  type = 'scatter',
  mode ="lines",
  name = "Correlation"
)%>% 
  add_trace(y = ~r_squareds_h, name = "R^2")%>% 
  layout(legend = list(x = 0.8, y = 1), yaxis = list(title="Correlation", zeroline=F),xaxis = list(title="Days" ,zeroline=F))


orca(a, "plot.svg")

#------------------------------------------------------#
#sp500 data
sp500 <- as.xts(
  get.hist.quote(
    "^GSPC",
    start = s_date,
    end = e_date,
    quote = "Close",
    provider = "yahoo",
    retclass = "zoo"
  )
)
sp500_lr <- na.omit(
  diff(
    log(sp500)
  )
)
#vix data
vix <- as.xts(
  get.hist.quote(
    "^VIX",
    start = s_date,
    end = e_date,
    quote = "Close",
    provider = "yahoo",
    retclass = "zoo"
  )
)
# sp500 future 22 day volatility as historical volatility lagged backward of d days
sp500_sd_f <- na.omit(
  lag.xts(
    rollapply(
      sp500_lr,
      width = 22,
      FUN = sd
    ),
    k = - (22-1)
  )
)*sqrt(252)*100

n <- length(vix)
vix_f <- (vix[2:(n-(22-1))])

model <- lm(sp500_sd_f~vix_f)




