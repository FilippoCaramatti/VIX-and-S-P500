library(tseries)
library(xts)
library(readxl)
library(plotly)
library(ggplot2)

library(lmtest)
library(MuMIn)


#start and end date
s_date <- "1990-01-02"
e_date <- "2022-04-10"

#sp500 data
sp500 <- as.xts(get.hist.quote("^GSPC", start = s_date, end = e_date, 
                               quote = "Close", provider = "yahoo", 
                               retclass = "zoo"))
sp500_lr <- na.omit(diff(log(sp500)))

#vix data
vix <- as.xts(get.hist.quote("^VIX", start = s_date, end = e_date, 
                             quote = "Close", provider = "yahoo", 
                             retclass = "zoo"))
n_vix <- length(vix)

#linear model tests fir f22d
d=22
sp500_sd_f <- na.omit(lag.xts(rollapply(sp500_lr, width = d, FUN = sd), 
                              k = - (d-1)))*sqrt(252)*100
vix_f <- (vix[2:(n_vix-(d-1))])
model_test1
resettest(sp500_sd_f~vix_f, power = 2:6 , type = "regressor")

#transformation of vix
vix_2 = vix_f^2
vix_3 = vix_f^3
vix_4 = vix_f^4
vix_5 = vix_f^5
vix_6 = vix_f^6
vix_ln =log(vix_f)
vix_sqrt = sqrt(vix_f)
vix_1ov = 1/vix_f

options(na.action = "na.fail")
model_test <- lm(sp500_sd_f~vix_f+vix_2+vix_3+vix_4+vix_5+vix_6+vix_ln+vix_sqrt+vix_1ov)
 dredge(model_test, evaluate = T, rank = "AIC", extra = "adjR^2")
write.csv(dre, file="dredge.csv")


f_model <- function(d) {
  # sp500 future 22 day volatility as historical volatility lagged backward of d days
  sp500_sd_f <- na.omit(lag.xts(rollapply(sp500_lr, width = d, FUN = sd), 
                                k = - (d-1)))*sqrt(252)*100
  
  vix_f <- (vix[2:(n_vix-(d-1))])
  
  model_f <- lm(sp500_sd_f~vix_f)
  
  correl <- cor(coredata(vix_f), coredata(sp500_sd_f))
  
  #vix and vola graph
  fig_1 <- ggplot(sp500_sd_f, aes(x=index(sp500_sd_f), y=coredata(sp500_sd_f)))+
    geom_line(aes(color="S&P500 future 22d vaolatility"))+
    geom_line(aes(x=index(sp500_sd_f), y=coredata(vix_f), color="VIX"))+
    theme_classic()+
    theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', 
                                          colour = "#EAEAEA"), 
          legend.position = c(0.25,0.9), legend.title=element_blank())+
    labs(x="Data", y="Annualized volatility")
  
  
  #graph of the model_f
  xy_f <- data.frame(coredata(vix_f), coredata(sp500_sd_f))
  
  legend_2<-c("VIX, S&P500 Future volatility 22d"="#1e76b4",
              "Regression line"="#ff8921")
  
  fig_2 <- ggplot(xy_f, aes(x=coredata(vix_f), y=coredata(sp500_sd_f)))+
    geom_point(shape=20, size=2, alpha = 1/10, 
               aes(color="VIX, S&P500 Future volatility 22d"))+
    theme_classic()+
    geom_smooth(method = "lm", se=F, size=1, aes(color="Regression line"))+
    labs(x="VIX", y="S&P500 forward volatility 22d")+
    scale_y_continuous(breaks = seq(10, 90, by = 10))+
    scale_x_continuous(breaks = seq(10, 80, by = 10))+
    scale_colour_manual(values = legend_2)+
    theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', 
                                          colour = "#EAEAEA"),
          legend.position = c(0.25,0.9), legend.title=element_blank())
  
  
  return(list(summary(model_f), correl, fig_1, fig_2))
}

f_model(d=22)



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

ggplot(r_corr_f)+
  geom_line(aes(color="Correlation", y=r_corr_f$correlations_f,
                x=c(5,11,22,33,44,55,66)))+
  geom_line(aes(color="R^2", y=r_corr_f$r_squareds_f, x=c(5,11,22,33,44,55,66)))+
  scale_x_continuous(breaks=c(5,11,22,33,44,55,66))+
  scale_y_continuous(limits = c(0.35, 0.85), breaks = seq(0.35, 0.85, by = 0.05))+
  ylab( expression(paste( R^{2},", correlation")))+
  xlab("Days in the volatility calculation")+
  theme_classic()+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', 
                                        colour = "#EAEAEA"), 
        legend.position = c(0.9,0.9), legend.title=element_blank())



h_model<-function(d) {
  
  # sp500 historical d day volatility as historical volatility lagged backward of d days
  sp500_sd_h <- na.omit(rollapply(sp500_lr, width = d, FUN = sd))*sqrt(252)*100
  
  vix_h <- (vix[(2+(d-1)):n_vix])
  
  #vix and vola graph
  fig_1 <- ggplot(sp500_sd_h, aes(x=index(sp500_sd_h), y=coredata(sp500_sd_h)))+
    geom_line(aes(color="S&P500 Historical 22d vaolatility"))+
    geom_line(aes(x=index(sp500_sd_h), y=coredata(vix_h), color="VIX"))+
    theme_classic()+
    theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', 
                                          colour = "#EAEAEA"), 
          legend.position = c(0.25,0.9), legend.title=element_blank())+
    labs(x="Data", y="Annualized volatility")
  
  model_h <- lm(sp500_sd_h~vix_h)
  
  #graph of the model_f
  xy_h <- data.frame(coredata(vix_h), coredata(sp500_sd_h))
  legend_2<-c("VIX, S&P500 Historical volatility 22d"="#1e76b4",
              "Regression line"="#ff8921")
  fig_2 <- ggplot(xy_h, aes(x=coredata(vix_h), y=coredata(sp500_sd_h)))+
    geom_point(shape=20, size=3, alpha = 1/10, 
               aes(color="VIX, S&P500 Historical volatility 22d"))+
    theme_classic()+
    geom_smooth(method = "lm", se=F, size=1, aes(color="Regression line"))+
    labs(x="VIX", y="S&P500 forward volatility 22d")+
    scale_y_continuous(breaks = seq(10, 90, by = 10))+
    scale_x_continuous(breaks = seq(10, 80, by = 10))+
    scale_colour_manual(values = legend_2)+
    theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', 
                                          colour = "#EAEAEA"),
          legend.position = c(0.25,0.9), legend.title=element_blank())
  
  correl_h <- cor(coredata(vix_h),(coredata(sp500_sd_h)))
  
  return(list(summary(model_h), correl_h, fig_1, fig_2))
  
}

h_model(d=22)

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

legend <- c("Correlation Historical model"="red1", "R^2 Historical model"="royalblue1",
            "Correlation Future model"="red4", "R^2 Future model"="royalblue4")

ggplot(r_corr_h)+
  geom_line(aes(color="Correlation Historical model", y=r_corr_h$correlations_h,
                x=c(5,11,22,33,44,55,66)))+
  geom_line(aes(color="R^2 Historical model", y=r_corr_h$r_squareds_h,
                x=c(5,11,22,33,44,55,66)))+
  geom_line(aes(color="Correlation Future model", y=r_corr_f$correlations_f,
                x=c(5,11,22,33,44,55,66)),)+
  geom_line(aes(color="R^2 Future model", y=r_corr_f$r_squareds_f, x=c(5,11,22,33,44,55,66)))+
  scale_x_continuous(breaks=c(5,11,22,33,44,55,66))+
  scale_y_continuous(limits = c(0.35, 0.95), breaks = seq(0.35, 0.95, by = 0.05))+
  ylab( expression(paste( R^{2},", correlation")))+
  xlab("Days in the volatility calculation")+
  theme_classic()+
  theme(panel.grid.major = element_line(size = 0.5, colour = "#EAEAEA"), 
        legend.position = c(0.8,0.9), legend.title=element_blank())+
  scale_color_manual(values = legend)