library("XLConnect")
library(forecast)
library(fpp)

# Reading data
export1997 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=2,endRow=13,header=FALSE)
export1998 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=14,endRow=25,header=FALSE)
export1999 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=26,endRow=37,header=FALSE)
export2000 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=38,endRow=49,header=FALSE)
export2001 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=50,endRow=61,header=FALSE)
export2002 <-readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=62,endRow=73,header=FALSE)
export2003 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=74,endRow=85,header=FALSE)
export2004 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=86,endRow=97,header=FALSE)
export2005 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=98,endRow=109,header=FALSE)
export2006 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=110,endRow=121,header=FALSE)
export2007 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=122,endRow=133,header=FALSE)
export2008 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=134,endRow=145,header=FALSE)
export2009 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=146,endRow=157,header=FALSE)
export2010 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=158,endRow=169,header=FALSE)
export2011 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=170,endRow=181,header=FALSE)
export2012 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=182,endRow=193,header=FALSE)
export2013 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=194,endRow=205,header=FALSE)
export2014 <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                    sheet=2,startCol=2,endCol=2,startRow=206,endRow=217,header=FALSE)

  # or you can use the code below for all data

  export <- readWorksheetFromFile("C:/RCodes/export.izzetkilic.xlsx",
                                      sheet=2,startCol=2,endCol=2,startRow=2,endRow=217,header=FALSE)

# Merging and create time series

export <- cbind(export1997,export1998,export1999,export2000,export2001,export2002,export2003,export2004,export2005,
                export2006,export2007,export2008,export2009,export2010,export2012,export2013,export2013,export2014)
colnames(export) <- c("1997","1998","1999","2000","2001","2002","2003","2004","2005",
                      "2006","2007","2008","2009","2010","2011","2012","2013","2014") 

exportAsTimeSeries <- ts(export, start=1997, frequency=12)

# Graphics and forecasting
plot(exportAsTimeSeries, xlab = "Years", ylab = "Monthly Export", main = "Monthly Export Data for 18 Years")

  # 3 Basic Method in forecasting
  exportForForecast1 <- window(exportAsTimeSeries, start=1997, end=2010-.1)
  exportfit1 <- meanf(exportForForecast1, h=12)
  exportfit2 <- naive(exportForForecast1, h=12)
  exportfit3 <- snaive(exportForForecast1, h=12)

  plot(exportfit1, plot.conf=FALSE, main="Forecasts for Monthly Export")
    lines(exportfit2$mean,col=2)
    lines(exportfit3$mean,col=3)
    lines(exportAsTimeSeries)
    legend("topleft",lty=1,col=c(4,2,3), legend=c("Mean method","Naive method","Seasonal naive method"))

  # Errors
  exportForForecast2 <- window(exportAsTimeSeries, start=2010)
    accuracy(exportfit1, exportForForecast2)
    accuracy(exportfit2, exportForForecast2)
    accuracy(exportfit3, exportForForecast2)
  
  # Residuals
  exportForForecast <- window(exportAsTimeSeries, start=1997, end=2014-.1)
  par(mfrow=c(2,2))
  plot(exportForForecast, xlab = "Years", ylab = "Monthly Export", main = "Monthly Export Data (1997-2013)")
  res <- residuals(naive(exportForForecast))
  plot(res, main="Residuals From Naive Method", 
      ylab="", xlab="Year")
  Acf(res, main="ACF of Residuals")
  hist(res, nclass="FD", main="Histogram of Residuals")

  # Forecast
  plot(forecast(exportAsTimeSeries), xlab = "Years", ylab = "Monthly Export", 
       main = "Monthly Export Data for 18 Years")
  exportForForecast <- window(exportAsTimeSeries, start=1997, end=2014-.1)
  theRestData <- window(exportAsTimeSeries, start=2014)

    # Forecast: Holt-Winters Seasonal Multiplicative Method
    plot(hw(exportForForecast,h=12,seasonal = "multiplicative"), xlab = "Years", ylab = "Monthly Export", 
        main = "Holt-Winters Seasonal Multiplicative Method Monthly - Export Data for 17 Years")
    par(mfrow=c(2,2))
    plot(exportForForecast, xlab = "Years", ylab = "Monthly Export", main = "Monthly Export Data")
    res <- residuals(hw(exportForForecast,h=12,seasonal = "multiplicative"))
    plot(res, main="Residuals From Holt-Winters Multiplicative Method", 
         ylab="", xlab="Year")
    Acf(res, main="ACF of residuals")
    hist(res, nclass="FD", main="Histogram of Residuals")
    ets(exportForForecast,model="MAM")
    plot(ets(exportForForecast,model="MAM"))

    # Forecast: Holt-Winters Seasonal Additive Method
    plot(hw(exportForForecast,h=12,seasonal = "additive"), xlab = "Years", ylab = "Monthly Export", 
         main = "Holt-Winters Seasonal Additive Method Monthly - Export Data for 17 Years")
    par(mfrow=c(2,2))
    plot(exportForForecast, xlab = "Years", ylab = "Monthly Export", main = "Monthly Export Data")
    res <- residuals(hw(exportForForecast,h=12,seasonal = "additive"))
    plot(res, main="Residuals From Holt-Winters Additive Method", 
         ylab="", xlab="Year")
    Acf(res, main="ACF of residuals")
    hist(res, nclass="FD", main="Histogram of Residuals")
    ets(exportForForecast,model="MAA")
    plot(ets(exportForForecast,model="MAA"))

    # Automatic Model Choosing
    ets(exportForForecast,model="ZZZ")

# ARIMA Models
library(TSA)
tsdisplay(exportForForecast,main="ACF-PACF For Data")
tsdisplay(diff(diff(exportForForecast,12)),main="ACF-PACF For Data")

fit1 <- Arima(exportForForecast,order = c(1,2,2), seasonal=c(2,2,3))
tsdisplay(residuals(fit1),main="Arima(1,2,2)(2,2,3)[12]")

fit2 <- auto.arima(exportForForecast,stepwise = FALSE)
tsdisplay(residuals(fit2),main="Auto Arima")

plot(forecast(fit1,h=12))

  # Controlling errors
  theRestOfData <- window(exportAsTimeSeries, start=2014)
  fitArima <- auto.arima(exportForForecast,stepwise = FALSE)
  fitHW <- hw(exportForForecast,h=12,seasonal = "multiplicative")
  accuracy(forecast(fitArima,h=12),theRestData)
  accuracy(fitHW,theRestData)
