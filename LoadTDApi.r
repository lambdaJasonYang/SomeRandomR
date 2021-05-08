library(xts)
library(httr)
library(jsonlite)
library(ggplot2)
library(scales)
library(tidyverse)
library(quantmod)
#library(anytime)
ticker <- "GOOG"
apikey <- ""
endpoint <- sprintf("https://api.tdameritrade.com/v1/marketdata/%s/pricehistory",ticker)
result <- GET(endpoint, query = list("apikey"=apikey,"frequencyType"="minute"))
class(content(result,as ="text"))

jdataFull <- fromJSON(content(result, as="text"))
jdata <- jdataFull$candles
jdata$datetime <- as.POSIXct(jdata$datetime/1000,origin ="1970-01-01") 
#jdata$datetime <- lubridate::as_datetime(jdata$datetime)
#rownames(jdata) <- jdata$datetime
tdata <- xts(jdata$close,jdata$datetime)

#rownames(tdata) <- tdata$datetime
#head(jdata$datetime)
#zoodata

#out <- ggplot(data=zoodata, aes(x=datetime, y=close)) + #geom_point(color="darkblue")+ 
#   ggtitle('business dates, month breaks')
#print(tdata[,"close"])
plot(tdata)
lines(rollmean(tdata,20),col="red",pch=3)
lines(TTR::SMA(tdata,n=30),col="blue",pch=2)

ret <- diff(log(tdata))
ggplot(ret, aes(x=index(ret), y=ret))+
  geom_line(color="blue")
