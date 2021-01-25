
library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="password")
# theseBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.94, b=-1.39)

load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/noShadeNoHypo.RData")
control <- xts(zoo(noShadeNoHypo$svValue, order.by = seq(ymd_hms("2014-01-01 00:00:00"), ymd_hms("2018-01-01 00:00:00"), by = 3600)))

load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/shade30.RData")
load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/shade60.RData")
load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/shade90.RData")

load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/highHypo.RData")
load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/medHypo.RData")
load(file = "C:/Users/t24x137/Desktop/Old Tower Desktop Folders/TempToolModelTesting2/littleHypo.RData")

load(file = "C:/Users/t24x137/Desktop/TempTool_2020/highHypo870.RData")
load(file = "C:/Users/t24x137/Desktop/TempTool_2020/medHypo870.RData")
load(file = "C:/Users/t24x137/Desktop/TempTool_2020/littleHypo870.RData")


daterange <- "2016-01-01/2017-06-01"

par(mfrow = c(2,1))
## NEW ##
plot(coredata(apply.daily(control[daterange], mean)), type = "l", lwd = 2, ylim = c(-1, 23))
lines(coredata(apply.daily(shade30[daterange], mean)), col = "gray25", lwd = 2)
lines(coredata(apply.daily(shade60[daterange], mean)), col = "gray50", lwd = 2)
lines(coredata(apply.daily(shade90[daterange], mean)), col = "gray75", lwd = 2)

plot(coredata(apply.daily(control[daterange], mean)), type = "l", lwd = 2, ylim = c(-1, 23))
lines(coredata(apply.daily(littleHypo870$cTemp$svValue[daterange], mean)), col = "gray25", lwd = 2)
lines(coredata(apply.daily(medHypo870$cTemp$svValue[daterange], mean)), col = "gray50", lwd = 2)
lines(coredata(apply.daily(highHypo870$cTemp$svValue[daterange], mean)), col = "gray75", lwd = 2)

## OLD ##
plot(coredata(apply.daily(control[daterange], mean)), type = "l", lwd = 2, ylim = c(-1, 23))
lines(coredata(apply.daily(shade30[daterange], mean)), col = "gray25", lwd = 2)
lines(coredata(apply.daily(shade60[daterange], mean)), col = "gray50", lwd = 2)
lines(coredata(apply.daily(shade90[daterange], mean)), col = "gray75", lwd = 2)

plot(coredata(apply.daily(control[daterange], mean)), type = "l", lwd = 2, ylim = c(-1, 23))
lines(coredata(apply.daily(littleHypo$cTemp$svValue[daterange], mean)), col = "gray25", lwd = 2)
lines(coredata(apply.daily(medHypo$cTemp$svValue[daterange], mean)), col = "gray50", lwd = 2)
lines(coredata(apply.daily(highHypo$cTemp$svValue[daterange], mean)), col = "gray75", lwd = 2)
#############
#############
#############


plot.zoo(highHypo870$cTemp$svValue["2016"], col ="orange", type = "l")
lines(as.zoo(highHypo$cTemp$svValue["2016"]), col = rgb(0,0,0, alpha = 0.5))

plot.zoo(medHypo870$cTemp$svValue["2016"], col ="red", type = "l")
lines(as.zoo(medHypo$cTemp$svValue["2016"]), col = rgb(0,0,0, alpha = 0.5))


plot.zoo(highHypo870$cTemp$svValue["2016-06-15"], col ="orange", type = "l", ylim = c(16,22))
lines(as.zoo(highHypo$cTemp$svValue["2016-06-15"]), col = rgb(0,0,0, alpha = 0.5))


max(abs(coredata(highHypo$cTemp$svValue["2016"]) - highHypo870$cTemp$svValue["2016"]))
max(abs(coredata(medHypo$cTemp$svValue["2016"]) - medHypo870$cTemp$svValue["2016"]))



length(coredata(highHypo$cTemp$svValue))
length(highHypo870$cTemp$svValue)
