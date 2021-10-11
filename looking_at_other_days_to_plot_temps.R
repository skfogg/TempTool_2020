##
## Looking at other days to plot daily temps
##


library(hydrogeom)
library(RODBC)
library(zoo)
library(xts)
library(lubridate)
library(temptool)
setwd("C:/Users/t24x137/Desktop/TempTool_2020")

connect <- odbcConnect("TempToolFourANSI", uid="root", pwd="MSUFLL!!")

umatillaBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.95, b=-1.39)
umatillaBins_2third <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 23.97, b=-1.39)
umatillaBins_1third <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)

load("runs_using_2017_umatilla_aquifer_geometry/highHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/medHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/littleHypo870.RData")
load("model_output/noShadeNoHypo.RData")
load("model_output/shade30.RData")
load("model_output/shade60.RData")
load("model_output/shade90.RData")


control <- xts(zoo(noShadeNoHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

hypolit <- littleHypo870$cTemp$svValue
hypomed <- medHypo870$cTemp$svValue
hypohigh <- highHypo870$cTemp$svValue

lowcol <- "gray37"
medcol <- "gray60"
highcol <- "grey85"

plotdays <- c("2016-01-15/2016-01-16", "2016-04-15/2016-04-16", "2016-07-15/2016-07-16", "2016-10-15/2016-10-16")
yr1.5 <- "2016-01-01/2017-06-01"

time2plot <- "2016-09-01"
scenarios_shade <- list(shade30, shade60, shade90)
scenarios_hypo <- list(hypolit, hypomed, hypohigh)
graycols <- c(lowcol, medcol, highcol)

plot.zoo(control[time2plot],
         lwd =2,
         ylim =c(10,25))
mapply(function(s, c) lines(as.zoo(s[time2plot]), col = c, lwd = 2),
       scenarios_shade,
       graycols)

time2plot <- "2016-08-24/2016-08-27"
plot.zoo(apply.daily(control[time2plot], mean),
         lwd =2,
         ylim =c(10,27), type = "o")
mapply(function(s, c) lines(as.zoo(apply.daily(s[time2plot], mean)),
                            col = c,
                            lwd = 2,
                            type = "o"),
       scenarios_hypo,
       graycols)


plot.zoo(control[time2plot],
         lwd =2,
         ylim =c(10,27))
mapply(function(s, c) lines(as.zoo(s[time2plot]), col = c, lwd = 2),
       scenarios_hypo,
       graycols)

rangecalc <- function(x){
  (max(x) - min(x))
}

rangecalc(control["2016-08-26"])
rangecalc(hypohigh["2016-08-26"])



### Plot annual diel temp range:
par(mfrow = c(2,1),
    mar = c(4,4,1,1),
    oma = c(0,0,0,0))
yrandhalf <- "2016-01-01/2017-06-01"
plot.zoo(apply.daily(control["2016-01-01/2017-06-01"], rangecalc),
         lwd = 2,
         ylim = c(0,7),
         ylab = "Temperature")
mapply(function(s,c) lines(as.zoo(apply.daily(s[yrandhalf], rangecalc)),
                           col = c,
                           lwd = 2),
       scenarios_shade,
       graycols)
plot.zoo(apply.daily(control["2016-01-01/2017-06-01"], rangecalc),
         lwd = 2,
         ylim = c(0,7),
         ylab = "Temperature")
mapply(function(s,c) lines(as.zoo(apply.daily(s[yrandhalf], rangecalc)),
                           col = c,
                           lwd = 2),
       scenarios_hypo,
       graycols)

## Plot annual daily max temp:
plot.zoo(apply.daily(control["2016-01-01/2017-06-01"], max),
         lwd = 2,
         ylim = c(0,28),
         ylab = "Temperature")
mapply(function(s,c) lines(as.zoo(apply.daily(s[yrandhalf], max)),
                           col = c,
                           lwd = 2),
       scenarios_shade,
       graycols)
plot.zoo(apply.daily(control["2016-01-01/2017-06-01"], max),
         lwd = 2,
         ylim = c(0,28),
         ylab = "Temperature")
mapply(function(s,c) lines(as.zoo(apply.daily(s[yrandhalf], max)),
                           col = c,
                           lwd = 2),
       scenarios_hypo,
       graycols)

