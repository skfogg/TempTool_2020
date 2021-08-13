#library(RODBC)
library(lubridate)
library(zoo)
library(xts)
library(devtools)
# install_github("FluvialLandscapeLab/hydrogeom")
library(hydrogeom)
library(grDevices)

# fileslocation <- "C:/Users/t24x137/Desktop/TempTool_2020"
# setwd(fileslocation)

### CHANNEL TEMPERATURE ###
load("model_output/noShadeNoHypo.RData")
load("model_output/shade30.RData")
load("model_output/shade60.RData")
load("model_output/shade90.RData")
load("runs_using_2017_umatilla_aquifer_geometry/high/highHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/medHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/littleHypo870.RData")

### LONGWAVE FLUX ###
load("runs_using_2017_umatilla_aquifer_geometry/high/longwaveHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/longwaveMedHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/longwaveLittleHypo.RData")
load("model_output/longwaveNet90.RData")
load("model_output/longwaveNet60.RData")
load("model_output/longwaveNet30.RData")
load("model_output/longwaveNet.RData")

### LATENT FLUX ###
load("runs_using_2017_umatilla_aquifer_geometry/high/latentHighHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/latentMedHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/latentLittleHypo.RData")
load("model_output/latent90.RData")
load("model_output/latent60.RData")
load("model_output/latent30.RData")
load("model_output/latent.RData")

### SENSIBLE FLUX ###
load("runs_using_2017_umatilla_aquifer_geometry/high/sensibleHighHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/sensibleMedHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/sensibleLittleHypo.RData")
load("model_output/sensible90.RData")
load("model_output/sensible60.RData")
load("model_output/sensible30.RData")
load("model_output/sensible.RData")

### SHORTWAVE FLUX ###
load("runs_using_2017_umatilla_aquifer_geometry/high/shortwaveHighHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/shortwaveMedHypo.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/shortwaveLittleHypo.RData")
load("model_output/shortwaveNet90.RData")
load("model_output/shortwaveNet60.RData")
load("model_output/shortwaveNet30.RData")
load("model_output/shortwaveNet.RData")

### HYPORHEIC FLUX ###
load("runs_using_2017_umatilla_aquifer_geometry/high/Q_b_high.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/Q_b_med.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/Q_b_little.RData")


### CHANNEL TEMEPRATURE TO XTS/ZOO OBJECTS ###
reference <- xts(zoo(noShadeNoHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
shade90 <- shade90["2014/2018-01-01 00:00:00"]
shade60 <- shade60["2014/2018-01-01 00:00:00"]
shade30 <- shade30["2014/2018-01-01 00:00:00"]
hypohigh <- xts(zoo(coredata(highHypo870$cTemp$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hypomed <- xts(zoo(coredata(medHypo870$cTemp$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hypolit <- xts(zoo(coredata(littleHypo870$cTemp$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))



spHeatWater <- 4193

### CONVERT REF DATA TO XTS/ZOO ###
shortwaveNetRef <- xts(zoo(shortwaveNet$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveNetRef <- xts(zoo(longwaveNet$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentRef <- xts(zoo(latent$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleRef <- xts(zoo(sensible$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

### CONVERT LITTLE HYPO DATA TO XTS/ZOO ###
shortwaveLittleHypo <- xts(zoo(shortwaveLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveLittleHypo <- xts(zoo(longwaveLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentLittleHypo <- xts(zoo(latentLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleLittleHypo <- xts(zoo(sensibleLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hyporheicLittleHypo <- xts(zoo(Q_b_little, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

### CONVERT MED HYPO DATA TO XTS/ZOO ###
shortwaveMedHypo <- xts(zoo(shortwaveMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveMedHypo <- xts(zoo(longwaveMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentMedHypo <- xts(zoo(latentMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleMedHypo <- xts(zoo(sensibleMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hyporheicMedHypo <- xts(zoo(Q_b_med, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

### CONVERT HIGH HYPO DATA TO XTS/ZOO ###
shortwaveHighHypo <- xts(zoo(shortwaveHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveHighHypo <- xts(zoo(longwaveHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentHighHypo <- xts(zoo(latentHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleHighHypo <- xts(zoo(sensibleHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hyporheicHighHypo <- xts(zoo(Q_b_high, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

#
# littlehypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)
# #lFluxTot <- sum(littlehypoBins$returning)
# littleHypoChannelT <- coredata(hypolit)
# for(i in 2:19){
#   assign(paste0("tsz", i-1, "T_little"),
#          as.vector(coredata(xts(zoo(coredata(hypolit[[i]]), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065))))))
# }
# hypoTempNames <- paste0("tsz", 1:18, "T_little")
# hypDataList <- lapply(hypoTempNames, get)
# names(hypDataList) <- hypoTempNames
# hypdf <- do.call(data.frame, args = hypDataList)
# meanUpwellingTempLittle <- rowSums(hypdf * rep(littlehypoBins$returning[1:18], each = nrow(hypdf))) / sum(littlehypoBins$returning[1:18])
# hypoJoulesFlux_little <- ((meanUpwellingTempLittle - littleHypoChannelT)*littlehypoBins$entering[1]) * spHeatWater #kJ s-1 m-2
#
# ## testing ##
# test <- data.frame(a = rep(1, times = 4), b = rep(2, times = 4), c = rep(3, times = 4))
# testvector <- rep(c(3,2,0.5), each = 4)
#
#
# mediumhypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 23.96, b=-1.39)
# mFluxTot <- sum(mediumhypoBins$returning)
# medHypoChannelT <- coredata(hypomed)
# for(i in 2:19){
#   assign(paste0("tsz", i-1, "T_med"),
#          as.vector(coredata(xts(zoo(coredata(hypomed[[i]]), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065))))))
# }
# hypoTempNames <- paste0("tsz", 1:18, "T_med")
# hypDataList <- lapply(hypoTempNames, get)
# names(hypDataList) <- hypoTempNames
# hypdf <- do.call(data.frame, args = hypDataList)
# meanUpwellingTempMed <- rowSums(hypdf * rep(mediumhypoBins$returning[1:18], each = nrow(hypdf))) / sum(mediumhypoBins$returning[1:18])
# hypoJoulesFlux_med <- ((meanUpwellingTempMed - medHypoChannelT)*mediumhypoBins$entering[1]) * spHeatWater #kJ s-1 m-2
#
#
# highhypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.94, b=-1.39)
# hFluxTot <- sum(highhypoBins$returning)
# highHypoChannelT <- coredata(hypohigh)
# for(i in 2:19){
#   assign(paste0("tsz", i-1, "T_high"),
#          as.vector(coredata(xts(zoo(coredata(hypohigh[[i]]), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065))))))
# }
# hypoTempNames <- paste0("tsz", 1:18, "T_high")
# hypDataList <- lapply(hypoTempNames, get)
# names(hypDataList) <- hypoTempNames
# hypdf <- do.call(data.frame, args = hypDataList)
# meanUpwellingTempHigh <- rowSums(hypdf * rep(highhypoBins$returning[1:18], each = nrow(hypdf))) / sum(highhypoBins$returning[1:18])
# hypoJoulesFlux_high <- ((meanUpwellingTempHigh - highHypoChannelT)*highhypoBins$entering[1]) * spHeatWater #kJ s-1 m-2
#
#
# hyporheicLittleHypo <- xts(zoo(hypoJoulesFlux_little, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
# hyporheicMedHypo <- xts(zoo(hypoJoulesFlux_med, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
# hyporheicHighHypo <- xts(zoo(hypoJoulesFlux_high, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))


afewdays <- c("2014-01-15/2014-01-16", "2014-04-15/2014-04-16", "2014-07-15/2014-07-16", "2014-10-15/2014-10-16")


shade30Netkw <- sum(shortwaveNet30, longwaveNet30, sensible30, latent30)


### COLOR SHWIZZ ###
shade30gray = "gray37"
shade60gray = "gray60"
shade90gray = "gray85"

littlehypogray = "gray37"
medhypogray = "gray60"
highhypogray = "gray85"

refcol = "black"

yrandhalf <- "2016-01-01/2017-07-01"
lwdparam <- 2
swrange <- c(0,0.9)
lwrange <- c(-0.1, 0.07)
sensrange <- c(-0.1, 0.1)
latrange <- c(-0.35, 0.02)
hyporange <- c(-0.35, 0.35)
xaxislabels <- c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan", "Mar", "May", "Jul")
xaxisat <- c(ymd_hms("2016-01-01 00:00:00"), ymd_hms("2016-03-01 00:00:00"),
             ymd_hms("2016-05-01 00:00:00"), ymd_hms("2016-07-01 00:00:00"),
             ymd_hms("2016-09-01 00:00:00"), ymd_hms("2016-11-01 00:00:00"),
             ymd_hms("2017-01-01 00:00:00"), ymd_hms("2017-03-01 00:00:00"),
             ymd_hms("2017-05-01 00:00:00"), ymd_hms("2017-07-01 00:00:00"))

##### Only partial fluxes #####
png("plots/2017_umatilla/annualEnergyFluxes.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(5,2),
    mar = c(2,5,1,1),
    cex.lab = 1.3,
    cex.axis = 1.2,
    oma = c(0,0,0,0))
### SW SHADE SCENARIOS ###
plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
         ylab = expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet30[yrandhalf], max)), lwd = 2, col = shade30gray)

lines(as.zoo(shortwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet60[yrandhalf], max)), lwd = 2, col = shade60gray)

lines(as.zoo(shortwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet90[yrandhalf], max)), lwd = 2, col = shade90gray)
abline(h= 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LW SHADE SCENARIOS ###
plot.zoo(longwaveNetRef[yrandhalf], col = refcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(longwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(longwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


### SENSIBLE SHADE SCENARIOS ###
plot.zoo(sensibleRef[yrandhalf], col = refcol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(sensibleRef[yrandhalf], max)), lwd = 2, col = refcol)

lines(as.zoo(sensible30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(sensible30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(sensible60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(sensible60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(sensible90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(sensible90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LATENT SHADE SCENARIOS ###
plot.zoo(latentRef[yrandhalf], col = refcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(latent30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(latent30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(latent60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(latent60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(latent90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(latent90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### STREAMBED FLUX: SHADE SCENARIOS ###
plot.zoo(sensible30[yrandhalf], col = shade30gray, ylim = hyporange,
         type = "n",
         xaxt = "n",
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### SW: HYPORHEIC SCENARIOS ###
plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
         ylab =  expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)

lines(as.zoo(shortwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)

lines(as.zoo(shortwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LW: HYPORHEIC SCENARIOS ###
plot.zoo(longwaveNetRef[yrandhalf], col = refcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(longwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(longwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### SENSIBLE: HYPORHEIC SENARIOS ###
plot.zoo(sensibleRef[yrandhalf], col = refcol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(sensibleLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(sensibleMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(sensibleHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LATENT: HYPORHEIC SCENARIOS ###
plot.zoo(latentRef[yrandhalf], col = refcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")

lines(as.zoo(latentLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(latentMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(latentHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### HYPORHEIC: HYPORHEIC SCENARIOS ###
plot.zoo(hyporheicLittleHypo[yrandhalf], col = littlehypogray, lwd = lwdparam, ylim = hyporange,
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(hyporheicMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(hyporheicHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
dev.off()
#####

# par(mfcol = c(1,2))
# ### SW SHADE SCENARIOS ###
# plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
#          ylab = expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
#          xaxt = "n")
# lines(as.zoo(shortwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
# lines(as.zoo(apply.daily(shortwaveNet30[yrandhalf], max)), lwd = 2, col = shade30gray)
#
# lines(as.zoo(shortwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
# lines(as.zoo(apply.daily(shortwaveNet60[yrandhalf], max)), lwd = 2, col = shade60gray)
#
# lines(as.zoo(shortwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
# lines(as.zoo(apply.daily(shortwaveNet90[yrandhalf], max)), lwd = 2, col = shade90gray)
# abline(h= 0, lty = 2)
# axis(1, at = xaxisat, labels = xaxislabels)
#
# ### Hypo Solar- Hypo Hypo ###
# plot.zoo((shortwaveLittleHypo + hyporheicLittleHypo)[yrandhalf], col = littlehypogray, lwd = 2)
# lines(as.zoo((shortwaveMedHypo + hyporheicMedHypo)[yrandhalf]), col = medhypogray, lwd = 2)
# lines(as.zoo((shortwaveHighHypo + hyporheicHighHypo)[yrandhalf]), col = highhypogray, lwd = 2)


refall <- shortwaveNetRef + longwaveNetRef + sensibleRef + latentRef
shade30all <- shortwaveNet30 + longwaveNet30 + sensible30 + latent30
shade60all <- shortwaveNet60 + longwaveNet60 + sensible60 + latent60
shade90all <- shortwaveNet90 + longwaveNet90 + sensible90 + latent90
hypolittleall <- shortwaveLittleHypo + longwaveLittleHypo + sensibleLittleHypo + latentLittleHypo + hyporheicLittleHypo
hypomedall <- shortwaveMedHypo + longwaveMedHypo + sensibleMedHypo + latentMedHypo + hyporheicMedHypo
hypohighall <- shortwaveHighHypo + longwaveHighHypo + sensibleHighHypo + latentHighHypo + hyporheicHighHypo




##### All Qs ######
png("plots/2017_umatilla/annualEnergyFluxes_allQs.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(6,2),
    mar = c(2,5,1,1),
    cex.lab = 1.3,
    cex.axis = 1.2,
    oma = c(0,0,0,0))

### Net energy flux Q_c ###
plot.zoo(refall[yrandhalf], col = refcol, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shade30all[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(shade30all[yrandhalf]), max), col = adjustcolor(shade30gray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(shade30all[yrandhalf]), min), col = adjustcolor(shade30gray, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(shade60all[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(shade60all[yrandhalf]), max), col = adjustcolor(shade60gray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(shade60all[yrandhalf]), min), col = adjustcolor(shade60gray, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(shade90all[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(shade90all[yrandhalf]), max), col = adjustcolor(shade90gray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(shade90all[yrandhalf]), min), col = adjustcolor(shade90gray, alpha.f = 1), lwd = lwdparam)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### SW SHADE SCENARIOS ###
plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
         ylab = expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet30[yrandhalf], max)), lwd = 2, col = shade30gray)

lines(as.zoo(shortwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet60[yrandhalf], max)), lwd = 2, col = shade60gray)

lines(as.zoo(shortwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet90[yrandhalf], max)), lwd = 2, col = shade90gray)
abline(h= 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LW SHADE SCENARIOS ###
plot.zoo(longwaveNetRef[yrandhalf], col = refcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(longwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(longwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


### SENSIBLE SHADE SCENARIOS ###
plot.zoo(sensibleRef[yrandhalf], col = refcol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(sensibleRef[yrandhalf], max)), lwd = 2, col = refcol)

lines(as.zoo(sensible30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(sensible30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(sensible60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(sensible60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(sensible90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(sensible90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LATENT SHADE SCENARIOS ###
plot.zoo(latentRef[yrandhalf], col = refcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(latent30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(latent30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(latent60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(latent60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(latent90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(latent90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### STREAMBED FLUX: SHADE SCENARIOS ###
plot.zoo(sensible30[yrandhalf], col = shade30gray, ylim = hyporange,
         type = "n",
         xaxt = "n",
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##########################################################
### Net HEat FLux Q_c Hypo ###
plot.zoo(refall[yrandhalf], col = refcol, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(hypolittleall[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypolittleall[yrandhalf]), max), col = adjustcolor(littlehypogray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypolittleall[yrandhalf]), min), col = adjustcolor(littlehypogray, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypomedall[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypomedall[yrandhalf]), max), col = adjustcolor(medhypogray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypomedall[yrandhalf]), min), col = adjustcolor(medhypogray, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypohighall[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypohighall[yrandhalf]), max), col = adjustcolor(highhypogray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypohighall[yrandhalf]), min), col = adjustcolor(highhypogray, alpha.f = 1), lwd = lwdparam)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


### SW: HYPORHEIC SCENARIOS ###
plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
         ylab =  expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)

lines(as.zoo(shortwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)

lines(as.zoo(shortwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LW: HYPORHEIC SCENARIOS ###
plot.zoo(longwaveNetRef[yrandhalf], col = refcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(longwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(longwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### SENSIBLE: HYPORHEIC SENARIOS ###
plot.zoo(sensibleRef[yrandhalf], col = refcol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(sensibleLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(sensibleMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(sensibleHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### LATENT: HYPORHEIC SCENARIOS ###
plot.zoo(latentRef[yrandhalf], col = refcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")

lines(as.zoo(latentLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(latentMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(latentHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

### HYPORHEIC: HYPORHEIC SCENARIOS ###
plot.zoo(hyporheicLittleHypo[yrandhalf], col = littlehypogray, lwd = lwdparam, ylim = hyporange,
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(hyporheicMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(hyporheicHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
dev.off()
#####




###################################################
### Check atm heat flux for hyporheic scenarios ###
load("runs_using_2017_umatilla_aquifer_geometry/high/atmChannelHeatFlux_high.RData")
allatmfluxes_high <- data.frame(sw = coredata(shortwaveHighHypo),
                                lw = coredata(longwaveHighHypo),
                                sens = coredata(sensibleHighHypo),
                                lat = coredata(latentHighHypo))

allatmfluxes_high_sum <- rowSums(allatmfluxes_high)

plot(atmChannelHeatFlux$svValue, type = "l")
lines(allatmfluxes_high_sum, col = "red")

allatmfluxes_med <- data.frame(sw = coredata(shortwaveMedHypo),
                                lw = coredata(longwaveMedHypo),
                                sens = coredata(sensibleMedHypo),
                                lat = coredata(latentMedHypo))

allatmfluxes_med_sum <- rowSums(allatmfluxes_med)
lines(allatmfluxes_med_sum, col = "blue")

allatmfluxes_little <- data.frame(sw = coredata(shortwaveLittleHypo),
                               lw = coredata(longwaveLittleHypo),
                               sens = coredata(sensibleLittleHypo),
                               lat = coredata(latentLittleHypo))

allatmfluxes_little_sum <- rowSums(allatmfluxes_little)
lines(allatmfluxes_little_sum, col = "forestgreen")




####################
##### OLD PLOTS ####
####################



# Plot Controls

sensiblelty = 6
longwavelty = 3
latentlty = 4
shortwavelty = 5
totallty = 1
hyporheiclty = 1

totalcol = "gray"

totallwd = 5
zerolty = 1
zerocol = "black"


png("plots/2017_umatilla/energyFluxes_Daily_ShadeScenarios.png",
    height  = 1350*3.5, width = 2000*3.5, res = 72*5)
layout(matrix(
  c(14, 1,4,7,10, 13,
    15, 2,5,8,11, 13,
    16, 3,6,9,12, 13
  ),
  nrow = 3, ncol = 6, byrow = T),
  heights = c(1, 1, 1),
  widths = c(0.3, 1, 1, 1, 1, 0.75))

par(mar = c(4, 2, 4, 0), oma = c(0, 0, 0, 2), cex.axis = 2, cex.main = 2.5, lend = 2)
for(i in 1:4){

  kWsum30 <- shortwaveNet30[afewdays[i]] + longwaveNet30[afewdays[i]] + latent30[afewdays[i]] + sensible30[afewdays[i]]
  kWsum60 <- shortwaveNet60[afewdays[i]] + longwaveNet60[afewdays[i]] + latent60[afewdays[i]] + sensible60[afewdays[i]]
  kWsum90 <- shortwaveNet90[afewdays[i]] + longwaveNet90[afewdays[i]] + latent90[afewdays[i]] + sensible90[afewdays[i]]

  kWsumtotal <- shortwaveNet30[afewdays] + longwaveNet30[afewdays] + latent30[afewdays] + sensible30[afewdays]

  rangetoplot <- c(min(shortwaveNet30[afewdays], longwaveNet30[afewdays], sensible30[afewdays], latent30[afewdays], kWsumtotal) - 0.04,
                   max(shortwaveNet30[afewdays], longwaveNet30[afewdays], sensible30[afewdays], latent30[afewdays], kWsumtotal) + 0.04)

  plot.zoo(shortwaveNet30[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = month(afewdays[i], label = T, abbr = F))
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNet30[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latent30[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensible30[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsum30,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))

  plot.zoo(shortwaveNet60[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = month(afewdays[i], label = T, abbr = F))
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNet60[afewdays[i]],lty = longwavelty, lwd = 2)
  lines(latent60[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensible60[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsum60,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))

  plot.zoo(shortwaveNet90[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = month(afewdays[i], label = T, abbr = F))
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNet90[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latent90[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensible90[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsum90,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels = c("", "", "", "", ""))

}

#### plot 13 (legend) ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
legend("center", c("Shortwave", "Longwave", "Sensible", "Latent", "Total"),
       lty = c(shortwavelty, longwavelty, sensiblelty, latentlty, totallty),
       title = "Legend",
       lwd = c(2,2,2,2,3),
       col = c(1,1,1,1,totalcol),
       cex = 2.5,
       bty = "n")

#### plot 14 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "30% Attenuation", cex = 2.3, srt = 90, font = 2)

#### plot 15 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "60% Attenuation", cex = 2.3, srt = 90, font = 2)

#### plot 16 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "90% Attenuation", cex = 2.3, srt = 90, font = 2)

dev.off()

####################
png("d:/Users/sarah.fogg/Dropbox/PAPER1/figs/energyFluxes_Daily_HEScenarios.png",
    height  = 1350*3.5, width = 2000*3.5, res = 72*5)
layout(matrix(
  c(14, 1,4,7,10, 13,
    15, 2,5,8,11, 13,
    16, 3,6,9,12, 13
  ),
  nrow = 3, ncol = 6, byrow = T),
  heights = c(1, 1, 1),
  widths = c(0.3, 1, 1, 1, 1, 0.75))

par(mar = c(4, 2, 4, 0), oma = c(0, 0, 0, 2), cex.axis = 2, cex.main = 2.5, lend = 2)
for(i in 1:4){

  kWsumLittleHypo <- shortwaveNetLittleHypo[afewdays[i]] + longwaveNetLittleHypo[afewdays[i]] + latentLittleHypo[afewdays[i]] + sensibleLittleHypo[afewdays[i]] + hyporheicLittleHypo[afewdays[i]]
  kWsumMedHypo <- shortwaveNetMedHypo[afewdays[i]] + longwaveNetMedHypo[afewdays[i]] + latentMedHypo[afewdays[i]] + sensibleMedHypo[afewdays[i]] + hyporheicMedHypo[afewdays[i]]
  kWsumHighHypo <- shortwaveNetHighHypo[afewdays[i]] + longwaveNetHighHypo[afewdays[i]] + latentHighHypo[afewdays[i]] + sensibleHighHypo[afewdays[i]] + hyporheicHighHypo[afewdays[i]]

  rangetoplot <- c(min(shortwaveNetHighHypo[afewdays], longwaveNetHighHypo[afewdays], sensibleHighHypo[afewdays], latentHighHypo[afewdays], kWsumHighHypo, hyporheicHighHypo[afewdays]) - 0.04,
                   max(shortwaveNetHighHypo[afewdays], longwaveNetHighHypo[afewdays], sensibleHighHypo[afewdays], latentHighHypo[afewdays], kWsumHighHypo, hyporheicHighHypo[afewdays]) + 0.04)


  plot.zoo(shortwaveNetLittleHypo[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = expression(paste("Energy (kJ ", sec^-1, m^-2, ")")),
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           main = month(afewdays[i], label = T, abbr = F))
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetLittleHypo[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentLittleHypo[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleLittleHypo[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(hyporheicLittleHypo[afewdays[i]], lty = hyporheiclty, lwd = 2)
  lines(kWsumLittleHypo,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)

  plot.zoo(shortwaveNetMedHypo[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = expression(paste("Energy (kJ ", sec^-1, m^-2, ")")),
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           main = month(afewdays[i], label = T, abbr = F))
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetMedHypo[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentMedHypo[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleMedHypo[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(hyporheicMedHypo[afewdays[i]], lty = hyporheiclty, lwd = 2)
  lines(kWsumMedHypo,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)

  plot.zoo(shortwaveNetHighHypo[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = expression(paste("Energy (kJ ", sec^-1, m^-2, ")")),
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           main = month(afewdays[i], label = T, abbr = F))
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetHighHypo[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentHighHypo[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleHighHypo[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(hyporheicHighHypo[afewdays[i]], lty = hyporheiclty, lwd = 2)
  lines(kWsumHighHypo,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
}
#### plot 13 (legend) ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
legend("center", c("Shortwave", "Longwave", "Sensible", "Latent", "Hyporheic","Total"),
       lty = c(shortwavelty, longwavelty, sensiblelty, latentlty, hyporheiclty, totallty),
       title = "Energy Flux:",
       lwd = c(2,2,2,2,2,3),
       col = c(1,1,1,1,1,totalcol),
       cex = 2.5,
       bty = "n")

#### plot 14 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Low HE", cex = 2.3, srt = 90, font = 2)

#### plot 15 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Moderate HE", cex = 2.3, srt = 90, font = 2)

#### plot 16 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "High HE", cex = 2.3, srt = 90, font = 2)

dev.off()

#######




png("d:/Users/sarah.fogg/Dropbox/PAPER1/figs/energyFluxes_Daily_alls.png",
    height  = 2000*3.5, width = 1700*3.5, res = 72*5)
layout(matrix(
  c(37, 38, 39, 40, 41, 29,
    30, 1, 8, 15, 22, 29,
    31, 2, 9, 16, 23, 29,
    32, 3, 10, 17, 24, 29,
    33, 4, 11, 18, 25, 29,
    34, 5, 12, 19, 26, 29,
    35, 6, 13, 20, 27, 29,
    36, 7, 14, 21, 28, 29,
    42, 42, 42, 42, 42, 42
  ),
  nrow = 9, ncol = 6, byrow = T),
  heights = c(0.3, 1, 1, 1, 1, 1, 1, 1, 0.3),
  widths = c(0.3, 1, 1, 1, 1, 0.75))

par(mar = c(4, 2, 0, 0), oma = c(0, 0, 0, 2), cex.axis = 2, cex.main = 2.5, lend = 2)

#### plots 1 - 28 ####
for(i in 1:4){

  kWsumRef <- shortwaveNetRef[afewdays[i]] + longwaveNetRef[afewdays[i]] + latentRef[afewdays[i]] + sensibleRef[afewdays[i]]
  kWsum30 <- shortwaveNet30[afewdays[i]] + longwaveNet30[afewdays[i]] + latent30[afewdays[i]] + sensible30[afewdays[i]]
  kWsum60 <- shortwaveNet60[afewdays[i]] + longwaveNet60[afewdays[i]] + latent60[afewdays[i]] + sensible60[afewdays[i]]
  kWsum90 <- shortwaveNet90[afewdays[i]] + longwaveNet90[afewdays[i]] + latent90[afewdays[i]] + sensible90[afewdays[i]]

  kWsumLittleHypo <- shortwaveNetLittleHypo[afewdays[i]] + longwaveNetLittleHypo[afewdays[i]] + latentLittleHypo[afewdays[i]] + sensibleLittleHypo[afewdays[i]] + hyporheicLittleHypo[afewdays[i]]
  kWsumMedHypo <- shortwaveNetMedHypo[afewdays[i]] + longwaveNetMedHypo[afewdays[i]] + latentMedHypo[afewdays[i]] + sensibleMedHypo[afewdays[i]] + hyporheicMedHypo[afewdays[i]]
  kWsumHighHypo <- shortwaveNetHighHypo[afewdays[i]] + longwaveNetHighHypo[afewdays[i]] + latentHighHypo[afewdays[i]] + sensibleHighHypo[afewdays[i]] + hyporheicHighHypo[afewdays[i]]

  kWsumtotal <- shortwaveNet30[afewdays] + longwaveNet30[afewdays] + latent30[afewdays] + sensible30[afewdays]

  rangetoplot <- c(min(shortwaveNetHighHypo[afewdays], longwaveNetHighHypo[afewdays], sensibleHighHypo[afewdays], latentHighHypo[afewdays], kWsumHighHypo, hyporheicHighHypo[afewdays]) - 0.04,
                   max(shortwaveNetHighHypo[afewdays], longwaveNetHighHypo[afewdays], sensibleHighHypo[afewdays], latentHighHypo[afewdays], kWsumHighHypo, hyporheicHighHypo[afewdays]) + 0.04)

  ## REFERENCE ##
  plot.zoo(shortwaveNetRef[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = "")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetRef[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentRef[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleRef[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsumRef,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))


  ## SHADING ##

  plot.zoo(shortwaveNet30[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = "")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNet30[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latent30[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensible30[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsum30,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))

  plot.zoo(shortwaveNet60[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = "")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNet60[afewdays[i]],lty = longwavelty, lwd = 2)
  lines(latent60[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensible60[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsum60,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))

  plot.zoo(shortwaveNet90[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = "",
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           xaxt = "n",
           main = "")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNet90[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latent90[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensible90[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(kWsum90,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNet30[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels = c("", "", "", "", ""))

  kWsumLittleHypo <- shortwaveNetLittleHypo[afewdays[i]] + longwaveNetLittleHypo[afewdays[i]] + latentLittleHypo[afewdays[i]] + sensibleLittleHypo[afewdays[i]] + hyporheicLittleHypo[afewdays[i]]
  kWsumMedHypo <- shortwaveNetMedHypo[afewdays[i]] + longwaveNetMedHypo[afewdays[i]] + latentMedHypo[afewdays[i]] + sensibleMedHypo[afewdays[i]] + hyporheicMedHypo[afewdays[i]]
  kWsumHighHypo <- shortwaveNetHighHypo[afewdays[i]] + longwaveNetHighHypo[afewdays[i]] + latentHighHypo[afewdays[i]] + sensibleHighHypo[afewdays[i]] + hyporheicHighHypo[afewdays[i]]


  plot.zoo(shortwaveNetLittleHypo[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = expression(paste("Energy (kJ ", sec^-1, m^-2, ")")),
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           main = "",
           xaxt = "n")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetLittleHypo[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentLittleHypo[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleLittleHypo[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(hyporheicLittleHypo[afewdays[i]], lty = hyporheiclty, lwd = 2)
  lines(kWsumLittleHypo,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNetLittleHypo[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))

  plot.zoo(shortwaveNetMedHypo[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = expression(paste("Energy (kJ ", sec^-1, m^-2, ")")),
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           main = "",
           xaxt = "n")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetMedHypo[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentMedHypo[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleMedHypo[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(hyporheicMedHypo[afewdays[i]], lty = hyporheiclty, lwd = 2)
  lines(kWsumMedHypo,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNetMedHypo[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))


  plot.zoo(shortwaveNetHighHypo[afewdays[i]],
           ylim = rangetoplot,
           lty = shortwavelty,
           lwd = 2,
           ylab = expression(paste("Energy (kJ ", sec^-1, m^-2, ")")),
           xlab = "",
           yaxt = c("s", "n", "n", "n")[i],
           main = "",
           xaxt = "n")
  abline(h = 0, col = zerocol, lty = zerolty)
  lines(longwaveNetHighHypo[afewdays[i]], lty = longwavelty, lwd = 2)
  lines(latentHighHypo[afewdays[i]], lty = latentlty, lwd = 2)
  lines(sensibleHighHypo[afewdays[i]], lty = sensiblelty, lwd = 2)
  lines(hyporheicHighHypo[afewdays[i]], lty = hyporheiclty, lwd = 2)
  lines(kWsumHighHypo,
        col = totalcol,
        lty = totallty,
        lwd = totallwd)
  axis(1, at = index(shortwaveNetHighHypo[afewdays[i]])[c(1,25,48)] + c(0,0,3600), labels = c(15, 16, 17))
  axis(2, at = c(-0.2, 0.0, 0.2, 0.4, 0.6), labels =  c("", "", "", "", ""))

}

#### plot 29 (legend) ####

plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
legend("center", c("Shortwave", "Longwave", "Sensible", "Latent", "Hyporheic","Total"),
       lty = c(shortwavelty, longwavelty, sensiblelty, latentlty, hyporheiclty, totallty),
       title = "Energy Flux",
       lwd = c(2,2,2,2,2,3),
       col = c(1,1,1,1,1,totalcol),
       cex = 2.5,
       bty = "n")

#### plot 30 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Reference", cex = 2.3, srt = 90, font = 2)

#### plot 31 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Low Shade", cex = 2.3, srt = 90, font = 2)

#### plot 32 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Moderate Shade", cex = 2.3, srt = 90, font = 2)

#### plot 33 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "High Shade", cex = 2.3, srt = 90, font = 2)

#### plot 34 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Low HE", cex = 2.3, srt = 90, font = 2)

#### plot 35 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "Moderate HE", cex = 2.3, srt = 90, font = 2)

#### plot 36 ####
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 6, y = 5.5, expression(paste("Energy (kJ ", sec^-1, m^-2, ")")), cex = 2, srt = 90)
text(x = 2, y = 5.5, "High HE", cex = 2.3, srt = 90, font = 2)

#### plot 37 ####
plot.new()

#### plot 38  "January" ####
par(mar = c(0,0,0,0))
plot(x = 1:1, y = 1:1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 1, y = 0.8, "January", cex = 3, font = 2)

#### plot 39  "April" ####
plot(x = 1:1, y = 1:1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 1, y = 0.8, "April", cex = 3, font = 2)

#### plot 40 "July" ####
plot(x = 1:1, y = 1:1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 1, y = 0.8, "July", cex = 3, font = 2)

#### plot 41 "October" ####
plot(x = 1:1, y = 1:1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 1, y = 0.8, "October", cex = 3, font = 2)

#### plot 42 "Day of Month" ####
plot(x = 1:1, y = 1:1, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 0.975, y = 1.15, "Day of Month", cex = 3, font = 1)


dev.off()






