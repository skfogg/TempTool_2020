#library(RODBC)
library(lubridate)
library(zoo)
library(xts)
library(devtools)
#install_github("FluvialLandscapeLab/hydrogeom")
library(hydrogeom)

# fileslocation <- "C:/Users/t24x137/Desktop/TempTool_2020"
# setwd(fileslocation)

load("model_output/noShadeNoHypo.RData")
load("model_output/shade30.RData")
load("model_output/shade60.RData")
load("model_output/shade90.RData")
load("runs_using_2017_umatilla_aquifer_geometry/highHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/medHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/littleHypo870.RData")

load("longwaveNetHighHypo.RData")
load("longwaveNetLittleHypo.RData")
load("longwaveNetMedHypo.RData")
load("longwaveNet90.RData")
load("longwaveNet60.RData")
load("longwaveNet30.RData")
load("longwaveNet.RData")


load("latentHighHypo.RData")
load("latentLittleHypo.RData")
load("latentMedHypo.RData")
load("latent90.RData")
load("latent60.RData")
load("latent30.RData")
load("latent.RData")

load("sensibleHighHypo.RData")
load("sensibleLittleHypo.RData")
load("sensibleMedHypo.RData")
load("sensible90.RData")
load("sensible60.RData")
load("sensible30.RData")
load("sensible.RData")

load("shortwaveNetHighHypo.RData")
load("shortwaveNetLittleHypo.RData")
load("shortwaveNetMedHypo.RData")
load("shortwaveNet90.RData")
load("shortwaveNet60.RData")
load("shortwaveNet30.RData")
load("shortwaveNet.RData")

reference <- xts(zoo(noShadeNoHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
shade90 <- shade90["2014/2018-01-01 00:00:00"]
shade60 <- shade60["2014/2018-01-01 00:00:00"]
shade30 <- shade30["2014/2018-01-01 00:00:00"]
hypohigh <- xts(zoo(coredata(highHypo$cTemp$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hypomed <- xts(zoo(coredata(medHypo$cTemp$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hypolit <- xts(zoo(coredata(littleHypo$cTemp$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

shade30gray = "gray37"
shade60gray = "gray60"
shade90gray = "gray85"

littlehypogray = "gray37"
medhypogray = "gray60"
highhypogray = "gray85"

spHeatWater <- 4193

shortwaveNetRef <- xts(zoo(shortwaveNet$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveNetRef <- xts(zoo(longwaveNet$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentRef <- xts(zoo(latent$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleRef <- xts(zoo(sensible$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

shortwaveNetLittleHypo <- xts(zoo(shortwaveNetLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveNetLittleHypo <- xts(zoo(longwaveNetLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentLittleHypo <- xts(zoo(latentLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleLittleHypo <- xts(zoo(sensibleLittleHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

shortwaveNetMedHypo <- xts(zoo(shortwaveNetMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveNetMedHypo <- xts(zoo(longwaveNetMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentMedHypo <- xts(zoo(latentMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleMedHypo <- xts(zoo(sensibleMedHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

shortwaveNetHighHypo <- xts(zoo(shortwaveNetHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
longwaveNetHighHypo <- xts(zoo(longwaveNetHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
latentHighHypo <- xts(zoo(latentHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
sensibleHighHypo <- xts(zoo(sensibleHighHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

littlehypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 11.98, b=-1.39)
#lFluxTot <- sum(littlehypoBins$returning)
littleHypoChannelT <- coredata(hypolit)
for(i in 2:19){
  assign(paste0("tsz", i-1, "T_little"),
         as.vector(coredata(xts(zoo(coredata(littleHypo[[i]]$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065))))))
}
hypoTempNames <- paste0("tsz", 1:18, "T_little")
hypDataList <- lapply(hypoTempNames, get)
names(hypDataList) <- hypoTempNames
hypdf <- do.call(data.frame, args = hypDataList)
meanUpwellingTempLittle <- rowSums(hypdf * rep(littlehypoBins$returning[1:18], each = nrow(hypdf))) / sum(littlehypoBins$returning[1:18])
hypoJoulesFlux_little <- ((meanUpwellingTempLittle - littleHypoChannelT)*littlehypoBins$entering[1]) * spHeatWater #kJ s-1 m-2

## testing ##
test <- data.frame(a = rep(1, times = 4), b = rep(2, times = 4), c = rep(3, times = 4))
testvector <- rep(c(3,2,0.5), each = 4)



mFluxTot <- sum(mediumhypoBins$returning)
mediumhypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 23.96, b=-1.39)
medHypoChannelT <- coredata(hypomed)
for(i in 2:19){
  assign(paste0("tsz", i-1, "T_med"),
         as.vector(coredata(xts(zoo(coredata(medHypo[[i]]$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065))))))
}
hypoTempNames <- paste0("tsz", 1:18, "T_med")
hypDataList <- lapply(hypoTempNames, get)
names(hypDataList) <- hypoTempNames
hypdf <- do.call(data.frame, args = hypDataList)
meanUpwellingTempMed <- rowSums(hypdf * rep(mediumhypoBins$returning[1:18], each = nrow(hypdf))) / sum(mediumhypoBins$returning[1:18])
hypoJoulesFlux_med <- ((meanUpwellingTempMed - medHypoChannelT)*mediumhypoBins$entering[1]) * spHeatWater #kJ s-1 m-2


highhypoBins <- hyporheicBins(18, 2, 60, 182*86400, 0.25, 35.94, b=-1.39)
hFluxTot <- sum(highhypoBins$returning)
highHypoChannelT <- coredata(hypohigh)
for(i in 2:19){
  assign(paste0("tsz", i-1, "T_high"),
         as.vector(coredata(xts(zoo(coredata(highHypo[[i]]$svValue), order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065))))))
}
hypoTempNames <- paste0("tsz", 1:18, "T_high")
hypDataList <- lapply(hypoTempNames, get)
names(hypDataList) <- hypoTempNames
hypdf <- do.call(data.frame, args = hypDataList)
meanUpwellingTempHigh <- rowSums(hypdf * rep(highhypoBins$returning[1:18], each = nrow(hypdf))) / sum(highhypoBins$returning[1:18])
hypoJoulesFlux_high <- ((meanUpwellingTempHigh - highHypoChannelT)*highhypoBins$entering[1]) * spHeatWater #kJ s-1 m-2


hyporheicLittleHypo <- xts(zoo(hypoJoulesFlux_little, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hyporheicMedHypo <- xts(zoo(hypoJoulesFlux_med, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
hyporheicHighHypo <- xts(zoo(hypoJoulesFlux_high, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))


afewdays <- c("2014-01-15/2014-01-16", "2014-04-15/2014-04-16", "2014-07-15/2014-07-16", "2014-10-15/2014-10-16")




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


png("d:/Users/sarah.fogg/Dropbox/PAPER1/figs/energyFluxes_Daily_ShadeScenarios.png",
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



#########


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






