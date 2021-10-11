
#### TRANSFORM DATA TO XTS/ZOO AND SAVE FOR EASY UPLOAD IN FUTURE SCRIPTS ####

library(lubridate)
library(zoo)
library(xts)

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

### NET FLUXES ####
refall <- shortwaveNetRef + longwaveNetRef + sensibleRef + latentRef
shade30all <- shortwaveNet30 + longwaveNet30 + sensible30 + latent30
shade60all <- shortwaveNet60 + longwaveNet60 + sensible60 + latent60
shade90all <- shortwaveNet90 + longwaveNet90 + sensible90 + latent90
hypolittleall <- shortwaveLittleHypo + longwaveLittleHypo + sensibleLittleHypo + latentLittleHypo + hyporheicLittleHypo
hypomedall <- shortwaveMedHypo + longwaveMedHypo + sensibleMedHypo + latentMedHypo + hyporheicMedHypo
hypohighall <- shortwaveHighHypo + longwaveHighHypo + sensibleHighHypo + latentHighHypo + hyporheicHighHypo

# save(refall, shade30all, shade60all, shade90all, hypohighall, hypomedall, hypolittleall,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/netHeats.R")


# save(shortwaveHighHypo, longwaveHighHypo, latentHighHypo, sensibleHighHypo, hyporheicHighHypo,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/highHypoHeat.R")
# save(shortwaveMedHypo, longwaveMedHypo, latentMedHypo, sensibleMedHypo, hyporheicMedHypo,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/medHypoHeat.R")
# save(shortwaveLittleHypo, longwaveLittleHypo, latentLittleHypo, sensibleLittleHypo, hyporheicLittleHypo,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/littleHypoHeat.R")
# save(shortwaveNetRef, longwaveNetRef, latentRef, sensibleRef,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/refHeat.R")
# save(shortwaveNet30, longwaveNet30, latent30, sensible30,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/shade30Heat.R")
# save(shortwaveNet60, longwaveNet60, latent60, sensible60,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/shade60Heat.R")
# save(shortwaveNet90, longwaveNet90, latent90, sensible90,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/shade90Heat.R")
# save(reference, shade30, shade60, shade90, hypohigh, hypomed, hypolit,
#      file = "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/temperatureOutput.R")





spHeatWater <- 4193

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
