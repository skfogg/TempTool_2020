#library(RODBC)
library(lubridate)
library(zoo)
library(xts)
library(devtools)
# install_github("FluvialLandscapeLab/hydrogeom")
library(hydrogeom)
library(grDevices)

file_loc <- "C:/Users/skati/Documents/TempTool_2020/runs_using_2017_umatilla_aquifer_geometry/xts_zooed_data/"

load(paste0(file_loc, "highHypoHeat.R"))
load(paste0(file_loc, "medHypoHeat.R"))
load(paste0(file_loc, "littleHypoHeat.R"))
load(paste0(file_loc, "refHeat.R"))
load(paste0(file_loc, "shade30Heat.R"))
load(paste0(file_loc, "shade60Heat.R"))
load(paste0(file_loc, "shade90Heat.R"))
load(paste0(file_loc, "temperatureOutput.R"))


afewdays <- c("2014-01-15/2014-01-16", "2014-04-15/2014-04-16", "2014-07-15/2014-07-16", "2014-10-15/2014-10-16")
year <- "2016"


##### CALCULATE BUDGETS ####
source('~/TempTool_2020/calc_budget.R')

refbudget <- calc_budget(shortwaveNetRef[year], longwaveNetRef[year], sensibleRef[year], latentRef[year])
shade30budget <- calc_budget(shortwaveNet30[year], longwaveNet30[year], sensible30[year], latent30[year])
shade60budget <- calc_budget(shortwaveNet60[year], longwaveNet60[year], sensible60[year], latent60[year])
shade90budget <- calc_budget(shortwaveNet90[year], longwaveNet90[year], sensible90[year], latent90[year])
hypolittlebudget <- calc_budget(sw = shortwaveLittleHypo[year],
                                lw = longwaveLittleHypo[year],
                                sens = sensibleLittleHypo[year],
                                lat = latentLittleHypo[year],
                                hypo = hyporheicLittleHypo[year])
hypomedbudget <- calc_budget(shortwaveMedHypo[year], longwaveMedHypo[year], sensibleMedHypo[year], latentMedHypo[year], hyporheicMedHypo[year])
hypohighbudget <- calc_budget(shortwaveHighHypo[year], longwaveHighHypo[year], sensibleHighHypo[year], latentHighHypo[year], hyporheicHighHypo[year])

allbudgets <- rbind(hypohighbudget, hypomedbudget, hypolittlebudget, refbudget, shade30budget, shade60budget, shade90budget)
rownames(allbudgets) <- c("highHE", "modHE", "lowHE", "Control", "highShade", "modShade", "lowShade")
write.csv(allbudgets, "umatilla_2017_annual_budgets.csv")

##### CALCULATE ABSOLUTE ANNUAL FLUX #####
source('~/TempTool_2020/calc_absolute_annual.R')
refabs <- calc_absolute_annual(shortwaveNetRef[year], longwaveNetRef[year], sensibleRef[year], latentRef[year])
shade30abs <- calc_absolute_annual(shortwaveNet30[year], longwaveNet30[year], sensible30[year], latent30[year])
shade60abs <- calc_absolute_annual(shortwaveNet60[year], longwaveNet60[year], sensible60[year], latent60[year])
shade90abs <- calc_absolute_annual(shortwaveNet90[year], longwaveNet90[year], sensible90[year], latent90[year])
hypolittleabs <- calc_absolute_annual(sw = shortwaveLittleHypo[year],
                                lw = longwaveLittleHypo[year],
                                sens = sensibleLittleHypo[year],
                                lat = latentLittleHypo[year],
                                hypo = hyporheicLittleHypo[year])
hypomedabs <- calc_absolute_annual(shortwaveMedHypo[year], longwaveMedHypo[year], sensibleMedHypo[year], latentMedHypo[year], hyporheicMedHypo[year])
hypohighabs <- calc_absolute_annual(shortwaveHighHypo[year], longwaveHighHypo[year], sensibleHighHypo[year], latentHighHypo[year], hyporheicHighHypo[year])

##### CALCULATE GAIN BUDGET & LOSS BUDGET #####
## calc_budget2 treats the loss and gain as separate budgets
source('~/TempTool_2020/calc_budget2.R')
refbudget2 <- calc_budget2(shortwaveNetRef[year], longwaveNetRef[year], sensibleRef[year], latentRef[year])
shade30budget2 <- calc_budget2(shortwaveNet30[year], longwaveNet30[year], sensible30[year], latent30[year])
shade60budget2 <- calc_budget2(shortwaveNet60[year], longwaveNet60[year], sensible60[year], latent60[year])
shade90budget2 <- calc_budget2(shortwaveNet90[year], longwaveNet90[year], sensible90[year], latent90[year])
hypolittlebudget2 <- calc_budget2(sw = shortwaveLittleHypo[year],
                                lw = longwaveLittleHypo[year],
                                sens = sensibleLittleHypo[year],
                                lat = latentLittleHypo[year],
                                hypo = hyporheicLittleHypo[year])
hypomedbudget2 <- calc_budget2(shortwaveMedHypo[year], longwaveMedHypo[year], sensibleMedHypo[year], latentMedHypo[year], hyporheicMedHypo[year])
hypohighbudget2 <- calc_budget2(shortwaveHighHypo[year], longwaveHighHypo[year], sensibleHighHypo[year], latentHighHypo[year], hyporheicHighHypo[year])

allbudgets2 <- rbind(hypohighbudget2, hypomedbudget2, hypolittlebudget2, refbudget2, shade30budget2, shade60budget2, shade90budget2)
rownames(allbudgets2) <- c("highHE_gain", "highHE_loss",
                          "modHE_gain", "modHE_loss",
                          "lowHE_gain", "lowHE_loss",
                          "control_gain", "control_loss",
                          "highShade_gain", "highShade_loss",
                          "modShade_gain", "modShade_loss",
                          "lowShade_gain", "lowShade_loss")
write.csv(allbudgets2, "umatilla_2017_annual_gainloss_budgets.csv")

##### CALCULATE BUDGET 3 ####
### calc_budget3 treats loss and gain as part of same total budget
source('~/TempTool_2020/calc_budget3.R')
refbudget3 <- calc_budget3(shortwaveNetRef[year], longwaveNetRef[year], sensibleRef[year], latentRef[year])
shade30budget3 <- calc_budget3(shortwaveNet30[year], longwaveNet30[year], sensible30[year], latent30[year])
shade60budget3 <- calc_budget3(shortwaveNet60[year], longwaveNet60[year], sensible60[year], latent60[year])
shade90budget3 <- calc_budget3(shortwaveNet90[year], longwaveNet90[year], sensible90[year], latent90[year])
hypolittlebudget3 <- calc_budget3(sw = shortwaveLittleHypo[year],
                                  lw = longwaveLittleHypo[year],
                                  sens = sensibleLittleHypo[year],
                                  lat = latentLittleHypo[year],
                                  hypo = hyporheicLittleHypo[year])
hypomedbudget3 <- calc_budget3(shortwaveMedHypo[year], longwaveMedHypo[year], sensibleMedHypo[year], latentMedHypo[year], hyporheicMedHypo[year])
hypohighbudget3 <- calc_budget3(shortwaveHighHypo[year], longwaveHighHypo[year], sensibleHighHypo[year], latentHighHypo[year], hyporheicHighHypo[year])


##### CALCULATE ANNUAL FLUXES #####
source('~/TempTool_2020/calc_annual_fluxes.R')
refannflux <- calc_annual_fluxes(shortwaveNetRef[year], longwaveNetRef[year], sensibleRef[year], latentRef[year])
shade30annflux <- calc_annual_fluxes(shortwaveNet30[year], longwaveNet30[year], sensible30[year], latent30[year])
shade60annflux <- calc_annual_fluxes(shortwaveNet60[year], longwaveNet60[year], sensible60[year], latent60[year])
shade90annflux <- calc_annual_fluxes(shortwaveNet90[year], longwaveNet90[year], sensible90[year], latent90[year])
hypolittleannflux <- calc_annual_fluxes(sw = shortwaveLittleHypo[year],
                                      lw = longwaveLittleHypo[year],
                                      sens = sensibleLittleHypo[year],
                                      lat = latentLittleHypo[year],
                                      hypo = hyporheicLittleHypo[year])
hypomedannflux <- calc_annual_fluxes(shortwaveMedHypo[year], longwaveMedHypo[year], sensibleMedHypo[year], latentMedHypo[year], hyporheicMedHypo[year])
hypohighannflux <- calc_annual_fluxes(shortwaveHighHypo[year], longwaveHighHypo[year], sensibleHighHypo[year], latentHighHypo[year], hyporheicHighHypo[year])
#####


### COLOR SHWIZZ ###
mycolorpal <- hcl.colors(4, "Inferno")
fluxpal <- hcl.colors(5, "Plasma")
refcol <- mycolorpal[1]

shade30gray <- mycolorpal[2]
shade60gray <- mycolorpal[3]
shade90gray <- mycolorpal[4]

littlehypogray <- mycolorpal[2]
medhypogray <- mycolorpal[3]
highhypogray <- mycolorpal[4]

highcollabel <- "yellow3"

colmatrix <-data.frame(c = fluxpal, a = c(0.7, 0.65, 0.6, 0.55, 0.5))
easyadjust <- function(x){
  return(adjustcolor(x[,1], alpha.f = x[,2]))
}
for (i in 1:5){
  colmatrix$adjusted[i] <- easyadjust(colmatrix[i,])
}


# shade30gray = "gray37"
# shade60gray = "gray60"
# shade90gray = "gray85"
#
# littlehypogray = "gray37"
# medhypogray = "gray60"
# highhypogray = "gray85"
#
# refcol = "black"

yrandhalf <- "2016-01-01/2017-07-01"
lwdparam <- 2
swrange <-  c(0,0.9) #c(-0.35, 0.9)
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



###############################
##### Only partial fluxes #####
###############################
#### Controls ####
png("plots/2017_umatilla/annualEnergyFluxes_color_budget.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(5,2),
    mar = c(2,5,1,1),
    cex.lab = 1.3,
    cex.axis = 1.2,
    oma = c(0,0,0,0))

#### SW SHADE SCENARIOS ####
plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
         ylab = expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet30[yrandhalf], max)), lwd = 2, col = shade30gray)

lines(as.zoo(shortwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet60[yrandhalf], max)), lwd = 2, col = shade60gray)

lines(as.zoo(shortwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet90[yrandhalf], max)), lwd = 2, col = shade90gray)
abline(h= 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.02, labels = paste0(round(refbudget$sw*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.12, labels = paste0(round(shade30budget$sw*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.22, labels = paste0(round(shade60budget$sw*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.32, labels = paste0(round(shade90budget$sw*100, 0), "%"),
     col = highcollabel)

#### LW SHADE SCENARIOS ####
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
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.002, labels = paste0(round(refbudget$lw*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.012, labels = paste0(round(shade30budget$lw*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.022, labels = paste0(round(shade60budget$lw*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.032, labels = paste0(round(shade90budget$lw*100, 0), "%"),
     col = highcollabel)


#### SENSIBLE SHADE SCENARIOS ####
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
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.002, labels = paste0(round(refbudget$sensible*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.015, labels = paste0(round(shade30budget$sensible*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.030, labels = paste0(round(shade60budget$sensible*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.045, labels = paste0(round(shade90budget$sensible*100, 0), "%"),
     col = highcollabel)

#### LATENT SHADE SCENARIOS ####
plot.zoo(latentRef[yrandhalf], col = refcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(latent30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(latent30[yrandhalf], max)), lwd = 2, col = shade30gray)
lines(as.zoo(apply.daily(latent30[yrandhalf], min)), lwd = 2, col = shade30gray)

lines(as.zoo(latent60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent60[yrandhalf], max)), lwd = 2, col = shade60gray)
lines(as.zoo(apply.daily(latent60[yrandhalf], min)), lwd = 2, col = shade60gray)

lines(as.zoo(latent90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(latent90[yrandhalf], max)), lwd = 2, col = shade90gray)
lines(as.zoo(apply.daily(latent90[yrandhalf], min)), lwd = 2, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.1, labels = paste0(round(refbudget$latent*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.13, labels = paste0(round(shade30budget$latent*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.16, labels = paste0(round(shade60budget$latent*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.19, labels = paste0(round(shade90budget$latent*100, 0), "%"),
     col = highcollabel)

#### STREAMBED FLUX: SHADE SCENARIOS ####
plot.zoo(sensible30[yrandhalf], col = shade30gray, ylim = hyporange,
         type = "n",
         xaxt = "n",
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(hyporange)-0.02, labels = paste0(round(refbudget$hyporheic*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(hyporange)-0.07, labels = paste0(round(shade30budget$hyporheic*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(hyporange)-0.12, labels = paste0(round(shade60budget$hyporheic*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(hyporange)-0.17, labels = paste0(round(shade90budget$hyporheic*100, 0), "%"),
     col = highcollabel)


#### SW: HYPORHEIC SCENARIOS ####
plot.zoo(shortwaveNetRef[yrandhalf], col = refcol, lwd = lwdparam, ylim = swrange,
         ylab =  expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)

lines(as.zoo(shortwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)

lines(as.zoo(shortwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.02, labels = paste0(round(refbudget$sw*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.12, labels = paste0(round(hypolittlebudget$sw*100, 0), "%"),
     col = littlehypogray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.22, labels = paste0(round(hypomedbudget$sw*100, 0), "%"),
     col = medhypogray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(swrange)-0.32, labels = paste0(round(hypohighbudget$sw*100, 0), "%"),
     col = highcollabel)

#### LW: HYPORHEIC SCENARIOS ####
plot.zoo(longwaveNetRef[yrandhalf], col = refcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(longwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(longwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.002, labels = paste0(round(refbudget$lw*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.012, labels = paste0(round(hypolittlebudget$lw*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.022, labels = paste0(round(hypomedbudget$lw*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(lwrange)-0.032, labels = paste0(round(hypohighbudget$lw*100, 0), "%"),
     col = highcollabel)

#### SENSIBLE: HYPORHEIC SENARIOS ####
plot.zoo(sensibleRef[yrandhalf], col = refcol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(sensibleLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(sensibleMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(sensibleHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.002, labels = paste0(round(refbudget$sensible*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.015, labels = paste0(round(hypolittlebudget$sensible*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.030, labels = paste0(round(hypomedbudget$sensible*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(sensrange)-0.045, labels = paste0(round(hypohighbudget$sensible*100, 0), "%"),
     col = highcollabel)

#### LATENT: HYPORHEIC SCENARIOS ####
plot.zoo(latentRef[yrandhalf], col = refcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")

lines(as.zoo(latentLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(latentMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)

lines(as.zoo(latentHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.1, labels = paste0(round(refbudget$latent*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.13, labels = paste0(round(shade30budget$latent*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.16, labels = paste0(round(shade60budget$latent*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = max(latrange)-0.19, labels = paste0(round(shade90budget$latent*100, 0), "%"),
     col = highcollabel)

#### HYPORHEIC: HYPORHEIC SCENARIOS ####
plot.zoo(hyporheicLittleHypo[yrandhalf], col = adjustcolor(littlehypogray, alpha.f = 0.7), lwd = lwdparam, ylim = hyporange,
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], max)), lwd = 2, col = littlehypogray)
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], min)), lwd = 2, col = littlehypogray)

lines(as.zoo(hyporheicMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], max)), lwd = 2, col = medhypogray)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], min)), lwd = 2, col = medhypogray)


lines(as.zoo(hyporheicHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], max)), lwd = 2, col = highhypogray)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], min)), lwd = 2, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)
text(x = ymd_hms("2016-01-25 00:00:00"), y = 0-0.05, labels = paste0(round(refbudget$hyporheic*100, 0), "%"),
     col = refcol)
text(x = ymd_hms("2016-01-25 00:00:00"), y = 0-0.10, labels = paste0(round(hypolittlebudget$hyporheic*100, 0), "%"),
     col = shade30gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = 0-0.15, labels = paste0(round(hypomedbudget$hyporheic*100, 0), "%"),
     col = shade60gray)
text(x = ymd_hms("2016-01-25 00:00:00"), y = 0-0.2, labels = paste0(round(hypohighbudget$hyporheic*100, 0), "%"),
     col = highcollabel)


dev.off()
#####

###################################
##### ANNUAL BUDGET BAR CHART #####
png("plots/2017_umatilla/annualBudgetBars_v2_new.png", width = 1000*5, height = 400*5,
    res = 72*5)
par(mfrow = c(1,2),
    mar = c(3,3,1,1))
barplot(matrix(c(shade90budget,
               shade60budget,
               shade30budget,
               refbudget), nrow = 5, ncol = 4),
        beside = F,
        names.arg = c("High", "Moderate", "Low", "Control"),
        horiz = T)
barplot(matrix(c(hypohighbudget,
                 hypomedbudget,
                 hypolittlebudget,
                 refbudget), nrow = 5, ncol = 4),
        beside = F,
        names.arg = c("High", "Moderate", "Low", "Control"),
        horiz = T)
dev.off()

##########################################
##### ANNUAL BUDGET BAR CHART:ONE GRAPH #####

png("plots/2017_umatilla/annualBudgetBars_oneGraph_color_v4.png", width = 400*5, height = 700*5,
    res = 72*5)
par(mfrow = c(1,1),
    mar = c(3,3,1,1))
barplot(matrix(c(shade90budget,
                 shade60budget,
                 shade30budget,
                 refbudget,
                 hypolittlebudget,
                 hypomedbudget,
                 hypohighbudget), nrow = 5, ncol = 7),
        beside = F,
        names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted)

dev.off()











####################################
##### ANNUAL ABSOLUTE VALUE BARS #####
png("plots/2017_umatilla/annualAnnualAbsoluteFluxBars_color_v4.png", width = 400*5, height = 700*5,
    res = 72*5)
par(mfrow = c(1,1),
    mar = c(3,3,1,1))
barplot(matrix(c(shade90abs*31499500*0.001,
                 shade60abs*31499500*0.001,
                 shade30abs*31499500*0.001,
                 refabs*31499500*0.001,
                 hypolittleabs*31499500*0.001,
                 hypomedabs*31499500*0.001,
                 hypohighabs*31499500*0.001), nrow = 5, ncol = 7),
        beside = F,
        names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted)

dev.off()

##########################
##### ANNUAL PIE CHARTS ####
pie_colors <- hcl.colors(5, "Dark 3")
pie_labels <- c("Shortwave", "Longwave", "Sensible", "Latent", "Hyporheic")
label_cex <- 2
png("plots/2017_umatilla/annualBudgetPies_color.png", width = 800*5, height = 1000*5,
    res = 72*5)
lay <- layout(matrix(c(1,1,2,3,4,5,6,7), nrow = 4, ncol = 2, byrow = T))
par(mar = c(0,0,0,0),
    cex.lab = 3)
with(refbudget, pie(c(sw, lw, sensible, latent, hyporheic),
                    labels = pie_labels,
                    col = pie_colors,
                    cex = label_cex))
with(shade30budget, pie(c(sw, lw, sensible, latent, hyporheic),
                    labels = pie_labels,
                    col = pie_colors,
                    cex = label_cex))
with(hypolittlebudget, pie(c(sw, lw, sensible, latent, hyporheic),
                           labels = pie_labels,
                           col = pie_colors,
                           cex = label_cex))
with(shade60budget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        col = pie_colors,
                        cex = label_cex))
with(hypomedbudget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        col = pie_colors,
                        cex = label_cex))
with(shade90budget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        col = pie_colors,
                        cex = label_cex))
with(hypohighbudget, pie(c(sw, lw, sensible, latent, hyporheic),
                           labels = pie_labels,
                           col = pie_colors,
                         cex = label_cex))
dev.off()

png("plots/2017_umatilla/annualBudgetPies_bw.png", width = 800*5, height = 1000*5,
    res = 72*5)
lay <- layout(matrix(c(1,1,2,3,4,5,6,7), nrow = 4, ncol = 2, byrow = T))
par(mar = c(0,0,0,0),
    cex.lab = 3)
with(refbudget, pie(c(sw, lw, sensible, latent, hyporheic),
                    labels = pie_labels,
                    density = seq(0,40, length.out = 5),
                    cex = label_cex))
with(shade30budget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        density = seq(0,40, length.out = 5),
                        cex = label_cex))
with(hypolittlebudget, pie(c(sw, lw, sensible, latent, hyporheic),
                           labels = pie_labels,
                           density = seq(0,40, length.out = 5),
                           cex = label_cex))
with(shade60budget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        density = seq(0,40, length.out = 5),
                        cex = label_cex))
with(hypomedbudget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        density = seq(0,40, length.out = 5),
                        cex = label_cex))
with(shade90budget, pie(c(sw, lw, sensible, latent, hyporheic),
                        labels = pie_labels,
                        density = seq(0,40, length.out = 5),
                        cex = label_cex))
with(hypohighbudget, pie(c(sw, lw, sensible, latent, hyporheic),
                         labels = pie_labels,
                         density = seq(0,40, length.out = 5),
                         cex = label_cex))
dev.off()

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

###########################################
##### * ANNUAL GAIN/LOSS BUDGET BAR CHART #####
png("plots/2017_umatilla/updates/lossBudget_gainBudget_update_2.png", width = 800*5, height = 700*5,
    res = 72*5)
par(mfrow = c(1,2),
    mar = c(3,3,1,1))
barplot(matrix(c(shade90budget2[2,],
                 shade60budget2[2,],
                 shade30budget2[2,],
                 refbudget2[2,],
                 hypolittlebudget2[2,],
                 hypomedbudget2[2,],
                 hypohighbudget2[2,]), nrow = 5, ncol = 7),
        beside = F,
        names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted,
        main = "Loss")

barplot(matrix(c(shade90budget2[1,],
                 shade60budget2[1,],
                 shade30budget2[1,],
                 refbudget2[1,],
                 hypolittlebudget2[1,],
                 hypomedbudget2[1,],
                 hypohighbudget2[1,]), nrow = 5, ncol = 7),
        beside = F,
        names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted,
        main = "Gain")
dev.off()

############################################################
##### * ANNUAL GAIN/LOSS BUDGET BAR CHART as same budget #####
png("plots/2017_umatilla/updates/lossANDgain_budget_v4_update_2.png", width = 800*5, height = 700*5,
    res = 72*5)
par(mfrow = c(1,2),
    mar = c(3,4,1,1),
    cex.axis = 1.5)
barplot(matrix(c(shade90budget3[2,]*-1,
                 shade60budget3[2,]*-1,
                 shade30budget3[2,]*-1,
                 refbudget3[2,]*-1,
                 hypolittlebudget3[2,]*-1,
                 hypomedbudget3[2,]*-1,
                 hypohighbudget3[2,]*-1), nrow = 5, ncol = 7),
        beside = F,
        #names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted,
        #main = "Loss",
        xlim = c(-0.5,0),
        xaxt = "n")
axis(1, at = c(seq(-0.5, 0, by = 0.1)), labels = c(seq(0.5, 0.0, by = -0.1)[1:4], "0.1","0.0"))

barplot(matrix(c(shade90budget3[1,],
                 shade60budget3[1,],
                 shade30budget3[1,],
                 refbudget3[1,],
                 hypolittlebudget3[1,],
                 hypomedbudget3[1,],
                 hypohighbudget3[1,]), nrow = 5, ncol = 7),
        beside = F,
        #names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted,
        #main = "Gain",
        xlim = c(0,0.5))
dev.off()

###########################################
##### ANNUAL GAIN/LOSS FLUX BAR CHART #####
png("plots/2017_umatilla/updates/annualFluxLoss_annualFluxGain_v2_update_2.png", width = 800*5, height = 700*5,
    res = 72*5)
par(mfrow = c(1,2),
    mar = c(3,4,1,1),
    cex.axis = 1.5)
barplot(matrix(c(shade90annflux[2,]/1000,
                 shade60annflux[2,]/1000,
                 shade30annflux[2,]/1000,
                 refannflux[2,]/1000,
                 hypolittleannflux[2,]/1000,
                 hypomedannflux[2,]/1000,
                 hypohighannflux[2,]/1000), nrow = 5, ncol = 7),
        beside = F,
        #names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted,
        #main = "Loss",
        xlim = c(-25,0)
        )
barplot(matrix(c(shade90annflux[1,]/1000,
                 shade60annflux[1,]/1000,
                 shade30annflux[1,]/1000,
                 refannflux[1,]/1000,
                 hypolittleannflux[1,]/1000,
                 hypomedannflux[1,]/1000,
                 hypohighannflux[1,]/1000), nrow = 5, ncol = 7),
        beside = F,
        #names.arg = c("High", "Moderate", "Low", "Control", "Low", "Moderate", "High"),
        horiz = T,
        col = colmatrix$adjusted,
        #main = "Gain",
        xlim = c(0,25))
dev.off()
#####


#################################
#### FLUX PLOTS PER SCENARIO ####
#################################
##### plot function #####
plotscenariofluxes <- function(sw, lw, sens, latent, hypo = NA, fluxcolors, alphanum, borderlwd, middlelwd, ...){
  plot.zoo(sw[yrandhalf],
           col = adjustcolor(fluxcolors[1], alpha.f = alphanum),
           lwd = middlelwd,
           xaxt = "n",
           ylab = "Flux",
           xlab = "Month",
           ...)
  lines(as.zoo(apply.daily(sw[yrandhalf], max)),
        col = fluxcolors[1],
        lwd = borderlwd)

  lines(as.zoo(lw[yrandhalf]),
        col = adjustcolor(fluxcolors[2], alpha.f = alphanum - 0.05),
        lwd = middlelwd)
  lines(as.zoo(apply.daily(lw[yrandhalf], max)),
        col = fluxcolors[2],
        lwd = borderlwd)
  lines(as.zoo(apply.daily(lw[yrandhalf], min)),
        col = fluxcolors[2],
        lwd = borderlwd)


  lines(as.zoo(latent[yrandhalf]),
        col = adjustcolor(fluxcolors[4], alpha.f = alphanum - 0.1),
        lwd = middlelwd)
  lines(as.zoo(apply.daily(latent[yrandhalf], max)),
        col = fluxcolors[4],
        lwd = borderlwd)
  lines(as.zoo(apply.daily(latent[yrandhalf], min)),
        col = fluxcolors[4],
        lwd = borderlwd)

  lines(as.zoo(sens[yrandhalf]),
        col = adjustcolor(fluxcolors[3], alpha.f = alphanum - 0.15),
        lwd = middlelwd)
  lines(as.zoo(apply.daily(sens[yrandhalf], max)),
        col = fluxcolors[3],
        lwd = borderlwd)
  lines(as.zoo(apply.daily(sens[yrandhalf], min)),
        col = fluxcolors[3],
        lwd = borderlwd)



  if(is.na(hypo) == F){
    lines(as.zoo(hypo[yrandhalf]),
          col = adjustcolor(fluxcolors[5], alpha.f = alphanum - 0.2),
          lwd = middlelwd)
    lines(as.zoo(apply.daily(hypo[yrandhalf], max)),
          col = fluxcolors[5],
          lwd = borderlwd)
    lines(as.zoo(apply.daily(hypo[yrandhalf], min)),
          col = fluxcolors[5],
          lwd = borderlwd)

    abline(h = 0, col = "white", lwd = 3)

    all <- sw + lw + sens + latent + hypo
    lines(as.zoo(apply.daily(all, mean)),
          lty = 2,
          lwd = 2)
  }else{

    abline(h = 0, col = "white", lwd = 3)

    all <- sw + lw + sens + latent
    lines(as.zoo(apply.daily(all, mean)),
          lty = 2,
          lwd = 2)
  }

  xaxislabels <- c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan", "Mar", "May", "Jul")
  xaxisat <- c(ymd_hms("2016-01-01 00:00:00"), ymd_hms("2016-03-01 00:00:00"),
               ymd_hms("2016-05-01 00:00:00"), ymd_hms("2016-07-01 00:00:00"),
               ymd_hms("2016-09-01 00:00:00"), ymd_hms("2016-11-01 00:00:00"),
               ymd_hms("2017-01-01 00:00:00"), ymd_hms("2017-03-01 00:00:00"),
               ymd_hms("2017-05-01 00:00:00"), ymd_hms("2017-07-01 00:00:00"))
  axis(1, at = xaxisat, labels = xaxislabels)
}

plotmeanscenariofluxes <- function(sw, lw, sens, latent, hypo = NA, fluxcolors, alphanum, borderlwd, middlelwd, ...){
  plot.zoo(apply.daily(sw[yrandhalf], mean),
           col = adjustcolor(fluxcolors[1], alpha.f = alphanum),
           lwd = middlelwd,
           xaxt = "n",
           ylab = "Flux",
           xlab = "Month",
           yaxt = "n",
           ...)

  lines(as.zoo(apply.daily(lw[yrandhalf], mean)),
        col = adjustcolor(fluxcolors[2], alpha.f = alphanum),
        lwd = middlelwd)

  lines(as.zoo(apply.daily(latent[yrandhalf], mean)),
        col = adjustcolor(fluxcolors[4], alpha.f = alphanum),
        lwd = middlelwd)

  lines(as.zoo(apply.daily(sens[yrandhalf], mean)),
        col = adjustcolor(fluxcolors[3], alpha.f = alphanum),
        lwd = middlelwd)

  if(is.na(hypo) == F){
    lines(as.zoo(apply.daily(hypo[yrandhalf], mean)),
          col = "black",
          lwd = middlelwd+2)
    lines(as.zoo(apply.daily(hypo[yrandhalf], mean)),
          col = adjustcolor(fluxcolors[5], alpha.f = alphanum),
          lwd = middlelwd)

    # abline(h = 0, col = "white", lwd = 3)

    abline(h = 0, lwd = 2, lty = 2)

    # all <- sw + lw + sens + latent + hypo
    # lines(as.zoo(apply.daily(all, mean)),
    #       lty = 2,
    #       lwd = 2)
  }else{

    # abline(h = 0, col = "white", lwd = 3)

    abline(h = 0, lwd = 2, lty = 2)

    # all <- sw + lw + sens + latent
    # lines(as.zoo(apply.daily(all, mean)),
    #       lty = 2,
    #       lwd = 2)
  }

  xaxislabels <- c("01", "03", "05", "07", "09", "11", "01", "03", "05", "07")
  xaxisat <- c(ymd_hms("2016-01-01 00:00:00"), ymd_hms("2016-03-01 00:00:00"),
               ymd_hms("2016-05-01 00:00:00"), ymd_hms("2016-07-01 00:00:00"),
               ymd_hms("2016-09-01 00:00:00"), ymd_hms("2016-11-01 00:00:00"),
               ymd_hms("2017-01-01 00:00:00"), ymd_hms("2017-03-01 00:00:00"),
               ymd_hms("2017-05-01 00:00:00"), ymd_hms("2017-07-01 00:00:00"))
  axis(1, at = xaxisat, labels = xaxislabels)
  axis(2, at = c(-0.2, 0.1, 0.0, 0.1, 0.2, 0.3),
       labels = c("-0.2", "", "0.0", "", "0.2", ""))
}

plotdailysumfluxes <- function(sw, lw, sens, latent, hypo = NA, fluxcolors, alphanum, borderlwd, middlelwd, ...){
  plot.zoo(apply.daily(sw[yrandhalf]*10, sum),
           col = adjustcolor(fluxcolors[1], alpha.f = alphanum),
           lwd = middlelwd,
           xaxt = "n",
           ylab = "Flux",
           xlab = "Month",
           # yaxt = "n",
           ...)

  lines(as.zoo(apply.daily(lw[yrandhalf]*10, sum)),
        col = adjustcolor(fluxcolors[2], alpha.f = alphanum),
        lwd = middlelwd)

  lines(as.zoo(apply.daily(latent[yrandhalf]*10, sum)),
        col = adjustcolor(fluxcolors[4], alpha.f = alphanum),
        lwd = middlelwd)

  lines(as.zoo(apply.daily(sens[yrandhalf]*10, sum)),
        col = adjustcolor(fluxcolors[3], alpha.f = alphanum),
        lwd = middlelwd)

  if(is.na(hypo) == F){
    lines(as.zoo(apply.daily(hypo[yrandhalf]*10, sum)),
          col = "black",
          lwd = middlelwd+2)
    lines(as.zoo(apply.daily(hypo[yrandhalf]*10, sum)),
          col = adjustcolor(fluxcolors[5], alpha.f = alphanum),
          lwd = middlelwd)
    abline(h = 0, lwd = 2, lty = 2)

  }else{

    abline(h = 0, lwd = 2, lty = 2)

  }

  xaxislabels <- c("01", "03", "05", "07", "09", "11", "01", "03", "05", "07")
  xaxisat <- c(ymd_hms("2016-01-01 00:00:00"), ymd_hms("2016-03-01 00:00:00"),
               ymd_hms("2016-05-01 00:00:00"), ymd_hms("2016-07-01 00:00:00"),
               ymd_hms("2016-09-01 00:00:00"), ymd_hms("2016-11-01 00:00:00"),
               ymd_hms("2017-01-01 00:00:00"), ymd_hms("2017-03-01 00:00:00"),
               ymd_hms("2017-05-01 00:00:00"), ymd_hms("2017-07-01 00:00:00"))
  axis(1, at = xaxisat, labels = xaxislabels)
  # axis(2, at = c(-0.2, 0.1, 0.0, 0.1, 0.2, 0.3),
  #      labels = c("-0.2", "", "0.0", "", "0.2", ""))
}


#### controls ####
fluxpal <- hcl.colors(5, "Plasma")
alphaparam <- 1
lwdparam <- 5

##### * PER SCENARIO MEAN FLUX PLOTS ####
png("plots/2017_umatilla/updates/perScenarioMeanFluxPlots_update.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(4,2),
    cex.lab = 1.5,
    cex.axis = 2,
    mar = c(2,3,1,1))
##### REFERENCE/CONTROL #####
plotmeanscenariofluxes(sw = shortwaveNetRef,
                   lw = longwaveNetRef,
                   sens = sensibleRef,
                   latent = latentRef,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))

##### SHADE 30 ####
plotmeanscenariofluxes(sw = shortwaveNet30,
                   lw = longwaveNet30,
                   sens = sensible30,
                   latent = latent30,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))

##### SHADE 60 ####
plotmeanscenariofluxes(sw = shortwaveNet60,
                   lw = longwaveNet60,
                   sens = sensible60,
                   latent = latent60,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))

##### SHADE 90 ####
plotmeanscenariofluxes(sw = shortwaveNet90,
                   lw = longwaveNet90,
                   sens = sensible90,
                   latent = latent90,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))

##### REFERENCE/CONTROL #####
plotmeanscenariofluxes(sw = shortwaveNetRef,
                   lw = longwaveNetRef,
                   sens = sensibleRef,
                   latent = latentRef,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))

##### HYPO LOW ####
plotmeanscenariofluxes(sw = shortwaveLittleHypo,
                   lw = longwaveLittleHypo,
                   sens = sensibleLittleHypo,
                   latent = latentLittleHypo,
                   hypo = hyporheicLittleHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))


##### HYPO MED ####
plotmeanscenariofluxes(sw = shortwaveMedHypo,
                   lw = longwaveMedHypo,
                   sens = sensibleMedHypo,
                   latent = latentMedHypo,
                   hypo = hyporheicMedHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))


##### HYPO HIGH ####
plotmeanscenariofluxes(sw = shortwaveHighHypo,
                   lw = longwaveHighHypo,
                   sens = sensibleHighHypo,
                   latent = latentHighHypo,
                   hypo = hyporheicHighHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.25, 0.35))
dev.off()



##############

#### PER SCENARIO ANNUAL DAILY SUM PLOTS ####
png("plots/2017_umatilla/updates/dailyCumulative_annualHeat.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(4,2),
    cex.lab = 1.5,
    cex.axis = 2,
    mar = c(2,3,1,1))
##### REFERENCE/CONTROL #####
plotdailysumfluxes(sw = shortwaveNetRef,
                       lw = longwaveNetRef,
                       sens = sensibleRef,
                       latent = latentRef,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                       ylim = c(-80, 80)
                   )

##### SHADE 30 ####
plotdailysumfluxes(sw = shortwaveNet30,
                       lw = longwaveNet30,
                       sens = sensible30,
                       latent = latent30,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                   )

##### SHADE 60 ####
plotdailysumfluxes(sw = shortwaveNet60,
                       lw = longwaveNet60,
                       sens = sensible60,
                       latent = latent60,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                   )

##### SHADE 90 ####
plotdailysumfluxes(sw = shortwaveNet90,
                       lw = longwaveNet90,
                       sens = sensible90,
                       latent = latent90,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                   )

##### REFERENCE/CONTROL #####
plotdailysumfluxes(sw = shortwaveNetRef,
                       lw = longwaveNetRef,
                       sens = sensibleRef,
                       latent = latentRef,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                   )

##### HYPO LOW ####
plotdailysumfluxes(sw = shortwaveLittleHypo,
                       lw = longwaveLittleHypo,
                       sens = sensibleLittleHypo,
                       latent = latentLittleHypo,
                       hypo = hyporheicLittleHypo,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                   )


##### HYPO MED ####
plotdailysumfluxes(sw = shortwaveMedHypo,
                       lw = longwaveMedHypo,
                       sens = sensibleMedHypo,
                       latent = latentMedHypo,
                       hypo = hyporheicMedHypo,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                   )


##### HYPO HIGH ####
plotdailysumfluxes(sw = shortwaveHighHypo,
                       lw = longwaveHighHypo,
                       sens = sensibleHighHypo,
                       latent = latentHighHypo,
                       hypo = hyporheicHighHypo,
                       fluxcolors = fluxpal,
                       alphanum = alphaparam,
                       borderlwd = lwdparam,
                       middlelwd = lwdparam,
                   ylim = c(-80, 80)
                       )
dev.off()



##############



################################################
###### FLUX PLOTS PER SCENARIO W/ NET FLUX #####
################################################
#### controls ####
fluxpal <- hcl.colors(5, "Plasma")
alphaparam <- 0.7
shade30gray = "gray37"
shade60gray = "gray60"
shade90gray = "gray85"

littlehypogray = "gray37"
medhypogray = "gray60"
highhypogray = "gray85"

refcol = "black"

png("plots/2017_umatilla/perScenarioFluxPlots_withNetFlux.png",
    width = 1100*5, height = 700*5,
    res = 72*5)
par(mfcol = c(4,4),
    cex.lab = 1.3,
    mar = c(2,3,1,1))
#### REF/CONTROL NET  ####
plot.zoo(refall[yrandhalf], col = refcol, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)
#### SHADE 30 NET ####
plot.zoo(shade30all[yrandhalf], col = shade30gray, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)
#### SHADE 60 NET ####
plot.zoo(shade60all[yrandhalf], col = shade60gray, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)
#### SHADE 90 NET ####
plot.zoo(shade90all[yrandhalf], col = shade90gray, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)

##### REFERENCE/CONTROL #####
plotscenariofluxes(sw = shortwaveNetRef,
                   lw = longwaveNetRef,
                   sens = sensibleRef,
                   latent = latentRef,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### SHADE 30 ####
plotscenariofluxes(sw = shortwaveNet30,
                   lw = longwaveNet30,
                   sens = sensible30,
                   latent = latent30,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### SHADE 60 ####
plotscenariofluxes(sw = shortwaveNet60,
                   lw = longwaveNet60,
                   sens = sensible60,
                   latent = latent60,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### SHADE 90 ####
plotscenariofluxes(sw = shortwaveNet90,
                   lw = longwaveNet90,
                   sens = sensible90,
                   latent = latent90,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### REFERENCE/CONTROL #####
plotscenariofluxes(sw = shortwaveNetRef,
                   lw = longwaveNetRef,
                   sens = sensibleRef,
                   latent = latentRef,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### HYPO LOW ####
plotscenariofluxes(sw = shortwaveLittleHypo,
                   lw = longwaveLittleHypo,
                   sens = sensibleLittleHypo,
                   latent = latentLittleHypo,
                   hypo = hyporheicLittleHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))


##### HYPO MED ####
plotscenariofluxes(sw = shortwaveMedHypo,
                   lw = longwaveMedHypo,
                   sens = sensibleMedHypo,
                   latent = latentMedHypo,
                   hypo = hyporheicMedHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))


##### HYPO HIGH ####
plotscenariofluxes(sw = shortwaveHighHypo,
                   lw = longwaveHighHypo,
                   sens = sensibleHighHypo,
                   latent = latentHighHypo,
                   hypo = hyporheicHighHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

#### REF/CONTROL NET  ####
plot.zoo(refall[yrandhalf], col = refcol, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)
#### Little Hypo NET ####
plot.zoo(hypolittleall[yrandhalf], col = littlehypogray, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)
#### SHADE 60 NET ####
plot.zoo(hypomedall[yrandhalf], col = medhypogray, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)
#### SHADE 90 NET ####
plot.zoo(hypohighall[yrandhalf], col = highhypogray, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
axis(1, at = xaxisat, labels = xaxislabels)

dev.off()



##############

############################################################
###### FLUX PLOTS PER SCENARIO W/ NET FLUX ON ONE PLOT #####
############################################################
#### controls ####
fluxpal <- hcl.colors(5, "Plasma")
alphaparam <- 0.7
shade30gray = hcl.colors(6, "Vik")[6]
shade60gray = hcl.colors(6, "Vik")[5]
shade90gray = hcl.colors(6, "Vik")[4]

littlehypogray = hcl.colors(6, "Vik")[1]
medhypogray = hcl.colors(6, "Vik")[2]
highhypogray = hcl.colors(6, "Vik")[3]

refcol = "black"

png("plots/2017_umatilla/perScenarioFluxPlots_withNetFluxSameGraph.png",
    width = 1100*5, height = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    cex.lab = 1.3,
    mar = c(2,3,1,1))

##### REFERENCE/CONTROL #####
plotscenariofluxes(sw = shortwaveNetRef,
                   lw = longwaveNetRef,
                   sens = sensibleRef,
                   latent = latentRef,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### SHADE 30 ####
plotscenariofluxes(sw = shortwaveNet30,
                   lw = longwaveNet30,
                   sens = sensible30,
                   latent = latent30,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### SHADE 60 ####
plotscenariofluxes(sw = shortwaveNet60,
                   lw = longwaveNet60,
                   sens = sensible60,
                   latent = latent60,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### SHADE 90 ####
plotscenariofluxes(sw = shortwaveNet90,
                   lw = longwaveNet90,
                   sens = sensible90,
                   latent = latent90,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### REFERENCE/CONTROL #####
plotscenariofluxes(sw = shortwaveNetRef,
                   lw = longwaveNetRef,
                   sens = sensibleRef,
                   latent = latentRef,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

##### HYPO LOW ####
plotscenariofluxes(sw = shortwaveLittleHypo,
                   lw = longwaveLittleHypo,
                   sens = sensibleLittleHypo,
                   latent = latentLittleHypo,
                   hypo = hyporheicLittleHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))


##### HYPO MED ####
plotscenariofluxes(sw = shortwaveMedHypo,
                   lw = longwaveMedHypo,
                   sens = sensibleMedHypo,
                   latent = latentMedHypo,
                   hypo = hyporheicMedHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))


##### HYPO HIGH ####
plotscenariofluxes(sw = shortwaveHighHypo,
                   lw = longwaveHighHypo,
                   sens = sensibleHighHypo,
                   latent = latentHighHypo,
                   hypo = hyporheicHighHypo,
                   fluxcolors = fluxpal,
                   alphanum = alphaparam,
                   borderlwd = lwdparam,
                   middlelwd = lwdparam,
                   ylim = c(-0.35, 0.9))

#### REF/CONTROL NET ####
plot.zoo(refall[yrandhalf], col = refcol, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
abline(h = 0, col = "white", lwd = 3)
axis(1, at = xaxisat, labels = xaxislabels)
#### Little Hypo/Shade 30 NET ####
plot.zoo(hypolittleall[yrandhalf], col = adjustcolor(littlehypogray, alpha.f = 0.5), lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
lines(as.zoo(apply.daily(hypolittleall[yrandhalf], max)),
      col = littlehypogray,
      lwd = 2)
lines(as.zoo(apply.daily(hypolittleall[yrandhalf], min)),
      col = littlehypogray,
      lwd = 2)

lines(as.zoo(shade30all[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.5), lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
lines(as.zoo(apply.daily(shade30all[yrandhalf], max)),
      col = shade30gray,
      lwd = 2)
lines(as.zoo(apply.daily(shade30all[yrandhalf], min)),
      col = shade30gray,
      lwd = 2)

abline(h = 0, col = "white", lwd = 3)
axis(1, at = xaxisat, labels = xaxislabels)
#### Moderate Hypo/Shade 60 NET ####
plot.zoo(hypomedall[yrandhalf], col = adjustcolor(medhypogray, alpha.f = 0.5), lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
lines(as.zoo(apply.daily(hypomedall[yrandhalf], max)),
      col = medhypogray,
      lwd = 2)
lines(as.zoo(apply.daily(hypomedall[yrandhalf], min)),
      col = medhypogray,
      lwd = 2)

lines(as.zoo(shade60all[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.5), lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
lines(as.zoo(apply.daily(shade60all[yrandhalf], max)),
      col = shade60gray,
      lwd = 2)
lines(as.zoo(apply.daily(shade60all[yrandhalf], min)),
      col = shade60gray,
      lwd = 2)
abline(h = 0, col = "white", lwd = 3)
axis(1, at = xaxisat, labels = xaxislabels)

#### High Hypo/Shade 90 NET ####
plot.zoo(hypohighall[yrandhalf], col = adjustcolor(highhypogray, alpha.f = 0.5), lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = refrange,
         xlab = "Month")
lines(as.zoo(apply.daily(hypohighall[yrandhalf], max)),
      col = highhypogray,
      lwd = 2)
lines(as.zoo(apply.daily(hypohighall[yrandhalf], min)),
      col = highhypogray,
      lwd = 2)

lines(as.zoo(shade90all[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.5), lwd = lwdparam,
      ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
      xaxt = "n",
      ylim = refrange,
      xlab = "Month")
lines(as.zoo(apply.daily(shade90all[yrandhalf], max)),
      col = shade90gray,
      lwd = 2)
lines(as.zoo(apply.daily(shade90all[yrandhalf], min)),
      col = shade90gray,
      lwd = 2)
abline(h = 0, col = "white", lwd = 3)
axis(1, at = xaxisat, labels = xaxislabels)

dev.off()






##############

refall <- shortwaveNetRef + longwaveNetRef + sensibleRef + latentRef
shade30all <- shortwaveNet30 + longwaveNet30 + sensible30 + latent30
shade60all <- shortwaveNet60 + longwaveNet60 + sensible60 + latent60
shade90all <- shortwaveNet90 + longwaveNet90 + sensible90 + latent90
hypolittleall <- shortwaveLittleHypo + longwaveLittleHypo + sensibleLittleHypo + latentLittleHypo + hyporheicLittleHypo
hypomedall <- shortwaveMedHypo + longwaveMedHypo + sensibleMedHypo + latentMedHypo + hyporheicMedHypo
hypohighall <- shortwaveHighHypo + longwaveHighHypo + sensibleHighHypo + latentHighHypo + hyporheicHighHypo

refrange <- c(-0.35, 0.9)

shade30all_abs <- abs(shortwaveNet30) + abs(longwaveNet30) + abs(sensible30) + abs(latent30)
swpercent_shade30 <- abs(shortwaveNet30)/shade30all_abs
lwpercent_shade30 <- abs(longwaveNet30)/shade30all_abs
senspercent_shade30 <- abs(sensible30)/shade30all_abs
latentpercent_shade30 <- abs(latent30)/shade30all_abs

plot.zoo(swpercent_shade30[day], ylim = c(0,1))
lines(as.zoo(lwpercent_shade30[day]), col = "red")
lines(as.zoo(senspercent_shade30[yrandhalf]), col = "blue")
lines(as.zoo(latentpercent_shade30[yrandhalf]), col = "forestgreen")

day <- "2016-10-01/2016-10-05"
plot.zoo(swpercent_shade30[day], ylim = c(0,1))
# lines(as.zoo(swpercent_shade30[day] + lwpercent_shade30[day]), col = "red")
# lines(as.zoo(swpercent_shade30[day] + lwpercent_shade30[day] + senspercent_shade30[yrandhalf]), col = "blue")
# lines(as.zoo(swpercent_shade30[day] + lwpercent_shade30[day] + senspercent_shade30[yrandhalf] + latentpercent_shade30[yrandhalf]), col = "forestgreen")

polygon(x = c(index(swpercent_shade30[day]), rev(index(lwpercent_shade30[day]))),
        y = c(coredata(swpercent_shade30[day]),
              rev(coredata(swpercent_shade30[day] + lwpercent_shade30[day]))),
        col = "red")
polygon(x = c(index(swpercent_shade30[day]), rev(index(lwpercent_shade30[day]))),
        y = c(coredata(swpercent_shade30[day] + lwpercent_shade30[day]),
              rev(coredata(swpercent_shade30[day] + lwpercent_shade30[day] + senspercent_shade30[yrandhalf]))),
        col = "blue")
polygon(x = c(index(swpercent_shade30[day]), rev(index(lwpercent_shade30[day]))),
        y = c(coredata(swpercent_shade30[day] + lwpercent_shade30[day] + senspercent_shade30[yrandhalf]),
              rev(coredata(swpercent_shade30[day] + lwpercent_shade30[day]+ senspercent_shade30[yrandhalf] + latentpercent_shade30[yrandhalf]))),
        col = "forestgreen")


year <- "2016"

pie(c(sum(abs(shortwaveNet30[day])), sum(abs(longwaveNet30[day])), sum(abs(sensible30[day])), sum(abs(latent30[day]))), labels = c("sw", "lw", "sens", "latent"))







##### All Qs ######
swrange <-  c(0, 0.875)
lwrange <- c(-0.4375, 0.4375)
sensrange <- c(-0.4375, 0.4375)
latrange <- c(-0.425, 0.4375)
hyporange <- c(-0.4375, 0.4375)
netrange <- c(-0.33, 0.58)

fluxpal <- hcl.colors(5, "Plasma")

netref <- "black"
shadecols <- hcl.colors(6, "Vik", rev = T)[1:3]
hypocols <- hcl.colors(6, "Vik")[1:3]

refcol <- "grey10"

shade30gray <- shadecols[1]
shade60gray <- shadecols[2]
shade90gray <- shadecols[3]

littlehypogray <- hypocols[1]
medhypogray <- hypocols[2]
highhypogray <- hypocols[3]
lwdparam <- 3

# refcol <- "black"
#
# shade30gray <- shadecols[1]
# shade60gray <- shadecols[2]
# shade90gray <- shadecols[3]
#
# littlehypogray <- hypocols[1]
# medhypogray <- hypocols[2]
# highhypogray <- hypocols[3]


png("plots/2017_umatilla/annualEnergyFluxes_allQs_equalylimrange_color_bluebrown.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(6,2),
    mar = c(2,2,1,1),
    cex.lab = 1.3,
    cex.axis = 1.2,
    oma = c(0,0,0,0))


##### SW SHADE SCENARIOS #####
plot.zoo(shortwaveNetRef[yrandhalf],
         col = adjustcolor(refcol, alpha.f = 0.6),
         lwd = lwdparam,
         ylim = swrange,
         ylab = expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(shortwaveNetRef[yrandhalf], max)), lwd = lwdparam, col = refcol)

lines(as.zoo(shortwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet30[yrandhalf], max)), lwd = lwdparam, col = shade30gray)

lines(as.zoo(shortwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet60[yrandhalf], max)), lwd = lwdparam, col = shade60gray)

lines(as.zoo(shortwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet90[yrandhalf], max)), lwd = lwdparam, col = shade90gray)
abline(h= 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LW SHADE SCENARIOS #####
plot.zoo(longwaveNetRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6),
         ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(longwaveNetRef[yrandhalf], max)), lwd = lwdparam, col = refcol)
lines(as.zoo(apply.daily(longwaveNetRef[yrandhalf], min)), lwd = lwdparam, col = refcol)

lines(as.zoo(longwaveNet30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], max)), lwd = lwdparam, col = shade30gray)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], min)), lwd = lwdparam, col = shade30gray)

lines(as.zoo(longwaveNet60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], max)), lwd = lwdparam, col = shade60gray)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], min)), lwd = lwdparam, col = shade60gray)

lines(as.zoo(longwaveNet90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], max)), lwd = lwdparam, col = shade90gray)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], min)), lwd = lwdparam, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


##### SENSIBLE SHADE SCENARIOS #####
plot.zoo(sensibleRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6),
         ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(sensibleRef[yrandhalf], max)), lwd = lwdparam, col = refcol)
lines(as.zoo(apply.daily(sensibleRef[yrandhalf], min)), lwd = lwdparam, col = refcol)

lines(as.zoo(sensible30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.7), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible30[yrandhalf], max)), lwd = lwdparam, col = shade30gray)
lines(as.zoo(apply.daily(sensible30[yrandhalf], min)), lwd = lwdparam, col = shade30gray)

lines(as.zoo(sensible60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible60[yrandhalf], max)), lwd = lwdparam, col = shade60gray)
lines(as.zoo(apply.daily(sensible60[yrandhalf], min)), lwd = lwdparam, col = shade60gray)

lines(as.zoo(sensible90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.4), lwd = lwdparam)
lines(as.zoo(apply.daily(sensible90[yrandhalf], max)), lwd = lwdparam, col = shade90gray)
lines(as.zoo(apply.daily(sensible90[yrandhalf], min)), lwd = lwdparam, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LATENT SHADE SCENARIOS #####
plot.zoo(latentRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6),
         ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(latentRef[yrandhalf], max)), lwd = lwdparam, col = refcol)
lines(as.zoo(apply.daily(latentRef[yrandhalf], min)), lwd = lwdparam, col = refcol)

lines(as.zoo(latent30[yrandhalf]), col = adjustcolor(shade30gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent30[yrandhalf], max)), lwd = lwdparam, col = shade30gray)
lines(as.zoo(apply.daily(latent30[yrandhalf], min)), lwd = lwdparam, col = shade30gray)

lines(as.zoo(latent60[yrandhalf]), col = adjustcolor(shade60gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent60[yrandhalf], max)), lwd = lwdparam, col = shade60gray)
lines(as.zoo(apply.daily(latent60[yrandhalf], min)), lwd = lwdparam, col = shade60gray)

lines(as.zoo(latent90[yrandhalf]), col = adjustcolor(shade90gray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latent90[yrandhalf], max)), lwd = lwdparam, col = shade90gray)
lines(as.zoo(apply.daily(latent90[yrandhalf], min)), lwd = lwdparam, col = shade90gray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### STREAMBED FLUX: SHADE SCENARIOS #####
plot.zoo(sensible30[yrandhalf], col = shade30gray, ylim = hyporange,
         type = "n",
         xaxt = "n",
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


##### Net energy flux Q_c #####
plot.zoo(refall[yrandhalf],
         col = adjustcolor(refcol, alpha.f = 0.6),
         lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = netrange)
lines(apply.daily(as.zoo(refall[yrandhalf]), max), col = adjustcolor(shade30gray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(refall[yrandhalf]), min), col = adjustcolor(shade30gray, alpha.f = 1), lwd = lwdparam)

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

##### SW: HYPORHEIC SCENARIOS #####
plot.zoo(shortwaveNetRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6), lwd = lwdparam, ylim = swrange,
         ylab =  expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(shortwaveNetRef[yrandhalf], max)), lwd = lwdparam, col = refcol)

lines(as.zoo(shortwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveLittleHypo[yrandhalf], max)), lwd = lwdparam, col = littlehypogray)

lines(as.zoo(shortwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveMedHypo[yrandhalf], max)), lwd = lwdparam, col = medhypogray)

lines(as.zoo(shortwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveHighHypo[yrandhalf], max)), lwd = lwdparam, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LW: HYPORHEIC SCENARIOS #####
plot.zoo(longwaveNetRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6), ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(longwaveNetRef[yrandhalf], max)), lwd = lwdparam, col = refcol)
lines(as.zoo(apply.daily(longwaveNetRef[yrandhalf], min)), lwd = lwdparam, col = refcol)

lines(as.zoo(longwaveLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], max)), lwd = lwdparam, col = littlehypogray)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], min)), lwd = lwdparam, col = littlehypogray)

lines(as.zoo(longwaveMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], max)), lwd = lwdparam, col = medhypogray)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], min)), lwd = lwdparam, col = medhypogray)

lines(as.zoo(longwaveHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], max)), lwd = lwdparam, col = highhypogray)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], min)), lwd = lwdparam, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### SENSIBLE: HYPORHEIC SENARIOS #####
plot.zoo(sensibleRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6), ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(sensibleRef[yrandhalf], max)), lwd = lwdparam, col = refcol)
lines(as.zoo(apply.daily(sensibleRef[yrandhalf], min)), lwd = lwdparam, col = refcol)

lines(as.zoo(sensibleLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], max)), lwd = lwdparam, col = littlehypogray)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], min)), lwd = lwdparam, col = littlehypogray)

lines(as.zoo(sensibleMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], max)), lwd = lwdparam, col = medhypogray)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], min)), lwd = lwdparam, col = medhypogray)

lines(as.zoo(sensibleHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], max)), lwd = lwdparam, col = highhypogray)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], min)), lwd = lwdparam, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LATENT: HYPORHEIC SCENARIOS #####
plot.zoo(latentRef[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6), ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(latentRef[yrandhalf], max)), lwd = lwdparam, col = refcol)
lines(as.zoo(apply.daily(latentRef[yrandhalf], min)), lwd = lwdparam, col = refcol)

lines(as.zoo(latentLittleHypo[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], max)), lwd = lwdparam, col = littlehypogray)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], min)), lwd = lwdparam, col = littlehypogray)

lines(as.zoo(latentMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], max)), lwd = lwdparam, col = medhypogray)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], min)), lwd = lwdparam, col = medhypogray)

lines(as.zoo(latentHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], max)), lwd = lwdparam, col = highhypogray)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], min)), lwd = lwdparam, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### HYPORHEIC: HYPORHEIC SCENARIOS #####
plot.zoo(hyporheicLittleHypo[yrandhalf], col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam, ylim = hyporange,
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], max)), lwd = lwdparam, col = littlehypogray)
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], min)), lwd = lwdparam, col = littlehypogray)

lines(as.zoo(hyporheicMedHypo[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], max)), lwd = lwdparam, col = medhypogray)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], min)), lwd = lwdparam, col = medhypogray)

lines(as.zoo(hyporheicHighHypo[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], max)), lwd = lwdparam, col = highhypogray)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], min)), lwd = lwdparam, col = highhypogray)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### Net HEat FLux Q_c Hypo #####
plot.zoo(refall[yrandhalf], col = adjustcolor(refcol, alpha.f = 0.6), lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = netrange)
lines(apply.daily(as.zoo(refall[yrandhalf]), max), col = adjustcolor(refcol, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(refall[yrandhalf]), min), col = adjustcolor(refcol, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypolittleall[yrandhalf]), col = adjustcolor(littlehypogray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypolittleall[yrandhalf]), max), col = adjustcolor(littlehypogray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypolittleall[yrandhalf]), min), col = adjustcolor(littlehypogray, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypomedall[yrandhalf]), col = adjustcolor(medhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypomedall[yrandhalf]), max), col = adjustcolor(medhypogray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypomedall[yrandhalf]), min), col = adjustcolor(medhypogray, alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypohighall[yrandhalf]), col = adjustcolor(highhypogray, alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypohighall[yrandhalf]), max), col = adjustcolor(highhypogray, alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypohighall[yrandhalf]), min), col = adjustcolor(highhypogray, alpha.f = 1), lwd = lwdparam)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


dev.off()
#####

##############################
##### All Qs: color, v2 ######
##############################
swrange <-  c(0, 0.875)
lwrange <- c(-0.4375, 0.4375)
sensrange <- c(-0.4375, 0.4375)
latrange <- c(-0.425, 0.4375)
hyporange <- c(-0.4375, 0.4375)
netrange <- c(-0.33, 0.58)

fluxpal <- hcl.colors(5, "Plasma")
swcol <- fluxpal[1]
lwcol <- fluxpal[2]
senscol <- fluxpal[3]
latcol <- fluxpal[4]
hypocol <- fluxpal[5]

alpha_adjusts <- c(0.8, 0.6, 0.4)
alpha_adjusts2 <- c(1,1,1)

swlo <- adjustcolor(lighten(swcol, amount = 0.2), alpha.f = 0.6)
swmed <- adjustcolor(lighten(swcol, amount = 0.4), alpha.f = 0.6)
swhi <- adjustcolor(lighten(swcol, amount = 0.6), alpha.f = 0.6)

lwlo <- adjustcolor(lighten(lwcol, amount = 0.3), alpha.f = 0.6)
lwmed <- adjustcolor(lighten(lwcol, amount = 0.6), alpha.f = 0.6)
lwhi <- adjustcolor(lighten(lwcol, amount = 0.85), alpha.f = 0.6)

senslo <- adjustcolor(lighten(senscol, amount = 0.3), alpha.f = 0.6)
sensmed <- adjustcolor(lighten(senscol, amount = 0.6), alpha.f = 0.6)
senshi <- adjustcolor(lighten(senscol, amount = 0.85), alpha.f = 0.6)

latlo <- adjustcolor(lighten(latcol, amount = 0.3), alpha.f = 0.6)
latmed <- adjustcolor(lighten(latcol, amount = 0.6), alpha.f = 0.6)
lathi <- adjustcolor(lighten(latcol, amount = 0.85), alpha.f = 0.6)

hypolo <- adjustcolor(lighten(hypocol, amount = 0.3), alpha.f = 0.6)
hypomed <- adjustcolor(lighten(hypocol, amount = 0.6), alpha.f = 0.6)
hypohi <- adjustcolor(lighten(hypocol, amount = 0.85), alpha.f = 0.6)

netref <- "black"
shadecols <- hcl.colors(6, "Vik", rev = T)[1:3]
hypocols <- hcl.colors(6, "Vik")[1:3]


png("plots/2017_umatilla/annualEnergyFluxes_allQs_equalylimrange_color_v2.png", width = 800*5, height = 1100*5,
    res = 72*5)
par(mfcol = c(6,2),
    mar = c(2,5,1,1),
    cex.lab = 1.3,
    cex.axis = 1.2,
    oma = c(0,0,0,0))

##### Net energy flux Q_c #####
plot.zoo(refall[yrandhalf],
         col = netref,
         lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = netrange)
lines(as.zoo(shade30all[yrandhalf]),
      col = adjustcolor(shadecols[1], alpha.f = 0.6),
      lwd = lwdparam)
lines(apply.daily(as.zoo(shade30all[yrandhalf]), max),
      col = adjustcolor(shadecols[1], alpha.f = 1),
      lwd = lwdparam)
lines(apply.daily(as.zoo(shade30all[yrandhalf]), min),
      col = adjustcolor(shadecols[1], alpha.f = 1),
      lwd = lwdparam)

lines(as.zoo(shade60all[yrandhalf]),
      col = adjustcolor(shadecols[2], alpha.f = 0.6),
      lwd = lwdparam)
lines(apply.daily(as.zoo(shade60all[yrandhalf]), max),
      col = adjustcolor(shadecols[2], alpha.f = 1),
      lwd = lwdparam)
lines(apply.daily(as.zoo(shade60all[yrandhalf]), min),
      col = adjustcolor(shadecols[2], alpha.f = 1),
      lwd = lwdparam)

lines(as.zoo(shade90all[yrandhalf]),
      col = adjustcolor(shadecols[3], alpha.f = 0.6),
      lwd = lwdparam)
lines(apply.daily(as.zoo(shade90all[yrandhalf]), max),
      col = adjustcolor(shadecols[3], alpha.f = 1),
      lwd = lwdparam)
lines(apply.daily(as.zoo(shade90all[yrandhalf]), min),
      col = adjustcolor(shadecols[3], alpha.f = 1),
      lwd = lwdparam)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### SW SHADE SCENARIOS #####
plot.zoo(shortwaveNetRef[yrandhalf],
         col = swcol,
         lwd = lwdparam,
         ylim = swrange,
         ylab = expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveNet30[yrandhalf]),
      col = swlo,
      lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet30[yrandhalf], max)),
      lwd = 2,
      col = adjustcolor(swlo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(shortwaveNet60[yrandhalf]),
      col = swmed,
      lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet60[yrandhalf], max)),
      lwd = 2,
      col =adjustcolor(swmed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(shortwaveNet90[yrandhalf]),
      col = swhi, lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveNet90[yrandhalf], max)), lwd = 2,
      col = adjustcolor(swhi, alpha.f = alpha_adjusts[3]))
abline(h= 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LW SHADE SCENARIOS #####
plot.zoo(longwaveNetRef[yrandhalf], col = lwcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveNet30[yrandhalf]),
      col = lwlo, lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], max)), lwd = 2,
       col = adjustcolor(lwlo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(longwaveNet30[yrandhalf], min)), lwd = 2,
      col = adjustcolor(lwlo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(longwaveNet60[yrandhalf]),
      col = lwmed, lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], max)), lwd = 2,
      col = adjustcolor(lwmed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(longwaveNet60[yrandhalf], min)), lwd = 2,
      col = adjustcolor(lwmed, alpha.f = alpha_adjusts[2]))

lines(as.zoo(longwaveNet90[yrandhalf]),
      col = lwhi, lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], max)), lwd = 2,
      col = adjustcolor(lwhi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(longwaveNet90[yrandhalf], min)), lwd = 2,
      col = adjustcolor(lwhi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


##### SENSIBLE SHADE SCENARIOS #####
plot.zoo(sensibleRef[yrandhalf], col = senscol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")

lines(as.zoo(sensible30[yrandhalf]), col = senslo, lwd = lwdparam)
lines(as.zoo(apply.daily(sensible30[yrandhalf], max)), lwd = 2, col = adjustcolor(senslo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(sensible30[yrandhalf], min)), lwd = 2, col = adjustcolor(senslo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(sensible60[yrandhalf]), col = sensmed, lwd = lwdparam)
lines(as.zoo(apply.daily(sensible60[yrandhalf], max)), lwd = 2, col = adjustcolor(sensmed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(sensible60[yrandhalf], min)), lwd = 2, col = adjustcolor(sensmed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(sensible90[yrandhalf]), col = senshi, lwd = lwdparam)
lines(as.zoo(apply.daily(sensible90[yrandhalf], max)), lwd = 2, col = adjustcolor(senshi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(sensible90[yrandhalf], min)), lwd = 2, col = adjustcolor(senshi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LATENT SHADE SCENARIOS #####
plot.zoo(latentRef[yrandhalf], col = latcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(latent30[yrandhalf]), col = latlo, lwd = lwdparam)
lines(as.zoo(apply.daily(latent30[yrandhalf], max)), lwd = 2, col = adjustcolor(latlo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(latent30[yrandhalf], min)), lwd = 2, col = adjustcolor(latlo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(latent60[yrandhalf]), col = latmed, lwd = lwdparam)
lines(as.zoo(apply.daily(latent60[yrandhalf], max)), lwd = 2, col = adjustcolor(latmed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(latent60[yrandhalf], min)), lwd = 2, col = adjustcolor(latmed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(latent90[yrandhalf]), col = lathi, lwd = lwdparam)
lines(as.zoo(apply.daily(latent90[yrandhalf], max)), lwd = 2, col = adjustcolor(lathi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(latent90[yrandhalf], min)), lwd = 2, col = adjustcolor(lathi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### STREAMBED FLUX: SHADE SCENARIOS #####
plot.zoo(sensible30[yrandhalf], col = shade30gray, ylim = hyporange,
         type = "n",
         xaxt = "n",
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


##### Net HEat FLux Q_c Hypo #####
plot.zoo(refall[yrandhalf], col = netref, lwd = lwdparam,
         ylab = expression(paste("Net Heat Flux, ", Q[c]," (kW  ",m^-2, ")")),
         xaxt = "n",
         ylim = netrange)
lines(as.zoo(hypolittleall[yrandhalf]), col = adjustcolor(hypocols[1], alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypolittleall[yrandhalf]), max), col = adjustcolor(hypocols[1], alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypolittleall[yrandhalf]), min), col = adjustcolor(hypocols[1], alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypomedall[yrandhalf]), col = adjustcolor(hypocols[2], alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypomedall[yrandhalf]), max), col = adjustcolor(hypocols[2], alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypomedall[yrandhalf]), min), col = adjustcolor(hypocols[2], alpha.f = 1), lwd = lwdparam)

lines(as.zoo(hypohighall[yrandhalf]), col = adjustcolor(hypocols[3], alpha.f = 0.6), lwd = lwdparam)
lines(apply.daily(as.zoo(hypohighall[yrandhalf]), max), col = adjustcolor(hypocols[3], alpha.f = 1), lwd = lwdparam)
lines(apply.daily(as.zoo(hypohighall[yrandhalf]), min), col = adjustcolor(hypocols[3], alpha.f = 1), lwd = lwdparam)
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)


##### SW: HYPORHEIC SCENARIOS #####
plot.zoo(shortwaveNetRef[yrandhalf], col = swcol, lwd = lwdparam, ylim = swrange,
         ylab =  expression(paste("Shortwave Radiation, ", Q[s]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(shortwaveLittleHypo[yrandhalf]), col = swlo, lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveLittleHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(swlo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(shortwaveMedHypo[yrandhalf]), col = swmed, lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveMedHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(swmed, alpha.f =alpha_adjusts2[2]))

lines(as.zoo(shortwaveHighHypo[yrandhalf]), col = swhi, lwd = lwdparam)
lines(as.zoo(apply.daily(shortwaveHighHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(swhi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LW: HYPORHEIC SCENARIOS #####
plot.zoo(longwaveNetRef[yrandhalf], col = lwcol, ylim = lwrange, lwd = lwdparam,
         ylab = expression(paste("Longwave Radiation, ", Q[l]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(longwaveLittleHypo[yrandhalf]), col = lwlo, lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(lwlo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(longwaveLittleHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(lwlo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(longwaveMedHypo[yrandhalf]), col = lwmed, lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(lwmed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(longwaveMedHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(lwmed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(longwaveHighHypo[yrandhalf]), col = lwhi, lwd = lwdparam)
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(lwhi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(longwaveHighHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(lwhi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### SENSIBLE: HYPORHEIC SENARIOS #####
plot.zoo(sensibleRef[yrandhalf], col = senscol, ylim = sensrange, lwd = lwdparam,
         ylab = expression(paste("Sensible Heat, ", Q[h]," (kW  ",m^-2, ")")),
         xaxt = "n")

lines(as.zoo(sensibleLittleHypo[yrandhalf]), col = senslo, lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(senslo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(sensibleLittleHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(senslo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(sensibleMedHypo[yrandhalf]), col = sensmed, lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(sensmed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(sensibleMedHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(sensmed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(sensibleHighHypo[yrandhalf]), col = senshi, lwd = lwdparam)
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(senshi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(sensibleHighHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(senshi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### LATENT: HYPORHEIC SCENARIOS #####
plot.zoo(latentRef[yrandhalf], col = latcol, ylim = latrange, lwd = lwdparam,
         ylab = expression(paste("Latent Heat, ", Q[e]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(latentLittleHypo[yrandhalf]), col = latlo, lwd = lwdparam)
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(latlo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(latentLittleHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(latlo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(latentMedMypo[yrandhalf]), col = latmed, lwd = lwdparam)
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(latmed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(latentMedHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(latmed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(latentHighHypo[yrandhalf]), col = lathi, lwd = lwdparam)
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(lathi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(latentHighHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(lathi, alpha.f = alpha_adjusts2[3]))
abline(h = 0, lty = 2)
axis(1, at = xaxisat, labels = xaxislabels)

##### HYPORHEIC: HYPORHEIC SCENARIOS #####
plot.zoo(hyporheicLittleHypo[yrandhalf], col = hypolo, lwd = lwdparam, ylim = hyporange,
         ylab = expression(paste("Hyporheic Heat Flux, ", Q[b]," (kW  ",m^-2, ")")),
         xaxt = "n")
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(hypolo, alpha.f = alpha_adjusts2[1]))
lines(as.zoo(apply.daily(hyporheicLittleHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(hypolo, alpha.f = alpha_adjusts2[1]))

lines(as.zoo(hyporheicMedHypo[yrandhalf]), col = hypomed, lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(hypomed, alpha.f = alpha_adjusts2[2]))
lines(as.zoo(apply.daily(hyporheicMedHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(hypomed, alpha.f = alpha_adjusts2[2]))

lines(as.zoo(hyporheicHighHypo[yrandhalf]), col = hypohi, lwd = lwdparam)
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], max)), lwd = 2, col = adjustcolor(hypohi, alpha.f = alpha_adjusts2[3]))
lines(as.zoo(apply.daily(hyporheicHighHypo[yrandhalf], min)), lwd = 2, col = adjustcolor(hypohi, alpha.f = alpha_adjusts2[3]))
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






