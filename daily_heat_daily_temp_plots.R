
#### COMBINED DAILY HEAT FLUXES AND DAILY TEMPERATURE SIGNALS ####

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
load(paste0(file_loc, "netHeats.R"))

plot_daily_heat <- function(sw, lw, sens, latent, hypo = NA, net, colorpal, ylimits){
  plot.zoo(sw,
           ylim = ylimits,
           ylab = expression(paste("Heat Flux (kJ  ", m^-2, " ",s^-1, ")")),
           xlab = "Day of Month",
           xaxt = "n",
           type = "n")
  abline(v = seq(index(sw[1]), index(last(sw)), by = 12*3600),
         col = "darkgray")
  lines(as.zoo(sw),
        col = colorpal[1],
        lwd = linew)
  lines(as.zoo(lw),
        col = colorpal[2],
        lwd = linew)
  lines(as.zoo(sens),
        col = colorpal[3],
        lwd = linew)
  lines(as.zoo(latent),
        col = colorpal[4],
        lwd = linew)
  if(anyNA(hypo) == F){
    lines(as.zoo(hypo),
          col = colorpal[5],
          lwd = linew)
  }
  lines(as.zoo(net),
        col = colorpal[6],
        lwd = linew,
        lty = 2)
  axis(1, at = seq(index(sw[1]), index(last(sw)), by = 24*3600),
       labels = c("15", "16", "17"))
  abline(h = 0, col = "darkgray", lwd = 1, lty = 1)
}
plot_daily_temps <- function(ref, low, mod, high, colorpal, ylimits){
  plot.zoo(ref,
           ylim = ylimits,
           ylab = expression(paste("Temperature (  ", degree, " C)")),
           xlab = "Day of Month",
           xaxt = "n",
           type = "n")
  abline(v = seq(index(ref[1]), index(last(ref)), by = 12*3600),
         col = "darkgray")
  lines(as.zoo(ref),
        col = colorpal[1],
        lwd = linew)
  lines(as.zoo(low),
        col = colorpal[2],
        lwd = linew)
  lines(as.zoo(mod),
        col = colorpal[3],
        lwd = linew)
  lines(as.zoo(high),
        col = colorpal[4],
        lwd = linew)
  axis(1, at = seq(index(ref[1]), index(last(ref)), by = 24*3600),
       labels = c("15", "16", "17"))
}




days <- c("2016-01-15/2016-01-17 00:00:00", "2016-04-15/2016-04-17 00:00:00",
          "2016-07-15/2016-07-17 00:00:00", "2016-10-15/2016-10-17 00:00:00")
fluxpal <- c(hcl.colors(5, "Plasma"), "black")
linew <- 3
ylmts <- c(-0.3,0.8)
temp_ylmts <- list(c(-6,6), c(6,18), c(15,27), c(6,18))


for(i in 1:4){
#### SHADE DAILY BUDGETS ####
png(paste0("plots/2017_umatilla/daily_heat_daily_shade/shade_heat_", i, ".png"),
    width = 500*5, height = 1500*5,
    res = 72*5)
par(mfrow = c(4,1),
    mar = c(2,2,1,1),
    oma = c(3,3,0,0),
    cex = 1.5,
    cex.lab = 1.3)
plot_daily_heat(shortwaveNetRef[days[i]],
                longwaveNetRef[days[i]],
                sensibleRef[days[i]],
                latentRef[days[i]],
                hypo = NA,
                refall[days[i]],
                fluxpal,
                ylmts)
plot_daily_heat(shortwaveNet30[days[i]],
                longwaveNet30[days[i]],
                sensible30[days[i]],
                latent30[days[i]],
                hypo = NA,
                shade30all[days[i]],
                fluxpal,
                ylmts)
plot_daily_heat(shortwaveNet60[days[i]],
                longwaveNet60[days[i]],
                sensible60[days[i]],
                latent60[days[i]],
                hypo = NA,
                shade60all[days[i]],
                fluxpal,
                ylmts)
plot_daily_heat(shortwaveNet90[days[i]],
                longwaveNet90[days[i]],
                sensible90[days[i]],
                latent90[days[i]],
                hypo = NA,
                shade90all[days[i]],
                fluxpal,
                ylmts)
mtext("Day of Month", 1, 2, outer = T, cex = 1.5)
mtext(expression(paste("Heat Flux (kJ  ", m^-2, " ",s^-1, ")")), 2, 2, outer = T, cex = 1.5)
dev.off()

#### SHADE AND HYPO TEMPERATURE ####
png(paste0("plots/2017_umatilla/daily_heat_daily_shade/both_temps_", i, ".png"),
    width = 900*5, height = 1600*5,
    res = 72*5)
par(mfrow = c(2,1),
    mar = c(2,2,1,1),
    oma = c(3,3,0,0),
    cex = 1.5,
    cex.lab = 1.3)
plot_daily_temps(reference[days[i]],
                 shade30[days[i]],
                 shade60[days[i]],
                 shade90[days[i]],
                 c("black", hcl.colors(6, "Vik", rev = T)),
                 temp_ylmts[[i]])
plot_daily_temps(reference[days[i]],
                 hypolit[days[i]],
                 hypomed[days[i]],
                 hypohigh[days[i]],
                 c("black", hcl.colors(6, "Vik")[1:3]),
                 temp_ylmts[[i]])
mtext("Day of Month", 1, 2, outer = T, cex = 1.5)
mtext(expression(paste("Temperature (", degree, " C)")), 2,2,outer = T, cex = 1.5)
dev.off()

#### HYPORHEIC DAILY BUDGETS ####
png(paste0("plots/2017_umatilla/daily_heat_daily_shade/hypo_heat_", i, ".png"),
    width = 500*5, height = 1500*5,
    res = 72*5)
par(mfrow = c(4,1),
    mar = c(2,2,1,1),
    oma = c(3,3,0,0),
    cex = 1.5,
    cex.lab = 1.3)
plot_daily_heat(shortwaveNetRef[days[i]],
                longwaveNetRef[days[i]],
                sensibleRef[days[i]],
                latentRef[days[i]],
                hypo = NA,
                refall[days[i]],
                fluxpal,
                ylmts)
plot_daily_heat(shortwaveLittleHypo[days[i]],
                longwaveLittleHypo[days[i]],
                sensibleLittleHypo[days[i]],
                latentLittleHypo[days[i]],
                hyporheicLittleHypo[days[i]],
                hypolittleall[days[i]],
                fluxpal,
                ylmts)
plot_daily_heat(shortwaveMedHypo[days[i]],
                longwaveMedHypo[days[i]],
                sensibleMedHypo[days[i]],
                latentMedHypo[days[i]],
                hyporheicMedHypo[days[i]],
                hypomedall[days[i]],
                fluxpal,
                ylmts)
plot_daily_heat(shortwaveHighHypo[days[i]],
                longwaveHighHypo[days[i]],
                sensibleHighHypo[days[i]],
                latentHighHypo[days[i]],
                hyporheicHighHypo[days[i]],
                hypohighall[days[i]],
                fluxpal,
                ylmts)
mtext("Day of Month", 1, 2, outer = T, cex = 1.5)
mtext(expression(paste("Heat Flux (kJ  ", m^-2, " ",s^-1, ")")), 2,2,outer = T, cex = 1.5)
dev.off()
}
