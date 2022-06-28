#### Newest Daily heat plots ####

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


plot_daily_heat <- function(dummy, sw, lw, sens, latent, hypo = NA, net, fluxcols, ylimits, linew = 5){
  plot.zoo(dummy,
           ylim = ylimits,
           ylab = expression(paste("Heat Flux (kJ  ", m^-2, " ",s^-1, ")")),
           xlab = "Day of Month",
           xaxt = "n",
           yaxt = "n",
           type = "n")
  # polygon(c(index(sw[1])-6420, index(last(sw))+6420, index(last(sw))+6420, index(sw[1])-6420), c(0,0,-0.4,-0.4),
  #         border = NA,
  #         col = "white")

  abline(v = seq(index(dummy[1]), index(last(dummy)), by = 12*3600), col = "darkgrey")
  abline(h = 0, lty = 2, lwd = 2)

  lines(as.zoo(sw),
        col = fluxcols[1],
        lwd = linew)
  lines(as.zoo(lw),
        col = fluxcols[2],
        lwd = linew)
  lines(as.zoo(sens),
        col = fluxcols[3],
        lwd = linew)
  lines(as.zoo(latent),
        col = fluxcols[4],
        lwd = linew)
  if(anyNA(hypo) == F){
    lines(as.zoo(hypo),
          col = "black",
          lwd = linew+2
    )
    lines(as.zoo(hypo),
          col = fluxcols[5],
          lwd = linew)
  }
  # lines(as.zoo(net),
  #       col = fluxcols[6],
  #       lwd = linew+2,
  #       lty = 1)
  axis(1, at = c(index(dummy[1])-7200, seq(index(dummy[1]), index(last(dummy)), by = 24*3600), index(last(dummy))+7200),
       labels = c("", "15", "16", "17", ""),
       cex = 1.2)
  axis(2, at = c(-0.3, 0.0, 0.3, 0.6), cex = 1.2)
  segments(index(dummy[1])-7140, 0, index(dummy[1])-7140, -0.4)
  segments(index(last(dummy))+6520, 0, index(last(dummy))+6520, -0.4)
  abline(h = 0, lty = 2, lwd = 2)
  # abline(h = 0, col = "darkgray", lwd = 3, lty = 1)
}


plotregionindex <- c("2016-01-15/2016-01-17 00:00:00", "2016-04-15/2016-04-17 00:00:00",
                     "2016-07-15/2016-07-17 00:00:00", "2016-10-15/2016-10-17 00:00:00")
days <- c("2016-01-14/2016-01-18 00:00:00", "2016-04-14/2016-04-18 00:00:00",
          "2016-07-14/2016-07-18 00:00:00", "2016-10-14/2016-10-18 00:00:00")
hypocol <- "#DAFF47"#"#BFED30" #"#F1E814" "#EDF83B"
fluxpal <- c(hcl.colors(5, "Plasma")[1:4], hypocol,"black")
lw <- 5
ylmts <- c(-0.35,0.8)
temp_ylmts <- list(c(-6,6), c(6,18), c(15,27), c(6,18))
shadecols <- hcl.colors(6, "Vik", rev = T)[1:3]
hypocols <- hcl.colors(6, "Vik")[1:3]



# for(i in 1:4){
#   #### SHADE DAILY BUDGETS ####
#   png(paste0("plots/2017_umatilla/daily_heat_daily_shade/shade_heat_", i, "_v3.png"),
#       width = 475*5, height = 1800*5,
#       res = 72*5)
#   par(mfrow = c(4,1),
#       mar = c(2,3,1,1),
#       oma = c(0,0,0,0),
#       cex = 1.5,
#       cex.lab = 1.3,
#       cex.axis = 1.5)
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveNetRef[days[i]],
#                   longwaveNetRef[days[i]],
#                   sensibleRef[days[i]],
#                   latentRef[days[i]],
#                   hypo = NA,
#                   refall[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveNet30[days[i]],
#                   longwaveNet30[days[i]],
#                   sensible30[days[i]],
#                   latent30[days[i]],
#                   hypo = NA,
#                   shade30all[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveNet60[days[i]],
#                   longwaveNet60[days[i]],
#                   sensible60[days[i]],
#                   latent60[days[i]],
#                   hypo = NA,
#                   shade60all[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveNet90[days[i]],
#                   longwaveNet90[days[i]],
#                   sensible90[days[i]],
#                   latent90[days[i]],
#                   hypo = NA,
#                   shade90all[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   # mtext("Day of Month", 1, 2, outer = T, cex = 1.5)
#   # mtext(expression(paste("Heat Flux (kJ  ", m^-2, " ",s^-1, ")")), 2, 2, outer = T, cex = 1.5)
#   dev.off()
# }
#
#
#
# for(i in 1:4){
#   #### HYPORHEIC DAILY BUDGETS ####
#   png(paste0("plots/2017_umatilla/daily_heat_daily_shade/hypo_heat_", i, "_v3.png"),
#       width = 475*5, height = 1800*5,
#       res = 72*5)
#   par(mfrow = c(4,1),
#       mar = c(2,3,1,1),
#       oma = c(0,0,0,0),
#       cex = 1.5,
#       cex.lab = 1.3,
#       cex.axis = 1.5)
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveNetRef[days[i]],
#                   longwaveNetRef[days[i]],
#                   sensibleRef[days[i]],
#                   latentRef[days[i]],
#                   hypo = NA,
#                   refall[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveLittleHypo[days[i]],
#                   longwaveLittleHypo[days[i]],
#                   sensibleLittleHypo[days[i]],
#                   latentLittleHypo[days[i]],
#                   hyporheicLittleHypo[days[i]],
#                   hypolittleall[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveMedHypo[days[i]],
#                   longwaveMedHypo[days[i]],
#                   sensibleMedHypo[days[i]],
#                   latentMedHypo[days[i]],
#                   hyporheicMedHypo[days[i]],
#                   hypomedall[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
#                   shortwaveHighHypo[days[i]],
#                   longwaveHighHypo[days[i]],
#                   sensibleHighHypo[days[i]],
#                   latentHighHypo[days[i]],
#                   hyporheicHighHypo[days[i]],
#                   hypohighall[days[i]],
#                   fluxpal,
#                   ylmts,
#                   linew = 7)
#
#   # mtext("Day of Month", 1, 2, outer = T, cex = 1.5)
#   # mtext(expression(paste("Heat Flux (kJ  ", m^-2, " ",s^-1, ")")), 2,2,outer = T, cex = 1.5)
#   dev.off()
# }

#### ALL TOGETHER ON 1 PLOT ####
#### SHADE ####
png(paste0("plots/2017_umatilla/updates/shade_heat_v4_update.png"),
    width = 1800*5, height = 1800*5,
    res = 72*5)
par(mfcol = c(4,4),
    mar = c(2,3,1,1),
    oma = c(0,0,0,0),
    cex = 1.5,
    cex.lab = 1.3,
    cex.axis = 1.6)
for (i in 1:4){
plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveNetRef[days[i]],
                longwaveNetRef[days[i]],
                sensibleRef[days[i]],
                latentRef[days[i]],
                hypo = NA,
                refall[days[i]],
                fluxpal,
                ylmts,
                linew = 7)

plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveNet30[days[i]],
                longwaveNet30[days[i]],
                sensible30[days[i]],
                latent30[days[i]],
                hypo = NA,
                shade30all[days[i]],
                fluxpal,
                ylmts,
                linew = 7)

plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveNet60[days[i]],
                longwaveNet60[days[i]],
                sensible60[days[i]],
                latent60[days[i]],
                hypo = NA,
                shade60all[days[i]],
                fluxpal,
                ylmts,
                linew = 7)

plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveNet90[days[i]],
                longwaveNet90[days[i]],
                sensible90[days[i]],
                latent90[days[i]],
                hypo = NA,
                shade90all[days[i]],
                fluxpal,
                ylmts,
                linew = 7)
}
dev.off()

#### HYPORHEIC ####

png(paste0("plots/2017_umatilla/updates/hypo_heat_v4_update.png"),
    width = 1800*5, height = 1800*5,
    res = 72*5)
par(mfcol = c(4,4),
    mar = c(2,3,1,1),
    oma = c(0,0,0,0),
    cex = 1.5,
    cex.lab = 1.3,
    cex.axis = 1.6)
for (i in 1:4){
plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveNetRef[days[i]],
                longwaveNetRef[days[i]],
                sensibleRef[days[i]],
                latentRef[days[i]],
                hypo = NA,
                refall[days[i]],
                fluxpal,
                ylmts,
                linew = 7)

plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveLittleHypo[days[i]],
                longwaveLittleHypo[days[i]],
                sensibleLittleHypo[days[i]],
                latentLittleHypo[days[i]],
                hyporheicLittleHypo[days[i]],
                hypolittleall[days[i]],
                fluxpal,
                ylmts,
                linew = 7)

plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveMedHypo[days[i]],
                longwaveMedHypo[days[i]],
                sensibleMedHypo[days[i]],
                latentMedHypo[days[i]],
                hyporheicMedHypo[days[i]],
                hypomedall[days[i]],
                fluxpal,
                ylmts,
                linew = 7)

plot_daily_heat(shortwaveNetRef[plotregionindex[i]],
                shortwaveHighHypo[days[i]],
                longwaveHighHypo[days[i]],
                sensibleHighHypo[days[i]],
                latentHighHypo[days[i]],
                hyporheicHighHypo[days[i]],
                hypohighall[days[i]],
                fluxpal,
                ylmts,
                linew = 7)
}
dev.off()





######################









###Legend:
par(mfrow = c(1,1),
    lend = 1)
plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)

legend(x = 5,  y = 7.7,
         c(expression(paste(Q[s])),
           expression(paste(Q[l])),
           expression(paste(Q[h])),
           expression(paste(Q[e])),
           expression(paste(Q[b]))),
       lty = 1,
       col = fluxpal[1:5],
       lwd = 7,
       cex = 1.8,
       ncol = 1,
       title = "Legend",
       bty = "n")


####






#### UPDATE DAILY TEMP PLOTS ####
load("runs_using_2017_umatilla_aquifer_geometry/high/highHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/med/medHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/little/littleHypo870.RData")
load("model_output/noShadeNoHypo.RData")
load("model_output/shade30.RData")
load("model_output/shade60.RData")
load("model_output/shade90.RData")
control <- xts(zoo(noShadeNoHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))

hypolit <- littleHypo870$cTemp$svValue
hypomed <- medHypo870$cTemp$svValue
hypohigh <- highHypo870$cTemp$svValue

shade30gray = hcl.colors(6, "Vik")[6]
shade60gray = hcl.colors(6, "Vik")[5]
shade90gray = hcl.colors(6, "Vik")[4]

littlehypogray = hcl.colors(6, "Vik")[1]
medhypogray = hcl.colors(6, "Vik")[2]
highhypogray = hcl.colors(6, "Vik")[3]


plotdays <- c("2016-01-15/2016-01-16", "2016-04-15/2016-04-16", "2016-07-15/2016-07-16", "2016-10-15/2016-10-16")


linewidths <- 5
reflty <- 1
labelcex = 1.5

png("plots/2017_umatilla/DailyTemperature_color_shade.png",
    height = 450*3.5, width = 1800*3.5, res = 72*5)
par(mfrow = c(1,5),
    cex.axis = 2,
    cex.lab = 2.3,
    cex.main = 2.7,
    lend = 1,
    mar = c(2,2,3,1),
    oma = c(3,3,0,0))
for(i in 1:4){
  thisplotrange <- c(mean(c(coredata(control[plotdays[i]]), coredata(shade30[plotdays[i]]), coredata(shade60[plotdays[i]]), coredata(shade90[plotdays[i]]), coredata(hypohigh[plotdays[i]]), coredata(hypomed[plotdays[i]]), coredata(hypolit[plotdays[i]]))) - (fixedrange + 0.4)/2,
                     mean(c(coredata(control[plotdays[i]]), coredata(shade30[plotdays[i]]), coredata(shade60[plotdays[i]]), coredata(shade90[plotdays[i]]), coredata(hypohigh[plotdays[i]]), coredata(hypomed[plotdays[i]]), coredata(hypolit[plotdays[i]]))) + (fixedrange + 0.4)/2)
  par(mar = c(1, c(2, 2, 2, 2)[i], 4, 1))
  plot(coredata(control[plotdays[i]]),
       col = "black",
       lwd = linewidths,
       xaxt = "n",
       xlab = "",
       type = "l",
       ylim = thisplotrange,
       ylab = c(expression(paste("Temperature ( ", degree, "C )")), "", "", "")[i],
       main = "",
       yaxt = "n",
       lty = reflty)
  abline(v = c(1, 12, 24, 36, 48), col = "darkgray")

  lines(coredata(control[plotdays[i]]),
        lwd = linewidths,
        col = "black",
        lty = reflty)
  lines(coredata(shade30[plotdays[i]]),
        lwd = linewidths,
        col = shade30gray)
  lines(coredata(shade60[plotdays[i]]),
        lwd = linewidths,
        col = shade60gray)
  lines(coredata(shade90[plotdays[i]]),
        lwd = linewidths,
        col = shade90gray)

  axis(side = 2, at = seq(-6, 26, by = 2), labels = c("-6", "", "-2", "", "2", "", "6", "", "10", "", "14", "", "18", "", "22", "", "26"))
  axis(1, at = c(1, 24, 48, 72), labels = c(15, 16, 17, 18))
  mtext(c("January", "April", "July", "October")[i], side = 3, line = 1, cex = 2, font = 2)
}


plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)

legend(x = 0.1,  y = 7.7,
       c("Control","Low Shade", "Moderate Shade", "High Shade"),
       lty = c(reflty,1,1,1),
       col = c("black", shade30gray, shade60gray, shade90gray),
       lwd = linewidths,
       cex = 2.3,
       ncol = 1,
       title = "Legend",
       bty = "n")
dev.off()

## Hypo

png("plots/2017_umatilla/DailyTemperature_color_hypo.png",
    height = 450*3.5, width = 1800*3.5, res = 72*5)
par(mfrow = c(1,5),
    cex.axis = 2,
    cex.lab = 2.3,
    cex.main = 2.7,
    lend = 1,
    mar = c(2,2,3,1),
    oma = c(3,3,0,0))
for(i in 1:4){
  thisplotrange <- c(mean(c(coredata(control[plotdays[i]]), coredata(shade30[plotdays[i]]), coredata(shade60[plotdays[i]]), coredata(shade90[plotdays[i]]), coredata(hypohigh[plotdays[i]]), coredata(hypomed[plotdays[i]]), coredata(hypolit[plotdays[i]]))) - (fixedrange + 0.4)/2,
                     mean(c(coredata(control[plotdays[i]]), coredata(shade30[plotdays[i]]), coredata(shade60[plotdays[i]]), coredata(shade90[plotdays[i]]), coredata(hypohigh[plotdays[i]]), coredata(hypomed[plotdays[i]]), coredata(hypolit[plotdays[i]]))) + (fixedrange + 0.4)/2)
  par(mar = c(1, c(2, 2, 2, 2)[i], 6, 1))
  plot(coredata(control[plotdays[i]]),
       lwd = linewidths,
       xaxt = "n",
       xlab = "",
       type = "l",
       ylim = thisplotrange,
       ylab = "",
       lty = reflty,
       yaxt = "n")
  abline(v = c(1, 12, 24, 36, 48), col = "darkgray")

  lines(coredata(control[plotdays[i]]),
        lwd = linewidths,
        col = "black",
        lty = reflty)
  lines(coredata(hypolit[plotdays[i]]),
        lwd = linewidths,
        col = littlehypogray)
  lines(coredata(hypomed[plotdays[i]]),
        lwd = linewidths,
        col = medhypogray)
  lines(coredata(hypohigh[plotdays[i]]),
        lwd = linewidths,
        col = highhypogray)

  axis(side = 2, at = seq(-6, 26, by = 2), labels = c("-6", "", "-2", "", "2", "", "6", "", "10", "", "14", "", "18", "", "22", "", "26"))
  axis(1, at = c(1, 24, 48, 72), labels = c(15, 16, 17, 18))
  mtext(c("January", "April", "July", "October")[i], side = 3, line = 1, cex = 2, font = 2)
}

plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)

legend(x = 0.1, y = 7.7,
       c("Control","Low HE", "Moderate HE", "High HE"),
       lty = c(reflty,1,1,1),
       col = c("black", littlehypogray, medhypogray, highhypogray),
       lwd = linewidths,
       cex = 2.3,
       ncol = 1,
       title = "Legend",
       bty = "n")
dev.off()
