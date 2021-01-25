###
### 2017 Umatilla, 1/3, 2/3
###

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


rangecalc <- function(x){
  (max(x) - min(x))
}
ranges <- data.frame(january = numeric(2), april = numeric(2), july = numeric(2), october = numeric(2))
row.names(ranges) <- c("shade", "he")
for(i in 1:4){
  ranges[1,i] <- rangecalc(x = c(control[plotdays[i]], shade30[plotdays[i]], shade60[plotdays[i]], shade90[plotdays[i]]))
  ranges[2,i] <- rangecalc(x = c(control[plotdays[i]],
                                 hypolit[plotdays[i]],
                                 hypomed[plotdays[i]],
                                 hypohigh[plotdays[i]]))
}
fixedrange <- signif(max(ranges),2) +1.6

maxtiming <- data.frame(jan = numeric(7), april = numeric(7), july = numeric(7), oct = numeric(7))
for(i in 1:4){
  refmaxtiming <- hour(subset(control[plotdays], control[plotdays] == max(control[plotdays[i]])))
  trtmaxtiming <- c(hour(subset(shade30[plotdays], shade30[plotdays] == max(shade30[plotdays[i]]))),
                    hour(subset(shade60[plotdays], shade60[plotdays] == max(shade60[plotdays[i]]))),
                    hour(subset(shade90[plotdays], shade90[plotdays] == max(shade90[plotdays[i]]))),
                    hour(subset(hypolit[plotdays], hypolit[plotdays] == max(hypolit[plotdays[i]]))),
                    hour(subset(hypomed[plotdays], hypomed[plotdays] == max(hypomed[plotdays[i]]))),
                    hour(subset(hypohigh[plotdays], hypohigh[plotdays] == max(hypohigh[plotdays[i]]))))
  maxdiff <- trtmaxtiming - refmaxtiming

  maxtiming[,i] <- c(refmaxtiming, trtmaxtiming)
}

mintiming <- data.frame(jan = numeric(7), april = numeric(7), july = numeric(7), oct = numeric(7))
for(i in 1:4){
  refmintiming <- hour(subset(control[plotdays], control[plotdays] == min(control[plotdays[i]])))
  trtmintiming <- c(hour(subset(shade30[plotdays], shade30[plotdays] == min(shade30[plotdays[i]]))),
                    hour(subset(shade60[plotdays], shade60[plotdays] == min(shade60[plotdays[i]]))),
                    hour(subset(shade90[plotdays], shade90[plotdays] == min(shade90[plotdays[i]]))),
                    hour(subset(hypolit[plotdays], hypolit[plotdays] == min(hypolit[plotdays[i]]))),
                    hour(subset(hypomed[plotdays], hypomed[plotdays] == min(hypomed[plotdays[i]]))),
                    hour(subset(hypohigh[plotdays], hypohigh[plotdays] == min(hypohigh[plotdays[i]]))))

  mindiff <- trtmintiming - refmintiming
  mintiming[,i] <- c(refmintiming, trtmintiming)
}

linewidths <- 4
reflty <- 1

shadepch = 23
hypopch = 22
pointcex = 4

meancex = 6
rangelwd = 4
labelcex = 1.5

phasepntcex = 3
phaselwd = 2
x <- layout(matrix(
  c(10, 1,2,3,4, 5,
    10, 6,7,8,9, 5,
    10, 11, 11, 11, 11, 5,
    12, 13, 14, 15, 16, 5,
    12, 17, 18, 19, 20, 5,
    12, 21, 22, 23, 24, 5),
  nrow = 6, ncol = 6, byrow = T),
  heights = c(1, 1, 0.2, 1, 0.6, 0.6),
  widths = c(0.3, 1, 1, 1, 1, 0.75))
layout.show(x)
######
######
# OLD LOCATION: "d:/Users/sarah.fogg/Dropbox/PAPER1/figs/Temperature_Daily_ShadeAndHE_meanrangephase_3.png"
png("C:/Users/t24x137/Desktop/TempTool_2020/plots/2017_umatilla/DailyTemperature.png",
    height = 1700*3.5, width = 1850*3.5, res = 72*5)
par(mfcol = c(2,4), cex.axis = 2, cex.lab = 2.3, cex.main = 2.7, lend = 1)
layout(matrix(
  c(10, 1,2,3,4, 5,
    10, 6,7,8,9, 5,
    10, 11, 11, 11, 11, 5,
    12, 13, 14, 15, 16, 5,
    12, 17, 18, 19, 20, 5,
    12, 21, 22, 23, 24, 5),
  nrow = 6, ncol = 6, byrow = T),
  heights = c(1, 1, 0.2, 1, 1, 0.5),
  widths = c(0.3, 1, 1, 1, 1, 0.75))

#### plots 1-4 ####
par(oma = c(0,0,0,0))
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
        col = lowcol)
  lines(coredata(shade60[plotdays[i]]),
        lwd = linewidths,
        col = medcol)
  lines(coredata(shade90[plotdays[i]]),
        lwd = linewidths,
        col = highcol)

  axis(side = 2, at = seq(-6, 26, by = 2), labels = c("-6", "", "-2", "", "2", "", "6", "", "10", "", "14", "", "18", "", "22", "", "26"))
  axis(1, at = c(1,24, 48, 72), labels = c(15, 16, 17, 18))
  mtext(c("January", "April", "July", "October")[i], side = 3, line = 1, cex = 2, font = 2)
}


#### plot 5 (top legends) ####
par(mar = c(0,0,0,0))
plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)

legend(x = 0.1, y = 9.8,
       c("Control","Low Shade", "Moderate Shade", "High Shade"),
       lty = c(reflty,1,1,1),
       col = c("black", lowcol, medcol, highcol),
       lwd = linewidths,
       cex = 2.3,
       ncol = 1,
       title = "Legend",
       bty = "n")

legend(x = 0.1, y = 7.7,
       c("Control","Low HE", "Moderate HE", "High HE"),
       lty = c(reflty,1,1,1),
       col = c("black", lowcol, medcol, highcol),
       lwd = linewidths,
       cex = 2.3,
       ncol = 1,
       title = "Legend",
       bty = "n")
# # Points legend
# text(x = 0, y = 4, labels = "Light
#      Attenuation", cex = 2.3, pos = 2)
# points(x = 0.1, y = 3.8, bg = lowcol, pch = shadepch, cex = pointcex)


#### plots 6-9 ####
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
        col = lowcol)
  lines(coredata(hypomed[plotdays[i]]),
        lwd = linewidths,
        col = medcol)
  lines(coredata(hypohigh[plotdays[i]]),
        lwd = linewidths,
        col = highcol)

  axis(side = 2, at = seq(-6, 26, by = 2), labels = c("-6", "", "-2", "", "2", "", "6", "", "10", "", "14", "", "18", "", "22", "", "26"))
  axis(1, at = c(1, 24, 48, 72), labels = c(15, 16, 17, 18))
  mtext(c("January", "April", "July", "October")[i], side = 3, line = 1, cex = 2, font = 2)
}

#### plot 10 ####
# plot.new()

#### plot 10 (upper y-label, A, B) ####

par(mar=c(0.5, 0.5, 1, 0.5))
plot(x = 1:20, y = 1:20, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 10, y = 20, "A", cex= 7)
text(x = 13, y = 15.75, expression(paste("Temperature (", degree, "C)")), cex = 2.5, srt = 90)

text(x = 10, y = 9.5, "B", cex= 7)
text(x = 13, y = 5.25, expression(paste("Temperature (", degree, "C)")), cex = 2.5, srt = 90)

#### plot 11 (upper x-label) ####
par(mar=c(0, 0, 0, 0))
plot(x = 1:10, y = 1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 5.5, y = 4, "Day of Month", cex = 2.8)

#### plot 12 (lower y-label) ####
plot(x = 1:20, y = 1:20, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", frame.plot = F)
text(x = 13, y = 15.5, expression(paste("Temperature (", degree, "C)")), cex = 2.5, srt = 90)
text(x = 10, y = 20, "C", cex= 7)

text(x = 13, y = 7.5, "Hour of Extremums", cex = 2.5, srt = 90)
text(x = 10, y = 11.35, "D", cex= 7)

#### plots 13-16 ####
par(mar = c(0.2, 2, 4, 1))
for(i in 1:4){
  thisplotrange <- c(mean(c(coredata(control[plotdays[i]]), coredata(shade30[plotdays[i]]), coredata(shade60[plotdays[i]]), coredata(shade90[plotdays[i]]), coredata(hypohigh[plotdays[i]]), coredata(hypomed[plotdays[i]]), coredata(hypolit[plotdays[i]]))) - (fixedrange + 0.4)/2,
                     mean(c(coredata(control[plotdays[i]]), coredata(shade30[plotdays[i]]), coredata(shade60[plotdays[i]]), coredata(shade90[plotdays[i]]), coredata(hypohigh[plotdays[i]]), coredata(hypomed[plotdays[i]]), coredata(hypolit[plotdays[i]]))) + (fixedrange + 0.4)/2)

  dailymeans <- c(mean(control[plotdays[i]]),
                  mean(shade30[plotdays[i]]),
                  mean(shade60[plotdays[i]]),
                  mean(shade90[plotdays[i]]),
                  mean(hypolit[plotdays[i]]),
                  mean(hypomed[plotdays[i]]),
                  mean(hypohigh[plotdays[i]]))

  plot(1:9, c(dailymeans[2:4], NA, dailymeans[1], NA, dailymeans[5:7]),
       cex = meancex,
       pch = "-",
       col = c(lowcol, medcol, highcol, "white", "black", "white",lowcol, medcol, highcol),
       main = "",
       bty = "n",
       xaxt = "n",
       xlab = "",
       ylab = "",
       lwd = 2,
       ylim = thisplotrange,
       xlim = c(0.5, 9.5))
  segments(x0 = 1, y0 = max(shade30[plotdays[i]]),
           x1 = 1, y1 = min(shade30[plotdays[i]]), col = lowcol, lwd = rangelwd)
  segments(x0 = 2, y0 = max(shade60[plotdays[i]]),
           x1 = 2, y1 = min(shade60[plotdays[i]]), col = medcol, lwd = rangelwd)
  segments(x0 = 3, y0 = max(shade90[plotdays[i]]),
           x1 = 3, y1 = min(shade90[plotdays[i]]), col = highcol, lwd = rangelwd)

  segments(x0 = 5, y0 = max(control[plotdays[i]]),
           x1 = 5, y1 = min(control[plotdays[i]]), col = "black", lwd = rangelwd)

  segments(x0 = 7, y0 = max(hypolit[plotdays[i]]),
           x1 = 7, y1 = min(hypolit[plotdays[i]]), col = lowcol, lwd = rangelwd)
  segments(x0 = 8, y0 = max(hypomed[plotdays[i]]),
           x1 = 8, y1 = min(hypomed[plotdays[i]]), col = medcol, lwd = rangelwd)
  segments(x0 = 9, y0 = max(hypohigh[plotdays[i]]),
           x1 = 9, y1 = min(hypohigh[plotdays[i]]), col = highcol, lwd = rangelwd)

  mtext(c("January", "April", "July", "October")[i], side = 3, line = 1, cex = 2, font = 2)
}

#### plots 17-20 ####
par(mar = c(1, 2, 1, 1))
for(i in 1:4){
  plot(x = 1:9, y = c(maxtiming[2:4,i], NA, maxtiming[1,i], NA, maxtiming[5:7, i]),
       ylim = c(0,24),
       xlim = c(0.5, 9.5),
       pch = 24,
       cex = phasepntcex,
       col = "black",
       bg = c(lowcol, medcol, highcol, NA,"black", NA,lowcol, medcol, highcol),
       lwd = phaselwd,
       ylab = "",
       xaxt = "n",
       xlab = "", bty = "n")
  abline(h = maxtiming[1,i], col = "gray", lty = 2, lwd = 2)
  abline(h = mintiming[1,i], col = "gray", lty = 2, lwd = 2)
  points(x = 1:9, y = c(maxtiming[2:4,i], NA, maxtiming[1,i], NA, maxtiming[5:7, i]),
         ylim = c(mean(maxtiming[,i]) - 3, mean(maxtiming[,i]) + 3),
         xlim = c(0.5, 9.5),
         pch = 24,
         cex = phasepntcex,
         col = "black",
         bg = c(lowcol, medcol, highcol, NA,"black", NA,lowcol, medcol, highcol),
         lwd = phaselwd)
  points(x = 1:9, y = c(mintiming[2:4,i], NA, mintiming[1,i], NA, mintiming[5:7, i]),
         ylim = c(mean(mintiming[,i]) - 3, mean(mintiming[,i]) + 3),
         xlim = c(0.5, 9.5),
         pch = 25,
         cex = phasepntcex,
         col = "black",
         bg = c(lowcol, medcol, highcol, NA,"black", NA,lowcol, medcol, highcol),
         lwd = phaselwd)


  # mtext(c("January", "April", "July", "October")[i], side = 3, line = 1, cex = 2, font = 2)

  mtext("Low Shade",
        side = 1,
        at = 1,
        line = -1,
        las = 3,
        cex = labelcex)
  mtext("Moderate Shade",
        side = 1,
        at = 2,
        line = -1,
        las = 3,
        cex = labelcex)
  mtext("High Shade",
        side = 1,
        at = 3,
        line = -1,
        las = 3,
        cex = labelcex)

  mtext("Control",
        side = 1,
        at = 5,
        line = -1,
        las = 3,
        cex = labelcex)

  mtext("Low HE",
        side = 1,
        at = 7,
        line = -1,
        las = 3,
        cex = labelcex)
  mtext("Moderate HE",
        side = 1,
        at = 8,
        line = -1,
        las = 3,
        cex = labelcex)
  mtext("High HE",
        side = 1,
        at = 9,
        line = -1,
        las = 3,
        cex = labelcex)
}

#### plots 21-24 ####
plot.new()
plot.new()
plot.new()
plot.new()

#####

dev.off()





