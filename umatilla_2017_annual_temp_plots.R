library(RODBC)
library(lubridate)
library(zoo)
library(xts)
library(plotrix)

load("runs_using_2017_umatilla_aquifer_geometry/highHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/medHypo870.RData")
load("runs_using_2017_umatilla_aquifer_geometry/littleHypo870.RData")
load("model_output/noShadeNoHypo.RData")
load("model_output/shade30.RData")
load("model_output/shade60.RData")
load("model_output/shade90.RData")

control <- xts(zoo(noShadeNoHypo$svValue, order.by = seq(mdy_hms("01-01-2014 00:00:00"), by = 3600, length.out = 35065)))
shade90 <- shade90["2014/2018-01-01 00:00:00"]
shade60 <- shade60["2014/2018-01-01 00:00:00"]
shade30 <- shade30["2014/2018-01-01 00:00:00"]
hypohigh <- highHypo870$cTemp$svValue
hypomed <- medHypo870$cTemp$svValue
hypolit <- littleHypo870$cTemp$svValue

yearandhalf <- "2016-01-01 00:00:00/2017-07-01 00:00:00"
yearandhalf.dm <- "2016-01-01/2017-07-01"

control_dm <- apply.daily(control, mean)
shade90_dm <- apply.daily(shade90, mean)
shade60_dm <- apply.daily(shade60, mean)
shade30_dm <- apply.daily(shade30, mean)
hypohigh_dm <- apply.daily(hypohigh, mean)
hypomed_dm <- apply.daily(hypomed, mean)
hypolit_dm <- apply.daily(hypolit, mean)


diagnostics <- data.frame(dayofmin = c(yday(control[control$x == min(control[yearandhalf])]),
                                       yday(shade30[shade30$x == min(shade30[yearandhalf])]),
                                       yday(shade60[shade60$x == min(shade60[yearandhalf])]),
                                       yday(shade90[shade90$x == min(shade90[yearandhalf])]),
                                       yday(hypolit[hypolit$svValue == min(hypolit[yearandhalf])]),
                                       yday(hypomed[hypomed$svValue == min(hypomed[yearandhalf])]),
                                       yday(hypohigh[hypohigh$svValue == min(hypohigh[yearandhalf])])),
                          dayofmax = c(yday(control[control$x == max(control[yearandhalf])]),
                                       yday(shade30[shade30$x == max(shade30[yearandhalf])]),
                                       yday(shade60[shade60$x == max(shade60[yearandhalf])]),
                                       yday(shade90[shade90$x == max(shade90[yearandhalf])]),
                                       yday(hypolit[hypolit$svValue == max(hypolit[yearandhalf])]),
                                       yday(hypomed[hypomed$svValue == max(hypomed[yearandhalf])]),
                                       yday(hypohigh[hypohigh$svValue == max(hypohigh[yearandhalf])])))


shade30gray = "gray37"
shade60gray = "gray60"
shade90gray = "gray85"

littlehypogray = "gray37"
medhypogray = "gray60"
highhypogray = "gray85"

density = 25

x <- layout(matrix(
  c(7, 1,3, 6,
    7, 2,4, 6,
    7, 5,5, 5),
  nrow = 3, ncol = 4, byrow = T),
  heights = c(1, 1, 0.2),
  widths = c(0.2, 1.4, 0.5, 0.5))

layout.show(x)

# Annual Sine & Vert Bars
linewidths = 6
verticallinewidths = 4
xlabelcex = 5

# Dot line
meanpointcex = 10
annualrangelwd = 8
labelcex = 2.3

phaselegendcex = 2.3
labelline = -5
phasepointcex = 6


###
###
# OLD LOCATION: "d:/Users/sarah.fogg/Dropbox/PAPER1/figs/shadeHE_DailyMeans_AnnualMeanAndRange_AnnualPhaseShift_3.png"
png("C:/Users/t24x137/Desktop/TempTool_2020/plots/2017_umatilla/AnnualTemperature.png",
    width = 2000*3,
    height = 1900*3,
    res = 72*3)
par(cex.axis= 3, cex.lab = 3, mar = c(4,10,2,4) ,  cex = 1.6, lend = 2, oma = c(0,0,0,0))

layout(matrix(
  c(6, 1, 1, 1,
    6, 2, 2, 2,
    6, 4, 4, 4,
    6, 3, 3, 5,
    6, 7, 7, 7),
  nrow = 5, ncol = 4, byrow = T),
  heights = c(1, 1, 0.2, 1, 0.2),
  widths = c(0.15, 0.3, 0.3, 0.5))

#### plot 1 ####
plot(coredata(apply.daily(control[yearandhalf.dm], mean)),
     type = "l",
     main = "",
     ylab = "",
     xaxt = "n",
     xlab = "",
     lwd = linewidths,
     ylim = c(min(control, hypohigh, hypolit, hypomed, shade90, shade30, shade60),
              max(control, hypohigh, hypolit, hypomed, shade90, shade30, shade60)))
SEQ1 <- seq(ymd("2016-01-03", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ1[i]) == "2016"){
    segments(x0 = yday(SEQ1[i]), y0 = min(control[gsub(" UTC", "", SEQ1[i])]),
             x1 = yday(SEQ1[i]), y1 = max(control[gsub(" UTC", "", SEQ1[i])]),
             lwd = verticallinewidths)}
  else{segments(x0 = yday(SEQ1[i])+365, y0 = min(control[gsub(" UTC", "", SEQ1[i])]),
                x1 = yday(SEQ1[i])+365, y1 = max(control[gsub(" UTC", "", SEQ1[i])]),
                lwd = verticallinewidths)}}


lines(coredata(apply.daily(shade30[yearandhalf.dm], mean)),
      col = shade30gray,
      lwd = linewidths)
SEQ2 <- seq(ymd("2016-01-08", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ2[i]) == "2016"){
    segments(x0 = yday(SEQ2[i]), y0 = min(shade30[gsub(" UTC", "", SEQ2[i])]),
             x1 = yday(SEQ2[i]), y1 = max(shade30[gsub(" UTC", "", SEQ2[i])]),
             lwd = verticallinewidths,
             col = shade30gray)}
  else{segments(x0 = yday(SEQ2[i])+365, y0 = min(shade30[gsub(" UTC", "", SEQ2[i])]),
                x1 = yday(SEQ2[i])+365, y1 = max(shade30[gsub(" UTC", "", SEQ2[i])]),
                lwd = verticallinewidths,
                col = shade30gray)}}


lines(coredata(apply.daily(shade60[yearandhalf.dm], mean)),
      col = shade60gray,
      lwd = linewidths)
SEQ3 <- seq(ymd("2016-01-13" , tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ1[i]) == "2016"){
    segments(x0 = yday(SEQ3[i]), y0 = min(shade60[gsub(" UTC", "", SEQ3[i])]),
             x1 = yday(SEQ3[i]), y1 = max(shade60[gsub(" UTC", "", SEQ3[i])]),
             lwd = verticallinewidths,
             col = shade60gray)}
  else{segments(x0 = yday(SEQ3[i])+365, y0 = min(shade60[gsub(" UTC", "", SEQ3[i])]),
                x1 = yday(SEQ3[i])+365, y1 = max(shade60[gsub(" UTC", "", SEQ3[i])]),
                lwd = verticallinewidths,
                col = shade60gray)}}


lines(coredata(apply.daily(shade90[yearandhalf.dm], mean)),
      col = shade90gray,
      lwd = linewidths)
SEQ4 <- seq(ymd("2016-01-18", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ4[i]) == "2016"){
    segments(x0 = yday(SEQ4[i]), y0 = min(shade90[gsub(" UTC", "", SEQ4[i])]),
             x1 = yday(SEQ4[i]), y1 = max(shade90[gsub(" UTC", "", SEQ4[i])]),
             lwd = verticallinewidths,
             col = shade90gray)}
  else{segments(x0 = yday(SEQ4[i])+365, y0 = min(shade90[gsub(" UTC", "", SEQ4[i])]),
                x1 = yday(SEQ4[i])+365, y1 = max(shade90[gsub(" UTC", "", SEQ4[i])]),
                lwd = verticallinewidths,
                col = shade90gray)}}

legend("topleft", c("Control", "Low Shade", "Moderate Shade", "High Shade"),
       col = c("black", shade30gray, shade60gray,  shade90gray),
       lwd = linewidths,
       cex = 3)
axis(side = 1,
     labels = c("", "", "", "", "", "", "", "", "", ""),
     at = c(yday(ymd("2016-01-01", "2016-03-01", "2016-05-01", "2016-07-01", "2016-09-01", "2016-11-01")),
            yday(ymd("2017-01-01", "2017-03-01", "2017-05-01", "2017-07-01"))+365))
mtext(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan", "Mar", "May", "Jul"),
      side = 1,
      line = 2.5,
      cex = 2.2,
      at = c(yday(ymd("2016-01-01", "2016-03-01", "2016-05-01", "2016-07-01", "2016-09-01", "2016-11-01")),
             yday(ymd("2017-01-01", "2017-03-01", "2017-05-01", "2017-07-01"))+365))
mtext(expression(paste("Temperature (", degree, "C)")), side = 2, line = 5, cex = 3)
#### plot 2 ####
plot(coredata(apply.daily(control[yearandhalf.dm], mean)),
     main = "",
     ylab = "",
     xaxt = "n",
     xlab = "",
     type = "l",
     lwd = linewidths,
     ylim = c(min(control, hypohigh, hypolit, hypomed, shade90, shade30, shade60),
              max(control, hypohigh, hypolit, hypomed, shade90, shade30, shade60)))
SEQ1 <- seq(ymd("2016-01-03", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ1[i]) == "2016"){
    segments(x0 = yday(SEQ1[i]), y0 = min(control[gsub(" UTC", "", SEQ1[i])]),
             x1 = yday(SEQ1[i]), y1 = max(control[gsub(" UTC", "", SEQ1[i])]),
             lwd = verticallinewidths)}
  else{segments(x0 = yday(SEQ1[i])+365, y0 = min(control[gsub(" UTC", "", SEQ1[i])]),
                x1 = yday(SEQ1[i])+365, y1 = max(control[gsub(" UTC", "", SEQ1[i])]),
                lwd = verticallinewidths)}}


lines(coredata(apply.daily(hypolit[yearandhalf.dm], mean)),
      col = littlehypogray,
      lwd = linewidths)
SEQ2 <- seq(ymd("2016-01-08", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ2[i]) == "2016"){
    segments(x0 = yday(SEQ2[i]), y0 = min(hypolit[gsub(" UTC", "", SEQ2[i])]),
             x1 = yday(SEQ2[i]), y1 = max(hypolit[gsub(" UTC", "", SEQ2[i])]),
             lwd = verticallinewidths,
             col = littlehypogray)}
  else{segments(x0 = yday(SEQ2[i])+365, y0 = min(hypolit[gsub(" UTC", "", SEQ2[i])]),
                x1 = yday(SEQ2[i])+365, y1 = max(hypolit[gsub(" UTC", "", SEQ2[i])]),
                lwd = verticallinewidths,
                col = littlehypogray)}}


lines(coredata(apply.daily(hypomed[yearandhalf.dm], mean)),
      col = medhypogray,
      lwd = linewidths)
SEQ3 <- seq(ymd("2016-01-13", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ3[i]) == "2016"){
    segments(x0 = yday(SEQ3[i]), y0 = min(hypomed[gsub(" UTC", "", SEQ3[i])]),
             x1 = yday(SEQ3[i]), y1 = max(hypomed[gsub(" UTC", "", SEQ3[i])]),
             lwd = verticallinewidths,
             col = medhypogray)}
  else{segments(x0 = yday(SEQ3[i])+365, y0 = min(hypomed[gsub(" UTC", "", SEQ3[i])]),
                x1 = yday(SEQ3[i])+365, y1 = max(hypomed[gsub(" UTC", "", SEQ3[i])]),
                lwd = verticallinewidths,
                col = medhypogray)}}


lines(coredata(apply.daily(hypohigh[yearandhalf.dm], mean)),
      col = highhypogray,
      lwd = linewidths)
SEQ4 <- seq(ymd("2016-01-18", tz = "UTC"), by = 86400*density, length.out = (365 + 182)/20 )
for(i in 1:22){
  if(year(SEQ4[i]) == "2016"){
    segments(x0 = yday(SEQ4[i]), y0 = min(hypohigh[gsub(" UTC", "", SEQ4[i])]),
             x1 = yday(SEQ4[i]), y1 = max(hypohigh[gsub(" UTC", "", SEQ4[i])]),
             lwd = verticallinewidths,
             col = highhypogray)}
  else{segments(x0 = yday(SEQ4[i])+365, y0 = min(hypohigh[gsub(" UTC", "", SEQ4[i])]),
                x1 = yday(SEQ4[i])+365, y1 = max(hypohigh[gsub(" UTC", "", SEQ4[i])]),
                lwd = verticallinewidths,
                col = highhypogray)}}


legend("topleft", c("Control", "Low HE", "Moderate HE", "High HE"),
       col = c("black", littlehypogray, medhypogray,  highhypogray),
       lwd = linewidths,
       cex = 3)
axis(side = 1,
     labels = c("", "", "", "", "", "", "", "", "", ""),
     at = c(yday(ymd("2016-01-01", "2016-03-01", "2016-05-01", "2016-07-01", "2016-09-01", "2016-11-01")),
            yday(ymd("2017-01-01", "2017-03-01", "2017-05-01", "2017-07-01"))+365))
mtext(c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan", "Mar", "May", "Jul"),
      side = 1,
      line = 2.5,
      cex = 2.2,
      at = c(yday(ymd("2016-01-01", "2016-03-01", "2016-05-01", "2016-07-01", "2016-09-01", "2016-11-01")),
             yday(ymd("2017-01-01", "2017-03-01", "2017-05-01", "2017-07-01"))+365))
mtext(expression(paste("Temperature (", degree, "C)")), side = 2, line = 5, cex = 3)
#### plot 3 ####
par(mar = c(10, 10, 0, 15))
plot(1, mean(shade30["2016"]),
     pch = "-",
     cex = meanpointcex,
     ylab = "",
     ylim = c(-3, 26),
     xlim = c(0.5,9.5),
     xlab = "",
     xaxt = "n",
     bty = "n",
     col = shade30gray)

segments(x0 = 1, y0 = max(shade30_dm["2016"]),
         x1 = 1, y1 = min(shade30_dm["2016"]),
         col = shade30gray,
         lwd = annualrangelwd)

points(2, mean(shade60["2016"]),
       pch = "-",
       cex = meanpointcex,
       col = shade60gray)
segments(x0 = 2, y0 = max(shade60_dm["2016"]),
         x1 = 2, y1 = min(shade60_dm["2016"]),
         col = shade60gray,
         lwd = annualrangelwd)
points(3, mean(shade90["2016"]),
       pch = "-",
       cex = meanpointcex,
       col = shade90gray)
segments(x0 = 3, y0 = max(shade90_dm["2016"]),
         x1 = 3, y1 = min(shade90_dm["2016"]),
         col = shade90gray,
         lwd = annualrangelwd)

mtext("Low Shade",
      side = 1,
      at = 1,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Moderate Shade",
      side = 1,
      at = 2,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("High Shade",
      side = 1,
      at = 3,
      line = -2,
      las = 3,
      cex = labelcex)

points(5, mean(control["2016"]),
       pch = "-",
       cex = meanpointcex,
       ylab = c(expression(paste("Temperature (", degree, "C)"))),
       ylim = c(-2, 26),
       xlim = c(0.5,4.5),
       xlab = "",
       xaxt = "n",
       bty = "n")
segments(x0 = 5, y0 = max(control_dm["2016"]),
         x1 = 5, y1 = min(control_dm["2016"]),
         lwd = annualrangelwd)

points(7, mean(hypolit["2016"]),
       pch = "-",
       cex = meanpointcex,
       col = littlehypogray)
segments(x0 = 7, y0 = max(hypolit_dm["2016"]),
         x1 = 7, y1 = min(hypolit_dm["2016"]),
         col = littlehypogray,
         lwd = annualrangelwd)
points(8, mean(hypomed["2016"]),
       pch = "-",
       cex = meanpointcex,
       col = medhypogray)
segments(x0 = 8, y0 = max(hypomed_dm["2016"]),
         x1 = 8, y1 = min(hypomed_dm["2016"]),
         col = medhypogray,
         lwd = annualrangelwd)
points(9, mean(hypohigh["2016"]),
       pch = "-",
       cex = meanpointcex,
       col = highhypogray)
segments(x0 = 9, y0 = max(hypohigh_dm["2016"]),
         x1 = 9, y1 = min(hypohigh_dm["2016"]),
         col = highhypogray,
         lwd = annualrangelwd)
mtext("Control",
      side = 1,
      at = 5,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Low HE",
      side = 1,
      at = 7,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Moderate HE",
      side = 1,
      at = 8,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("High HE",
      side = 1,
      at = 9,
      line = -2,
      las = 3,
      cex = labelcex)
mtext(expression(paste("Temperature (", degree, "C)")), side = 2, line = 5, cex = 3)

# text(x = 9.5, y = mean(hypohigh[yearandhalf]), "}", cex = 30)


#### plot 4 ####
par(mar = c(0,0,0,0))
plot(x = 1:20, y = rep(1, times = 20), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", pch = "")
text(x = 11, y = 1.1, "Month", cex = xlabelcex)

#### plot 5 ####
par(mar= c(10, 18, 0, 5))
plot(x = 1:9,
     y = c(diagnostics$dayofmin[2:4], NA, diagnostics$dayofmin[1], NA,diagnostics$dayofmin[5:7]),
     pch = 25,
     cex = phasepointcex,
     bg = c(shade30gray, shade60gray, shade90gray, NA, "black", NA, littlehypogray, medhypogray, highhypogray),
     xaxt = "n",
     yaxt = "n",
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(0.5,9.5),
     ylim = c(1, 50))
points(x = 1:9,
       y = c(diagnostics$dayofmax[2:4], NA, diagnostics$dayofmax[1], NA,diagnostics$dayofmax[5:7]) - 160,
       pch = 24,
       cex = phasepointcex,
       bg = c(shade30gray, shade60gray, shade90gray, NA, "black", NA, littlehypogray, medhypogray, highhypogray))
abline(h = diagnostics$dayofmin[1], lty = 2, lwd = 2, col = "gray")
abline(h = diagnostics$dayofmax[1] - 160, lty = 2, lwd = 2, col = "gray")
points(c(5,5), c(diagnostics$dayofmax[1] - 160, diagnostics$dayofmin[1]),
       pch = c(24, 25),
       bg = "black",
       cex = phasepointcex)
mtext("Low Shade",
      side = 1,
      at = 1,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Moderate Shade",
      side = 1,
      at = 2,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("High Shade",
      side = 1,
      at = 3,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Control",
      side = 1,
      at = 5,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Low HE",
      side = 1,
      at = 7,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Moderate HE",
      side = 1,
      at = 8,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("High HE",
      side = 1,
      at = 9,
      line = -2,
      las = 3,
      cex = labelcex)
mtext("Day of Extremums", side = 2, line = 6, cex = 3)
mtext("D", cex = 7, side = 2, line = 12, at = 45, las = 2)
axis(2, at = c(0, 7, 14, 21, 35-2, 42-2, 49-2), labels = c(0, 7, 14, 21, 35+160-2, 42+160-2, 49+160-2))
axis.break(2, breakpos = 26, brw = 0.03)

#### plot 6 ####
par(mar = c(0,0,0,0))
plot(x = 1:20, y = 1:20, bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", pch = "")
text(x = 18, y = 20, "A", cex = 10)
text(x = 18, y = 14, "B", cex = 10)
text(x = 18, y = 7, "C", cex = 10)

# expression(paste("Temperature (", degree, "C )"))
#### plot 7 ####
plot.new()

dev.off()

